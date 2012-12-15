{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- This module implements the SMTP protocol, but allows the caller to provide
-- custom I/O and error handling methods.
module Network.Mail.SMTP.Abstract (
    -- * The MonadSMTP type class
    MonadSMTP(..),
    SMTPError,

    -- * Types
    module Network.Mail.SMTP.Types,

    -- * Actions
    hello,
    quit,
    authenticate,
    sendRenderedMail,

    -- ** Convenience
    renderAndSend,

    -- * Raw command execution
    sendCommand,
    getResponse,

    -- ** Command execution helpers
    tryOnce,
    tryCommand,
) where

import Network.Mail.SMTP.Auth
import Network.Mail.SMTP.Types

import Control.Exception        (Exception)
import Control.Monad
import Control.Monad.IO.Class
import Data.Char                (isDigit)
import Data.Monoid
import Data.Typeable
import Network                  (HostName)
import Network.Mail.Mime

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T

------------------------------------------------------------------------
-- The MonadSMTP type class

-- | This type class itemizes the side effects needed by the
-- SMTP protocol implementation.
class Monad m => MonadSMTP m where
    -- | Send the given line, followed by CRLF.  Flush so a subsequent
    -- 'recvLine' can expect a response.
    sendLine :: ByteString -> m ()

    -- | Receive a line, with the trailing CRLF removed.
    recvLine :: m ByteString

    throwSMTPError :: SMTPError -> m a

-- | The internal representation is deliberately not exported.
-- If you want to test for specific errors, please submit an issue to
-- <https://github.com/jhickner/smtp-mail/issues> describing your use case.
data SMTPError = SMTPError
    { errMessage   :: !ErrorMessage
    , errReplyCode :: !(Maybe ReplyCode)
    }
    deriving Typeable

instance Show SMTPError where
    show SMTPError{..} =
        "SMTP error: " ++ smtpErrorMessage errMessage ++
        case errReplyCode of
            Nothing -> ""
            Just c  -> " (server returned " ++ show c ++ ")"

instance Exception SMTPError

data ErrorMessage
    = ErrConnect
    | ErrExecuteCommand !Command !ReplyCode
    | ErrAcceptData
    | ErrAuth

smtpErrorMessage :: ErrorMessage -> String
smtpErrorMessage msg = case msg of
    ErrConnect ->
        "could not connect to the server"
    ErrExecuteCommand cmd expectedReply ->
        "could not execute command " ++ show cmd ++
          ", expected reply code " ++ show expectedReply
    ErrAcceptData ->
        "server would not accept any data"
    ErrAuth ->
        "authentication failed"

throwErrorMessage :: MonadSMTP m => ErrorMessage -> Maybe ReplyCode -> m a
throwErrorMessage errMessage errReplyCode = throwSMTPError SMTPError{..}

------------------------------------------------------------------------
-- Actions

-- | Get initial response from SMTP server, and issue 'EHLO'.
-- To satisfy the 'HostName' parameter, use 'Network.BSD.getHostName'.
--
-- This must be the first command issued in an SMTP session.
hello :: MonadSMTP m => HostName -> m ()
hello senderHost = do
    (code, _) <- getResponse
    unless (code == 220) $
        throwErrorMessage ErrConnect (Just code)
    _msg <- tryCommand 3 (EHLO $ B8.pack senderHost) 250
    return ()

-- | Send 'QUIT'.  This should be called before closing the connection.
quit :: MonadSMTP m => m ()
quit = tryOnce QUIT 221 >> return ()

authenticate :: MonadSMTP m => AuthType -> UserName -> Password -> m ()
authenticate at user pass =
    sendCommand (AUTH at user pass) >> return ()

sendRenderedMail
    :: MonadSMTP m
    => ByteString      -- ^ sender email address
    -> [ByteString]    -- ^ receivers
    -> ByteString      -- ^ data
    -> m ()
sendRenderedMail sender receivers dat = do
    _ <- tryOnce (MAIL sender) 250
    mapM_ (\r -> tryOnce (RCPT r) 250) receivers
    _ <- tryOnce (DATA dat) 250
    return ()

-- | Render a 'Mail' using 'renderMail', then send it.  This has a 'MonadIO'
-- constraint because 'renderMail' uses random number generation.
renderAndSend :: (MonadSMTP m, MonadIO m) => Mail -> m ()
renderAndSend mail@Mail{..} = do
    rendered <- liftIO $ fmap lazyToStrict $ renderMail' mail
    sendRenderedMail from to rendered
  where
    from = encodeAddress mailFrom
    to   = map encodeAddress mailTo

encodeAddress :: Address -> ByteString
encodeAddress = T.encodeUtf8 . addressEmail

lazyToStrict :: BL.ByteString -> B.ByteString
lazyToStrict = B.concat . BL.toChunks

------------------------------------------------------------------------
-- Raw command execution

-- | Send a 'Command' to the SMTP server
sendCommand :: MonadSMTP m => Command -> m (ReplyCode, ByteString)
sendCommand (DATA dat) =
    do sendLine "DATA"
       (code, _) <- getResponse
       unless (code == 354) $ throwErrorMessage ErrAcceptData (Just code)
       mapM_ sendLine $ B8.lines dat ++ [B8.singleton '.']
       getResponse
sendCommand (AUTH LOGIN username password) =
    do sendLine command
       _ <- getResponse
       sendLine userB64
       _ <- getResponse
       sendLine passB64
       (code, msg) <- getResponse
       unless (code == 235) $ throwErrorMessage ErrAuth (Just code)
       return (code, msg)
    where command = "AUTH LOGIN"
          (userB64, passB64) = encodeLogin username password
sendCommand (AUTH at username password) =
    do sendLine command
       (code, msg) <- getResponse
       unless (code == 334) $ throwErrorMessage ErrAuth (Just code)
       sendLine $ auth at (B8.unpack msg) username password
       getResponse
    where command = B8.pack $ unwords ["AUTH", show at]
sendCommand meth =
    do sendLine command
       getResponse
    where command = case meth of
                      (HELO param) -> "HELO " <> param
                      (EHLO param) -> "EHLO " <> param
                      (MAIL param) -> "MAIL FROM:<" <> param <> ">"
                      (RCPT param) -> "RCPT TO:<" <> param <> ">"
                      (EXPN param) -> "EXPN " <> param
                      (VRFY param) -> "VRFY " <> param
                      (HELP msg)   -> if B8.null msg
                                        then "HELP\r\n"
                                        else "HELP " <> msg
                      NOOP         -> "NOOP"
                      RSET         -> "RSET"
                      QUIT         -> "QUIT"
                      DATA{}       ->
                          error "BUG: DATA pattern should be matched by sendCommand patterns"
                      AUTH{}       ->
                          error "BUG: AUTH pattern should be matched by sendCommand patterns"

getResponse :: MonadSMTP m => m (ReplyCode, ByteString)
getResponse =
    do (code, bdy) <- readLines
       return (read $ B8.unpack code, B8.unlines bdy)
    where readLines =
              do l <- recvLine
                 let (c, bdy) = B8.span isDigit l
                 if not (B8.null bdy) && B8.head bdy == '-'
                    then do (c2, ls) <- readLines
                            return (c2, B8.tail bdy:ls)
                    else return (c, [B8.tail bdy])

-- | Attempt to send a 'Command' to the SMTP server once
tryOnce :: MonadSMTP m => Command -> ReplyCode -> m ByteString
tryOnce = tryCommand 1

-- | Repeatedly attempt to send a 'Command' to the SMTP server
tryCommand :: MonadSMTP m
           => Int -> Command -> ReplyCode
           -> m ByteString
tryCommand tries cmd expectedReply | tries <= 0 =
    throwErrorMessage (ErrExecuteCommand cmd expectedReply) Nothing
tryCommand tries cmd expectedReply = do
    (code, msg) <- sendCommand cmd
    if code == expectedReply then
        return msg
    else if tries > 1 then
        tryCommand (tries - 1) cmd expectedReply
    else
        throwErrorMessage (ErrExecuteCommand cmd expectedReply) (Just code)
