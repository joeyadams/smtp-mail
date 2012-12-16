{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module Network.Mail.SMTP
    ( -- * Main interface
      sendMail
    , sendMailWithLogin
    , simpleMail
    , plainTextPart
    , htmlPart
    , filePart

    -- * Types
    , module Network.Mail.SMTP.Types
    , SMTPConnection

      -- * Network.Mail.Mime's sendmail interface (reexports)
    , sendmail
    , sendmailCustom
    , renderSendMail
    , renderSendMailCustom

      -- * Establishing Connection
    , connectSMTPPort
    , connectSMTP

      -- * Operation to a Connection
    , sendCommand
    , login
    , closeSMTP
    , renderAndSend
    )
    where

import Network.Mail.SMTP.Abstract (MonadSMTP, quit)
import qualified Network.Mail.SMTP.Abstract as Abstract
import Network.Mail.SMTP.Auth
import Network.Mail.SMTP.Types

import System.IO
import System.FilePath (takeFileName)

import Control.Exception
import Control.Monad.IO.Class
import Data.Word (Word8)

import Network
import Network.BSD (getHostName)
import Network.Mail.Mime hiding (simpleMail)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

data SMTPConnection = SMTPC !Handle

instance Eq SMTPConnection where
    (==) (SMTPC a) (SMTPC b) = a == b

-- | Connect to an SMTP server with the specified host and port
connectSMTPPort :: String     -- ^ name of the server
                -> PortNumber -- ^ port number
                -> IO SMTPConnection
connectSMTPPort hostname port =
    connectTo hostname (PortNumber port) >>= connectStream

-- | Connect to an SMTP server with the specified host and default port (25)
connectSMTP :: String     -- ^ name of the server
            -> IO SMTPConnection
connectSMTP = flip connectSMTPPort 25

-- | Create an 'SMTPConnection' from an already connected Handle
connectStream :: Handle -> IO SMTPConnection
connectStream h = do
    let conn = SMTPC h
    senderHost <- getHostName
    runSMTP conn $ Abstract.hello senderHost
    return conn

-- | Send a 'Command' to the SMTP server
sendCommand :: SMTPConnection -> Command -> IO (ReplyCode, ByteString)
sendCommand conn = runSMTP conn . Abstract.sendCommand

-- | Send 'QUIT' and close the connection.
closeSMTP :: SMTPConnection -> IO ()
closeSMTP c@(SMTPC conn) = sendCommand c QUIT >> hClose conn

-- | Render a 'Mail' to a 'ByteString' then send it over the specified
-- 'SMTPConnection'
renderAndSend ::SMTPConnection -> Mail -> IO ()
renderAndSend conn mail =
    runSMTP conn $ Abstract.renderAndSend mail

-- | Connect to an SMTP server, send a 'Mail', then disconnect.
sendMail :: String -> PortNumber -> Mail -> IO ()
sendMail host port mail = do
  con <- connectSMTPPort host port
  renderAndSend con mail
  closeSMTP con

-- | Connect to an SMTP server, log in, send a 'Mail', disconnect.
sendMailWithLogin :: String -> PortNumber -> UserName -> Password -> Mail -> IO ()
sendMailWithLogin host port user pass mail = do
  con <- connectSMTPPort host port
  _ <- sendCommand con (AUTH LOGIN user pass)
  renderAndSend con mail
  closeSMTP con

-- | A convenience function that sends 'AUTH' 'LOGIN' to the server
login :: SMTPConnection -> UserName -> Password -> IO (ReplyCode, ByteString)
login con user pass = sendCommand con (AUTH LOGIN user pass)

-- | A simple interface for generating a 'Mail' with a plantext body and
-- an optional HTML body.
simpleMail :: Address   -- ^ from
           -> [Address] -- ^ to
           -> [Address] -- ^ CC
           -> [Address] -- ^ BCC
           -> T.Text -- ^ subject
           -> [Part] -- ^ list of parts (list your preferred part last)
           -> Mail
simpleMail from to cc bcc subject parts =
    Mail { mailFrom = from
         , mailTo   = to
         , mailCc   = cc
         , mailBcc  = bcc
         , mailHeaders = [ ("Subject", subject) ]
         , mailParts = [parts]
         }

-- | Construct a plain text 'Part'
plainTextPart :: TL.Text -> Part
plainTextPart = Part "text/plain; charset=utf-8" 
              QuotedPrintableText Nothing [] . TL.encodeUtf8

-- | Construct an html 'Part'
htmlPart :: TL.Text -> Part
htmlPart = Part "text/html; charset=utf-8" 
             QuotedPrintableText Nothing [] . TL.encodeUtf8

-- | Construct a file attachment 'Part'
filePart :: T.Text -- ^ content type
         -> FilePath -- ^ path to file 
         -> IO Part
filePart ct fp = do
    content <- BL.readFile fp
    return $ Part ct Base64 (Just $ T.pack (takeFileName fp)) [] content 

------------------------------------------------------------------------
-- The SMTP monad

newtype SMTP a = SMTP
    { runSMTP' :: SMTPConnection -> IO a
    }

runSMTP :: SMTPConnection -> SMTP a -> IO a
runSMTP = flip runSMTP'

instance Monad SMTP where
    return a = SMTP $ \_r -> return a

    m >>= k = SMTP $ \r -> do
        a <- runSMTP' m r
        runSMTP' (k a) r

    fail s = SMTP $ \_r -> fail s

instance MonadIO SMTP where
    liftIO io = SMTP $ \_r -> io

instance MonadSMTP SMTP where
    sendLine s = SMTP $ \(SMTPC h) ->
        B.hPut h s >> B.hPut h "\r\n" >> hFlush h

    recvLine = SMTP $ \(SMTPC h) ->
        fmap trimCR $ B8.hGetLine h

    throwSMTPError err = do
        quit
        SMTP $ \(SMTPC h) -> do
            hClose h
            throwIO err

trimCR :: ByteString -> ByteString
trimCR s = case unsnoc s of
               Just (s', 13) -> s'
               _otherwise    -> s

unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc s | B.null s  = Nothing
         | otherwise = Just (B.init s, B.last s)
