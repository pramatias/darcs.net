{-# LANGUAGE CPP #-}

module HTTP( fetchUrl, postUrl, requestUrl, waitNextUrl, ConnectionError(..) ) where

import Darcs.Global ( debugFail )
import Version ( version )

#ifdef HAVE_HTTP
import Control.Monad ( when )
import Data.IORef ( newIORef, readIORef, writeIORef, IORef )
import Network.HTTP
import Network.URI
import System.Environment ( getEnv )
import System.IO.Error ( ioeGetErrorString )
import System.IO.Unsafe ( unsafePerformIO )
import Darcs.Global ( debugMessage )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
#endif
-- | Data type to represent a connection error.
-- The following are the codes from libcurl
-- which map to each of the constructors:
-- * 6  -> CouldNotResolveHost : The remote host was not resolved.
-- * 7  -> CouldNotConnectToServer : Failed to connect() to host or proxy.
-- * 28 -> OperationTimeout: the specified time-out period was reached.
data ConnectionError = CouldNotResolveHost     |
                       CouldNotConnectToServer |
                       OperationTimeout
               deriving (Eq, Read, Show)

fetchUrl :: String -> IO String
postUrl
    :: String     -- ^ url
    -> String     -- ^ body
    -> String     -- ^ mime type
    -> IO ()  -- ^ result

requestUrl :: String -> FilePath -> a -> IO String
waitNextUrl :: IO (String, String, Maybe ConnectionError)

#ifdef HAVE_HTTP

headers :: [Header]
headers =  [Header HdrUserAgent $ "darcs-HTTP/" ++ version]

fetchUrl url = case parseURI url of
    Nothing -> fail $ "Invalid URI: " ++ url
    Just uri -> do debugMessage $ "Fetching over HTTP:  "++url
                   proxy <- getProxy
                   when (not $ null proxy) $
                     debugFail "No proxy support for HTTP package yet (try libcurl)!"
                   resp <- simpleHTTP $ Request { rqURI = uri,
                                                  rqMethod = GET,
                                                  rqHeaders = headers,
                                                  rqBody = "" }
                   case resp of
                     Right res@Response { rspCode = (2,0,0) } -> return (rspBody res)
                     Right Response { rspCode = (x,y,z) } ->
                         debugFail $ "HTTP " ++ show x ++ show y ++ show z ++ " error getting " ++ show uri
                     Left err -> debugFail $ show err

postUrl url body mime = case parseURI url of
    Nothing -> fail $ "Invalid URI: " ++ url
    Just uri -> do debugMessage $ "Posting to HTTP:  "++url
                   proxy <- getProxy
                   when (not $ null proxy) $
                     debugFail "No proxy support for HTTP package yet (try libcurl)!"
                   resp <- simpleHTTP $ Request { rqURI = uri,
                                                  rqMethod = POST,
                                                  rqHeaders = headers ++ [Header HdrContentType mime,
                                                                          Header HdrAccept "text/plain",
                                                                          Header HdrContentLength
                                                                                     (show $ length body) ],
                                                  rqBody = body }
                   case resp of
                     Right res@Response { rspCode = (2,y,z) } -> do
                        putStrLn $ "Success 2" ++ show y ++ show z
                        putStrLn (rspBody res)
                        return ()
                     Right res@Response { rspCode = (x,y,z) } -> do
                        putStrLn $ rspBody res
                        debugFail $ "HTTP " ++ show x ++ show y ++ show z ++ " error posting to " ++ show uri
                     Left err -> debugFail $ show err

requestedUrl :: IORef (String, FilePath)
requestedUrl = unsafePerformIO $ newIORef ("", "")

requestUrl u f _ = do
  (u', _) <- readIORef requestedUrl
  if null u'
     then do writeIORef requestedUrl (u, f)
             return ""
     else return "URL already requested"

waitNextUrl = do
  (u, f) <- readIORef requestedUrl
  if null u
     then return ("", "No URL requested", Nothing)
     else do writeIORef requestedUrl ("", "")
             e <- (fetchUrl u >>= \s -> B.writeFile f (BC.pack s) >> return "") `catch` h
             let ce = case e of
                       "timeout" -> Just OperationTimeout
                       _         -> Nothing
             return (u, e, ce)
    where h = return . ioeGetErrorString

getProxy :: IO String
getProxy =
  getEnv "http_proxy"
  `catch` \_ -> getEnv "HTTP_PROXY"
  `catch` \_ -> return ""
#else

fetchUrl _ = debugFail "Network.HTTP does not exist"
postUrl _ _ _ = debugFail "Cannot use http POST because darcs was not compiled with Network.HTTP."

requestUrl _ _ _ = debugFail "Network.HTTP does not exist"
waitNextUrl = debugFail "Network.HTTP does not exist"

#endif
