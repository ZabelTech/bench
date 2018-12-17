{-# LANGUAGE OverloadedStrings #-}
module Hedis ( Connection
             , delete
             , withRedis
             , Hedis.runRedis
             , runRedisServer ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import Data.Bits
import Data.IP
import Database.Redis
import Data.Word
import Types
import System.Exit             (ExitCode)
import System.Process

iSetKey = "test"

ipToInt :: IP -> Int
ipToInt (IPv4 ip) = fromIntegral $ byteSwap32 $ toHostAddress ip
ipToInt (IPv6 ip) = undefined

toRange = B.pack . show . ipToInt

sendRequest' :: [B.ByteString] -> Redis Res
sendRequest' args =
  either handleReply handleInt <$> sendRequest args
  where handleReply (Bulk Nothing)       = StringRes []
        handleReply (Bulk (Just a))      = StringRes [a]
        handleReply (MultiBulk Nothing)  = StringRes []
        handleReply (MultiBulk (Just a)) = Multi $ map handleReply a
        handleReply (SingleLine a)       = StringRes [a]
        handleReply (Integer a)          = IntRes a
        handleReply (Error err)          = error $ show err
        handleInt = StringRes

delete :: Redis (Either Reply Integer)
delete = del [iSetKey]

instance IpM Redis where
  doAction (Lookup ip) =
    sendRequest' [ "INTERVALLSET.STAB", iSetKey, toRange ip ]
  doAction (Insert start stop ele) =
    sendRequest' [ "INTERVALLSET.INSERT"
                            , iSetKey
                            , toRange start
                            , toRange stop
                            , ele ]
  doAction (Remove ele) =
    sendRequest' [ "INTERVALLSET.REM", iSetKey, ele ]

runRedis conn = liftIO . Database.Redis.runRedis conn

runRedisServer :: IO (ThreadId,Connection)
runRedisServer = do
  tId  <- forkIO $ print =<< run
  conn <- connect
  Hedis.runRedis conn delete
  return (tId,conn)
  where run  = readCreateProcessWithExitCode (proc cmd args) { close_fds = True } ""
        cmd  = "/home/dell/.nix-profile/bin/redis-server"
        args = [ "--loadmodule"
               , "/home/dell/redis_iset/intervalSet.so"
               , "--save \"\""
               , "--appendonly"
               , "no" ]
        connect = fix $ \cont -> do
          result <- tryConnect
          either (const cont) return result
        tryConnect :: IO (Either SomeException Connection)
        tryConnect = try $ checkedConnect defaultConnectInfo

withRedis act =
  bracket runRedisServer (killThread.fst) $ act . snd
