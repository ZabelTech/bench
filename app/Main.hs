{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.List (deleteBy, sortOn)
import Hedis
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Types
import GHC.Generics
import Generic.Random.Generic
import Data.IP
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import System.Exit             (ExitCode)
import System.Process

type IpDB = M.Map B.ByteString (IP,IP)

data Stats = Stats { getInserts :: Int
                   , getLookups :: (Int, Int)
                   , getRemoves :: (Int, Int) }
  deriving (Show)

instance Monoid Stats where
  mempty = Stats 0 (0,0) (0,0)
  a `mappend` b = Stats (getInserts a + getInserts b)
                        (addTuple (getLookups a) (getLookups b))
                        (addTuple (getRemoves a) (getRemoves b))
    where addTuple (a,b) (a',b') = (a+a',b+b')

type CheckM = WriterT Stats (StateT IpDB IO)

incrLookups [] = tell $ mempty { getLookups = (1,0) }
incrLookups xs = tell $ mempty { getLookups = (0,length xs) }

incrInserts = tell $ mempty { getInserts = 1 }

incrRems Nothing  = tell $ mempty { getRemoves = (1,0) }
incrRems (Just _) = tell $ mempty { getRemoves = (0,1) }

instance IpM CheckM where
  doAction (Lookup ip) = do
    results <- gets $ M.keys . M.filter isInRange
    incrLookups results
    return $ StringRes results
    where isInRange (start,stop) =
            start <= ip && stop >= ip
  doAction (Insert start stop e) = do
    modify' $ M.insert e (start,stop)
    incrInserts
    return $ IntRes 1
  doAction (Remove e) = do
    hits <- gets $ M.lookup e
    modify' $ M.delete e
    incrRems hits
    return $ IntRes $ maybe 0 (const 1) hits
    where notE (_,_,e') = e' /= e

prop1 :: (MonadIO m, IpM m) => Connection -> Action -> m Property
prop1 conn action = do
  result   <- runRedis conn $ doAction action
  expected <- doAction action
  return $ result === expected

test :: Connection -> Property
test conn =
  forAll gen $ \actions -> monadicIO $ do
   run (runRedis conn delete)
   ((props,stats),s) <- run (runStateT (runWriterT (prop (init : actions))) mempty)
   run (print stats)
   return $ conjoin props
  where gen = take 10000 <$> infiniteList
        prop :: [Action] -> CheckM [Property]
        prop = mapM (prop1 conn)
        init = Insert "127.0.0.1" "127.0.0.1" "init"

main =
  withRedis $ \conn -> do
    quickCheckWithResult args $ withMaxSuccess 10000 $ test conn
    where args = stdArgs { chatty     = True
                         , maxShrinks = 5 }

test' = withRedis $ \conn -> do
  runRedis conn $
    forM cmds $ \cmd -> do
      doAction cmd
  where cmds = [Insert "228.161.3.0" "237.55.2.0" "k",
                Insert "47.207.3.0" "77.168.1.0" "k",
                Insert "196.163.2.0" "211.165.189.1" "b",
                Insert "2.83.0.0" "99.190.2.0" "k",
                Lookup "54.127.3.0"
               ]
        init = Insert "127.0.0.1" "127.0.0.1" "init"

test'' = withRedis $ \_conn -> threadDelay 1000000000
