module Types where

import Data.ByteString.Char8
import Data.IP
import qualified Data.Set as S
import Database.Redis (Reply)
import Test.QuickCheck

data Action = Lookup IP
            | Insert IP IP ByteString
            | Remove ByteString
  deriving (Show)

data Res = StringRes [ByteString]
         | IntRes Integer
         | Empty
         | Multi [Res]
  deriving(Show,Ord)

instance Eq Res where
  StringRes l1 == StringRes l2 = S.fromList l1 == S.fromList l2
  Multi l1     == Multi l2     = S.fromList l1 == S.fromList l2
  Empty        == Empty        = True
  IntRes a     == IntRes b     = a == b
  _            == _            = False

class Monad m => IpM m where
  doAction :: Action -> m Res

instance Arbitrary IPv4 where
  arbitrary = fromHostAddress <$> arbitrary

instance Arbitrary ByteString where
  arbitrary = pack <$> arbitrary

ipGen :: Gen IP
ipGen = IPv4 <$> arbitrary

instance Arbitrary Action where
  arbitrary =
    oneof [ lookupGen, insertGen, removeGen ]
    where lookupGen = Lookup <$> ipGen
          insertGen = do
            start <- ipGen
            stop  <- suchThat ipGen (start <=)
            Insert start stop <$> strGen
          removeGen = Remove <$> strGen
          strGen    = pack <$> vectorOf 1 (elements ['a'..'z'])
