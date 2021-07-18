module Data.ProtoBuf.ZigZagSpec
    ( main
    , spec
    ) where

import Data.ProtoBuf.ZigZag (decode32, decode64, encode32, encode64)
import Test.Hspec (Spec, hspec, describe, it)
import Test.QuickCheck (Testable(property))
import Test.QuickCheck.Instances ()


main :: IO ()
main = hspec spec


spec :: Spec
spec =
    describe "ZigZag" $ do
      it "decode32 inverses encode32" $ property $
        \x -> (decode32 . encode32) x == x

      it "decode64 inverses encode64" $ property $
        \x -> (decode64 . encode64) x == x
