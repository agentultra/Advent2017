import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Protolude
import Seven
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Seven" $ do
    describe "supports" $ do
      it "should return the empty map for an empty list" $ do
        (supports []) `shouldBe` Map.empty
      it "should skip programs without children" $ do
        let p = Program { _name = "foo", _weight = 1, _disc = Nothing }
          in
          (supports [p]) `shouldBe` Map.empty
      it "should add programs with children" $ do
        let p = Program { _name = "Bar", _weight = 1, _disc = Just ["Foo"] }
          in
          (supports [p]) `shouldBe` Map.fromList [("Bar", Just ["Foo"])]

    describe "supported" $ do
      it "return the empty set from an empty map" $ do
        (supported Map.empty) `shouldBe` Set.fromList []
      it "should return the set of children from a map" $ do
        let m = Map.fromList [("Foo", Just ["Bar", "Baz"])]
          in
          (supported m) `shouldBe` Set.fromList ["Bar", "Baz"]
      it "should return the set of all children from a map" $ do
        let m = Map.fromList [ ("Foo", Just ["Bar", "Baz"])
                             , ("Bing", Just ["Frob"])
                             ]
          in
          (supported m) `shouldBe` Set.fromList ["Bar", "Baz", "Frob"]

    describe "findBottom" $ do
      it "should find Nothing from the empty list" $ do
        (findBottom []) `shouldBe` Nothing
      it "should find the Program name in supports that is not supported" $ do
        let programs = [ Program { _name = "root", _weight = 1, _disc = Just ["Baz"]}
                       , Program { _name = "Baz", _weight = 1, _disc = Just ["Qux"] }
                       ]
          in
          (findBottom programs) `shouldBe` Just "root"
