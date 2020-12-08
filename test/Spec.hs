module Main where

import           Data.List             ( sort )
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict       as M
import           Test.Hspec            ( Spec, SpecWith, describe, hspec, it, shouldBe )
import           Test.Hspec.QuickCheck ( prop )

import           Basics                as B
import qualified Maps                  as SM
import           ShippingSpec          ( shippingSpec )

basics :: SpecWith ()
basics = describe "basics" $ do
  describe "simple operations" $ do
    prop "add" (\a b -> add a b == a + b)
    prop "divide" (\a b -> b == 0 || divide a b == a `div` b)
    prop "divide' /= 0" (\a b -> b == 0 || divide' a b == IntSuccess (div a b))
    prop "divide' == 0" (\a -> divide' a 0 == IntFailure)
    prop "divide'' /= 0" (\a b -> b == 0 || divide'' a b == B.Success (div a b))
    prop "divide'' == 0" (\a -> divide'' a 0 == B.Failure)
  describe "eval" $ do
    prop "Value" (\a -> eval (Value a) == a)
    prop "Add" (\a b -> eval (Add (Value a) (Value b)) == a + b)
    prop "Sub" (\a b -> eval (Sub (Value a) (Value b)) == a - b)
    prop "Mul" (\a b -> eval (Mul (Value a) (Value b)) == a * b)
    it "test 1" (eval (Add (Mul (Value 5) (Value 3)) (Value 8)) `shouldBe` 5*3+8)
  describe "eval'" $ do
    prop "Value" (\a -> eval' (Value a) == B.Success a)
    prop "Add" (\a b -> eval' (Add (Value a) (Value b)) == B.Success (a + b))
    prop "Sub" (\a b -> eval' (Sub (Value a) (Value b)) == B.Success (a - b))
    prop "Mul" (\a b -> eval' (Mul (Value a) (Value b)) == B.Success (a * b))
    it "test 1" (eval' (Add (Mul (Value 5) (Value 3)) (Value 8)) `shouldBe` B.Success (5*3+8))
    it "test 2" (eval' (Div (Div (Value 5) (Value 3)) (Value 0)) `shouldBe` B.Failure)
    it "test 3" (eval' (Div (Div (Value 5) (Value 0)) (Value 1)) `shouldBe` B.Failure)
  describe "lists" $ do
    let foo :: Int -> Maybe Char
        foo x
          | x <  0 = Just 'a'
          | x >  0 = Just 'b'
          | otherwise = Nothing
    it "listHead1" (listHead (Cons 1 (Cons 2 Empty)) `shouldBe` Success (1 :: Int))
    it "listHead2" (listHead (Empty :: List Int) `shouldBe` Failure)
    prop "foo" (foo 3 == Just 'b')
    prop "listSum" (\l -> listSum (fromList l) == sum l)
    prop "listEq" (\l1 l2 -> listEq (fromList l1) (fromList l2) == ((l1 :: [Int]) == l2))
    prop "toList" (\l -> toList (fromList l) == (l :: [Int]))
    prop "lmap" (\l -> toList (lmap foo (fromList l)) == fmap foo l)
    -- uncomment the following if you wrote it!
    -- prop "ltraverse" (\l -> fmap toList (ltraverse foo (fromList l)) == traverse foo (l :: [Int]))

fromList :: [a] -> B.List a
fromList = foldr Cons Empty

mapTests :: Spec
mapTests = describe "MyMap tests" $ do
    it "empty" $ M.fromList SM.lempty `shouldBe` (M.empty :: M.Map Int String)
    prop "singleton" $ \k v -> SM.lsingleton k v == M.toList (M.singleton k v :: M.Map Int String)
    describe "insert" $
      prop "prop" $ \k v lst ->
        let mp = M.fromList lst :: M.Map Int String
            lst' = M.toList mp
        in  M.member k mp || M.fromList (SM.linsert k v lst') == M.insert k v mp
    it "insert overwrite" $
      SM.linsert (3 :: Int) "test" (SM.lsingleton 3 "bad") `shouldBe` SM.lsingleton 3 "test"
    describe "delete" $
      prop "prop" $ \k lst ->
        let mp = M.fromList lst :: M.Map Int String
            lst' = M.toList mp
        in  M.fromList (SM.ldelete k lst') == M.delete k mp
    describe "lookup" $
      prop "prop" $ \k lst ->
        let mp = M.fromList lst :: M.Map Int String
            lst' = M.toList mp
        in  M.lookup k mp == SM.llookup k lst'
    describe "lmaximum" $
      prop "prop" $ \lst ->
        let mp = M.fromList lst :: M.Map Int String
            lst' = M.toList mp
        in  SM.lmaximum lst' == case lst' of
              [] -> Nothing
              _ -> Just (maximum (map snd lst'))
    describe "fromList" $
      prop "prop" $ \lst ->
        let mp = M.fromList lst :: M.Map Int String
            lst' = M.toList mp
        in  lst' == sort (SM.fromList lst)
    describe "lfold (bonus)" $
      prop "lfold" $ \lst ->
        let mp = SM.fromList lst :: SM.MyMap Int String
        in  SM.lfold (++) "" mp == foldr (\e c -> snd e ++ c) "" mp
    describe "lsum" $
      prop "prop" $ \lst ->
        let mp = M.fromList lst :: M.Map String Integer
            lst' = M.toList mp
        in  SM.lsum lst' == sum mp
    describe "lminimum" $
      prop "prop" $ \lst ->
        let mp = M.fromList lst :: M.Map Int String
            lst' = M.toList mp
        in  SM.lminimum lst' == case lst' of
              [] -> Nothing
              _ -> Just (minimum (map snd lst'))
    describe "lfold" $ do
      prop "prop add" $ \lst ->
        let mp = M.fromList lst :: M.Map String Int
            lst' = M.toList mp
        in  SM.lfold (+) 0 lst' == sum (map snd lst')
      prop "prop multiply" $ \lst ->
        let mp = M.fromList lst :: M.Map String Int
            lst' = M.toList mp
        in  SM.lfold (*) 1 lst' == product (map snd lst')
    describe "intersectionWith" $
      prop "prop" $ \lst1 lst2 ->
        let mp1 = M.fromList lst1 :: M.Map Int String
            lst1' = M.toList mp1
            mp2 = M.fromList lst2 :: M.Map Int Int
            lst2' = M.toList mp2
        in  M.fromList (SM.intersectionWith replicate lst2' lst1') ==
            M.merge M.dropMissing M.dropMissing (M.zipWithMatched (const replicate)) mp2 mp1
    -- describe "mergeA" $ do
    --   let m1 = [(1, "lol"), (2, "lal")] :: SM.MyMap Int String
    --       m2 = [(1, 3), (3, 1)] :: SM.MyMap Int Int
    --   it "simple intersection" $
    --     SM.mergeA (\_ v1 -> Just [v1])
    --               (\_ _ -> Just [])
    --               (\_ k1 k2 -> Just (replicate k2 k1))
    --               m1
    --               m2 `shouldBe` Just [(1, replicate 3 "lol"), (2, ["lal"]), (3, [])]
    --   it "destructive intersection A" $
    --     SM.mergeA (\_ v1 -> Just [v1])
    --               (\_ _ -> Nothing)
    --               (\_ k1 k2 -> Just (replicate k2 k1))
    --               m1
    --               m2 `shouldBe` Nothing
    --   it "destructive intersection B" $
    --     SM.mergeA (\_ v1 -> Just [v1])
    --               (\_ _ -> Nothing)
    --               (\_ k1 k2 -> Just (replicate k2 k1))
    --               m1
    --               [(1, 3)] `shouldBe` Just [(1, replicate 3 "lol"), (2, ["lal"])]

main :: IO ()
main = hspec $ do
  basics
  mapTests
  shippingSpec
