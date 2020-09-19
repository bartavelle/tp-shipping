module Basics where

-- | Write a functions that adds two Double.
add :: Double -> Double -> Double
add = undefined

-- | Write a functions that divides two Double.
divide :: Double -> Double -> Double
divide = undefined

-- This is a sum type. A value of type "DoubleResult" can either be
-- a DoubleSuccess containing a Double, or a DoubleFailure.
--
-- This will be used to identify operations that fail (for example, because
-- of a divide by 0 operation).
data DoubleResult
    = DoubleSuccess Double
    | DoubleFailure
    deriving (Show, Eq)

-- | Rewrite a function that divides two Doubles, but that explicitely
-- fails when it divides by 0, or explicitely succeeds.
divide' :: Double -> Double -> DoubleResult
divide' = undefined

-- | This is a generic (parametric) type. It represents something that can
-- either fail, or succeed with a value of type "a".
data Result a
    = Success a
    | Failure
    deriving (Show, Eq)

-- | Write this function again, but this time using the generic type.
divide'' :: Double -> Double -> Result Double
divide'' = undefined

-- | Represent the kind of expressions one can compute on a pocket calculator.
data Operation
    = Add Operation Operation
    | Sub Operation Operation
    | Mul Operation Operation
    | Div Operation Operation
    | Value Double
    deriving (Show, Eq)

-- | This is (5 * 3.4) + 8
-- run it in GHCi
example1 :: Double
example1 = eval (Add (Mul (Value 5) (Value 3.4)) (Value 8))

-- | What is this ?
example2 :: Double
example2 = eval (Add (Div (Value 5) (Value 3.4)) (Value 0))

-- | From an operation, compute its value.
eval :: Operation -> Double
eval = undefined

-- | Try it in the REPL with example2!
-- Now write the correct function:
eval' :: Operation -> Result Double
eval' = undefined

-- | Examples to run in GHCi
example1' :: Result Double
example1' = eval' (Add (Mul (Value 5) (Value 3.4)) (Value 8))

example2' :: Result Double
example2' = eval' (Add (Div (Value 5) (Value 3.4)) (Value 0))

-- | A (linked) list is either empty, or contains an element (head) and a list (tail).
data List a
    = Empty
    | Cons a (List a)
    deriving Show

-- | Returns the first element of a list.
listHead :: List a -> Result a
listHead = undefined

head1 :: Result Int
head1 = listHead Empty -- Failure
head2 :: Result Int
head2 = listHead (Cons 5 (Cons 4 Empty)) -- success 5

-- | Returns the tail of a list.
listTail :: List a -> Result (List a)
listTail = undefined

tail1 :: Result (List Int)
tail1 = listTail Empty -- Failure
tail2 :: Result (List Int)
tail2 = listTail (Cons 5 (Cons 4 Empty)) -- success (Cons 4 Empty)

-- | Sum of all integers in a list.
listSum :: List Int -> Int
listSum = undefined

-- | Compare two lists for equality.
listEq :: Eq a => List a -> List a -> Bool
listEq = undefined

-- | Converts our list type into Haskell's built-in list type.
toList :: List a -> [a]
toList = undefined

-- Given a function, converts all elements of a list.
lmap
  :: (a -> b)
  -> List a
  -> List b
lmap = undefined

-- | Black magic!
-- Uncomment the relevant test if you wrote it!
ltraverse
  :: Applicative f
  => (a -> f b)
  -> List a
  -> f (List b)
ltraverse = undefined

