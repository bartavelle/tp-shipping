module Maps where

-- | Maps are lists of key/value pairs

-- !IMPORTANT! keys must be kept unique!
type MyMap k v = [(k, v)]

-- | The empty map
lempty :: MyMap k v
lempty = undefined

-- | The map with a single key/value pair
lsingleton :: k -> v -> MyMap k v
lsingleton = undefined

-- | Insert a key value pair into the map.
-- On key collision, the value is overwritten.
linsert
  :: Eq k
  => k
  -> v
  -> MyMap k v
  -> MyMap k v
linsert = undefined

-- | Remove a key/value pair based on a key.
ldelete
  :: Eq k
  => k
  -> MyMap k v
  -> MyMap k v
ldelete = undefined

-- | Finds the value associated with a key, if it exists.
llookup
  :: Eq k
  => k
  -> MyMap k v
  -> Maybe v
llookup = undefined

-- | Finds the largest value, if it exists.
lmaximum
  :: Ord v
  => MyMap k v
  -> Maybe v
lmaximum = undefined

-- | Create a Map from a list. Althrough they have the same type,
-- the "MyMap" must preserve the unicity key invariant. Keys that appear
-- earlier in the input list are overwritten on collision.
fromList
  :: Eq k
  => [(k, v)]
  -> MyMap k v
fromList = undefined

-- | Do you understand what this function is? Even if you do not, can you
-- write it?
lfold :: (v -> a -> a)
      -> a
      -> MyMap k v
      -> a
lfold = undefined

-- | Sum of all the values in the map. Implement it with lfold.
lsum
  :: MyMap k Integer
  -> Integer
lsum = undefined

-- | Finds the smallest value, if it exists. Implement it with lfold.
lminimum
  :: Ord v
  => MyMap k v
  -> Maybe v
lminimum = undefined

-- | Intersection of two maps. You should use lfold.
intersectionWith
  :: Eq k
  => (a -> b -> c)
  -> MyMap k a
  -> MyMap k b
  -> MyMap k c
intersectionWith = undefined

-- | optional assignment, only for the brave!
mergeA
  :: (Eq k, Applicative f)
  => (k -> a -> f c)  -- when it is only in map 1
  -> (k -> b -> f c)  -- when it is only in map 2
  -> (k -> a -> b -> f c) -- when it is in both maps
  -> MyMap k a -- first map
  -> MyMap k b -- second map
  -> f (MyMap k c) -- merged map
mergeA = undefined