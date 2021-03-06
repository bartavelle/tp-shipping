module Shipping (module ShippingShared, module Shipping) where

import           Data.List       (foldl')
import qualified Data.Map.Strict as M
import           ShippingShared

{-
 record Syntax:

 * definition

 data Record
    = Record
    { _element1 :: Int
    , _element2 :: String
    } deriving (Show)

 * creation

 foo :: Record
 foo = Record 34 "ddddd"

 * matching

getElement1 :: Record -> Int
getElement1 r =
  case r of
    Record x _ -> x

 * modification

setElements :: Int -> String -> Record -> Int
setElements x s r = r { _element1 = x, _element2 = s }
-}

-- the following imports might be useful at some point for advanced users
-- import           Control.Monad.Writer.Strict
-- import           Data.Maybe                  (mapMaybe)

-- | This is YOUR data type that defines how your state is represented.
-- Choose carefully!
data ShippingState
    = ShippingState
    { _stuff :: String
    , _machin :: [Int]
    } deriving (Show)

-- | Customize this if you want nicer debug messages
prettyShippingState :: ShippingState -> [String]
prettyShippingState s = [show s]

-- | Increase the stock
-- you should be able to use a function from Data.Map.Strict directly
increaseStock :: Stock -> Stock -> Stock
increaseStock = error "complete the increaseStock function"
{-
decreaseStock (M.singleton (ItemId 5) 3) (M.singleton (ItemId 5) 2) = Just (M.singleton (ItemId 5) 1)
decreaseStock (M.singleton (ItemId 5) 3) (M.singleton (ItemId 5) 3) = Just M.empty
decreaseStock (M.singleton (ItemId 5) 3) (M.singleton (ItemId 5) 4) = Nothing
-}

-- | Remove items from stock, if there are enough
-- if you are new to pure functional programming, try transforming the "items to be removed" in to a list, and using a helper function to iterate through that list
-- if you are experienced, look at mergeA function from Data.Map.Merge.Strict
decreaseStock
  :: Stock -- ^ the main stock
  -> Stock -- ^ items to be removed
  -> Maybe Stock -- ^ the updated stock, if there are enough items
decreaseStock = error "complete the decreaseStock function"

-- | return the current day from your `ShippingState` type.
-- you should update the `ShippingState` type to store this information
getDay :: ShippingState -> Day
getDay = error "complete the getDay function"

-- | create the initial shipping state, with the current day as input.
initialShippingState :: Day -> ShippingState
initialShippingState = error "complete the initialShippingState function"

-- | handle the order message
order
  :: OrderId
  -> OrderInformation
  -> Stock -- ^ current stock
  -> ShippingState -- ^ current shipping state
  -> (Stock, ShippingState, Maybe ShippingInformation) -- ^ updated stock, shippingstate, and possibly messages
order = error "complete the order function"

-- | return the list, in order of arrival, of the identifiers of orders waiting for stock to be replenished
getWaitingOrders :: ShippingState -> [OrderId]
getWaitingOrders = error "complete the getWaitingOrders function"

getWaitingTracking :: ShippingState -> [(OrderId, Day)]
getWaitingTracking = error "complete the getWaitingTracking function"

-- | handle new stock being received
-- do not forget the requests must be served *in order*
-- You will probably have to foldl' through the waiting orders
restock
  :: Stock -- ^ current stock
  -> Stock -- ^ stock received
  -> ShippingState -- ^ current shipping state
  -> (Stock, ShippingState, [ShippingInformation]) -- ^ updated stock, shippingstate, and possibly shipping information
restock = error "complete the restock function"

-- | This function increases the current day in ShippingState.
-- In order to get the full grade, you should implement this function so that it takes action when things get lost.
-- In particular:
--   * tracking requests can get lost. They must be reissued if no response has been received after 5 days, so that the transporter can pick the order up.
--   * items that have been shipped can also be lost. After 7 days, the following actions must be performed:
--       - for standard delivery, you can assume the item has been delivered
--       - for tracked packages, it means it has been lost, and you must send it again (stocks must be decreased again, a new tracking id must be used, just like a new order)
-- The implementation of this function can be greatly simplified by using the Writer monad (that logs the orders) and some form of traversal.
advanceDay :: Stock -> ShippingState -> (Stock, ShippingState, [OutEvent])
advanceDay = error "complete the advanceDay function"

getInTransit :: ShippingState -> [(OrderId, Day)]
getInTransit = error "complete the getInTransit function"

handleMessage
  :: Stock -- ^ initial stock before message handling
  -> ShippingState -- ^ shipping state after message handling
  -> InEvent
  -> (Stock, ShippingState, [OutEvent]) -- ^ updated states, with messages
handleMessage stock st event =
  case event of
    NewOrder oid oi ->
      let (stock', st', shipping) = order oid oi stock st
      in  (stock', st',  [maybe (OutofstockMessage oid) Ship shipping])
    Restock nstock ->
      let (stock', st', shippings) = restock stock nstock st
      in  (stock', st', map Ship shippings)
    NewDay -> advanceDay stock st
    _ -> error ("complete the branch in handleMessage for: " ++ show event)

handleMessages
  :: Stock
  -> ShippingState
  -> [InEvent]
  -> (Stock, ShippingState, [OutEvent])
handleMessages stock state = foldl' run1 (stock, state, [])
  where
    run1 (curstock, curstate, curout) = (\(a, b, c) -> (a, b, curout ++ c)) . handleMessage curstock curstate
