{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ShippingShared where

import qualified Data.Map.Strict as M

-- | identifies days
newtype Day =
  Day Int
  deriving (Show, Eq, Ord, Num)

-- | identifies item types
newtype ItemId =
  ItemId Int
  deriving (Show, Eq, Ord, Num)

-- | identifies orders
newtype OrderId =
  OrderId Int
  deriving (Show, Eq, Ord, Enum)

-- | tracking number for parcels
newtype TrackingId =
  TrackingId Int
  deriving (Show, Eq, Ord)

-- | type of deliveries
data DeliveryType
  = Standard -- ^ standard delivery, will not be acknowledged
  | Verified -- ^ delivery will be acknowledged with a ParcelDelivered message
  deriving (Show, Eq)

-- | amount of items in stock
-- invariant: items with a stock of 0 must be removed
type Stock = M.Map ItemId Int

-- | events that can be received
data InEvent
  = NewOrder OrderId OrderInformation  -- ^ a new order has to be processed
  | ParcelHandled OrderId TrackingId -- ^ the shipping company has received the order and is ready to ship it
  | ParcelReturned TrackingId  -- ^ parcel could not be delivered, and has been sent back to the warehouse
  | ParcelDelivered TrackingId -- ^ a verified parcel has been delivered, or a maildrop parcel is waiting to be picked up
  | NewDay -- ^ a new day has come
  | Restock Stock -- ^ stock has been received
  deriving (Show, Eq)

-- | events that can be sent
-- if multiple events must be sent at the same time, they must be ordered:
--   * first, Ship messages
--   * OutofstockMessage, ordered by OrderId
--   * InconsistentState, ordered by message
data OutEvent
  = Ship ShippingInformation  -- ^ ask the shipping company to deliver a parcel
  | OutofstockMessage OrderId -- ^ tell the customer that part of its order is out of stock, and that it should way some more
  | InconsistentState String -- ^ something is wrong with an InEvent
  deriving (Show, Eq)

data Destination
  = Destination
  { _address :: String
  , _method  :: DeliveryType
  } deriving (Show, Eq)

data OrderInformation
  = OrderInformation
  { _orderStock :: Stock
  , _orderDest  :: Destination
  } deriving (Show, Eq)

data ShippingInformation
  = ShippingInformation
  { _orderId      :: OrderId -- ^ internal identifier for this order
  , _shippingDest :: Destination
  } deriving (Show, Eq)

-- | a debug utility function
compactStocks :: Stock -> String
compactStocks = show . map (\(ItemId i, q) -> (i, q)) . M.toList