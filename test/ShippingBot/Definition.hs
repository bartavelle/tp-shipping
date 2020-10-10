{-# LANGUAGE TemplateHaskell #-}
module ShippingBot.Definition where

import           Control.Monad       (foldM, replicateM)
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import           Lens.Micro.Platform
import           Shipping            (Day (..), DeliveryType, ItemId (..), OrderId (..), ShippingState, Stock,
                                      TrackingId, initialShippingState)
import           Test.QuickCheck.Gen (Gen, choose, suchThat)

data Bot
  = Bot
  { _behaviour    :: BotState
  , _l_order      :: Stock
  , _complication :: Complication
  , _deliveryType :: DeliveryType
  } deriving Show

data BotState
  = OrderNow
  | Ordered
  | Waiting TrackingId
  | Received TrackingId
  | AwaitingLostParcel
  deriving (Show, Eq)

data Complication
  = NoComplication
  | ShippingLost
  deriving Show

data TODO
  = WakeUpBot OrderId
  | RestockStuff Stock
  | GiveTrackingId OrderId TrackingId
  deriving Show

data State
  = State
  { _botsmap       :: M.Map OrderId Bot
  , _shippingState :: ShippingState
  , _inventory     :: Stock
  , _todo          :: M.Map Day [TODO]
  , _stoday        :: Day
  , _usedTracking  :: S.Set TrackingId
  } deriving Show

makeLenses ''Bot
makeLenses ''State

newState :: [(DeliveryType, Complication)] -> Gen State
newState complications = do
  let rollBots :: (M.Map OrderId Bot, M.Map Day [TODO]) -> (DeliveryType, Complication) -> Gen (M.Map OrderId Bot, M.Map Day [TODO])
      rollBots (curbots, curtodo) (dtype, compl) = do
            wakeup <- Day <$> choose (0, 20)
            orderId <- rollOrderId (M.keysSet curbots)
            orderSize <- choose(0, 10)
            orderContent <- M.fromList <$> replicateM orderSize (genItem 10)
            pure
              ( M.insert orderId (Bot OrderNow orderContent compl dtype) curbots
              , M.insertWith (++) wakeup [WakeUpBot orderId] curtodo
              )
      rollOrderId taken = suchThat (OrderId <$> choose(0, 5000)) (`S.notMember` taken)
      genItem :: Int -> Gen (ItemId, Int)
      genItem mx = (\iid sz -> (ItemId iid, sz)) <$> choose(0, 100) <*> choose(1, mx)
  (botsMap, todoMap) <- foldM rollBots (M.empty, M.empty) complications
  inv <- M.fromList <$> replicateM 30 (genItem 40)
  pure (State botsMap (initialShippingState 0) inv todoMap 0 S.empty)
