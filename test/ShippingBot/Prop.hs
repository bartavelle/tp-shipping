module ShippingBot.Prop where

import           Shipping
import           ShippingBot.Definition

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, evalStateT, get)
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Lens.Micro.Platform
import           System.Random              (Random (randomIO, randomRIO))
import           Test.Hspec                 (Spec, shouldBe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (forAll)

propBot :: String -> [(DeliveryType, Complication)] -> Spec
propBot desc bots = prop desc $ forAll (newState bots) (evalStateT simulate)

type BOT a = StateT State IO a

getTracking :: BOT TrackingId
getTracking = do
  roll <- TrackingId <$> liftIO randomIO
  used <- use usedTracking
  if S.member roll used
    then getTracking
    else do
      usedTracking %= S.insert roll
      pure roll

simulate :: BOT ()
simulate = do
  today <- use stoday
  todo_today <- use (todo . ix today)
  tosend <- (++[NewDay]) . concat <$> mapM runTodo todo_today
  stt' <- get
  let (newstock, newustate, outevents) = handleMessages (stt' ^. inventory) (stt' ^. shippingState) tosend
      curday = getDay newustate
  liftIO (curday `shouldBe` today + 1)
  inventory .= newstock
  shippingState .= newustate
  todo %= M.filterWithKey (\k _ -> k > today)
  stoday .= curday
  mapM_ handleOutevents outevents
  todo_now <- use todo
  if null todo_now
    then finalCheck
    else simulate

finalCheck :: BOT ()
finalCheck = do
  let checkBot orderid bot =
        case _behaviour bot of
          Received _ -> pure ()
          _          -> fail ("Invalid state for bot " ++ show orderid ++ ": " ++ show bot)
  use botsmap >>= M.traverseWithKey checkBot
  stt' <- get
  let (_, sstate, outevents) = handleMessages (stt' ^. inventory) (stt' ^. shippingState) (replicate 8 NewDay)
  liftIO $ do
    outevents `shouldBe` []
    getWaitingOrders sstate `shouldBe` []
    getWaitingTracking sstate `shouldBe` []
    getWaitingPickup sstate `shouldBe` []
    getReceived sstate `shouldBe` []
    getInTransit sstate `shouldBe` []

getBot :: String -> OrderId -> BOT Bot
getBot msg oid = do
  r <- preuse (botsmap . ix oid)
  case r of
    Nothing -> fail ("Could not get bot in " ++ msg ++ " " ++ show oid)
    Just b  -> pure b

rollDelay :: (Int, Int) -> BOT Day
rollDelay range = Day <$> liftIO (randomRIO range)

queueTODO :: Day -> TODO -> BOT ()
queueTODO delay stuff = do
  now <- use stoday
  todo %= M.insertWith (++) (now + delay) [stuff]

handleOutevents :: OutEvent -> BOT ()
handleOutevents event
  = case event of
      Ship (ShippingInformation orderid (Destination _ deliverytype)) -> do
        bot <- getBot "Ship" orderid
        case bot ^. behaviour of
          Ordered -> do
            liftIO (bot ^. deliveryType `shouldBe` deliverytype)
            tracking <- getTracking
            delay <- rollDelay (1, 3)
            queueTODO delay (GiveTrackingId orderid tracking)
          b -> fail ("Spurious " ++ show event ++ ", bot is in state " ++ show b)
      OutofstockMessage orderid -> do
        bot <- getBot "OutofstockMessage" orderid
        delay <- rollDelay (1, 5)
        items <- traverse (\q -> (+ q) <$> liftIO (randomRIO (0, 3))) (bot ^. l_order)
        queueTODO delay (RestockStuff items)
      InconsistentState err -> fail err

runTodo :: TODO -> BOT [InEvent]
runTodo td = case td of
  RestockStuff newstock -> pure [Restock newstock]
  WakeUpBot oid -> do
      Bot bstate orderContent compl dtype <- getBot "Bot" oid
      case bstate of
        OrderNow -> do
          botsmap . ix oid . behaviour .= Ordered
          pure [NewOrder oid (OrderInformation orderContent (Destination "xx" dtype))]
        Waiting _ ->
          case compl of
            NoComplication -> do
              -- packet received, forget about it
              botsmap %= M.delete oid
              pure []
            _ -> fail ("Unhandled complication " ++ show compl)
        _ -> fail ("Unhandled " ++ show bstate)
  GiveTrackingId oid tid -> do
    _ <- getBot "GiveTrackingId" oid
    delay <- rollDelay (1, 4)
    queueTODO delay (WakeUpBot oid)
    botsmap . ix oid . behaviour .= Waiting tid
    pure [ParcelHandled oid tid]
