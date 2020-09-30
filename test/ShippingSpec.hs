{-# LANGUAGE TupleSections #-}
module ShippingSpec (shippingSpec) where

import qualified Data.Map.Strict        as M
import           Test.Hspec
import           Test.Hspec.QuickCheck  (prop)
import           Test.QuickCheck

import           Shipping
import           ShippingBot.Definition (Complication (..))
import           ShippingBot.Prop       (propBot)

genStocks :: Gen (Stock, Stock)
genStocks = (,) <$> genStock <*> genStock

genStock :: Gen Stock
genStock = do
  keys <- suchThat (sublistOf [0..100]) (not . null)
  M.fromList <$> mapM (\k -> (ItemId k,) <$> elements[1..100] ) keys

shippingSpec :: Spec
shippingSpec = do
  let qstock = M.findWithDefault 0
  describe "increaseStock" $ do
    prop "additivity" $ forAll genStocks $ \(s1, s2) ->
      let s' = increaseStock s1 s2
      in  all (\k -> qstock k s1 + qstock k s2 == qstock k s') (M.keys s1)
    prop "non extra" $ forAll genStocks $ \(s1, s2) ->
      forAll (elements (M.keys s1)) $ \k ->
        let s1' = M.delete k s1
            s2' = M.delete k s2
            s'  = increaseStock s1' s2'
        in  qstock k s' == 0
  prop "decreaseStock" $ forAll genStocks $ \(s1, s2) ->
    let diff = M.unionWith (+) s1 (fmap negate s2)
    in case decreaseStock s1 s2 of
      Nothing -> any (< 0) diff
      Just r  -> all (> 0) r && r == M.filter (/= 0) diff
  describe "initialShippingState" $
    prop "is today" (\n -> let d = Day n in getDay (initialShippingState d) == d)
  let istate = initialShippingState 0
      order0 = OrderId 0
      order1 = OrderId 1
      order2 = OrderId 2
      istock = M.fromList [(1, 10), (2, 5)] :: Stock
      trackingA = TrackingId 1233145

  describe "order" $ do
    it "will work when stock is available" $ do
      let (stock', _, events) = order order0 (OrderInformation (M.singleton 1 5) "" Standard) istock istate
      stock' `shouldBe` M.fromList [(1, 5), (2, 5)]
      events `shouldBe` Just (ShippingInformation "" order0 Standard)
    it "will wait when stock is not available" $ do
      let (stock', stt', events) = order order0 (OrderInformation (M.singleton 2 10) "" Standard) istock istate
      stock' `shouldBe` istock
      events `shouldBe` Nothing
      getWaitingOrders stt' `shouldBe` [order0]
    describe "initialShippingState order" $ do
      it "has no waiting orders" $ getWaitingOrders istate `shouldBe` []
      it "has no orders waiting for a tracking number" $ getWaitingTracking istate `shouldBe` []
      it "has no orders waiting for pickup" $ getWaitingPickup istate `shouldBe` []


  describe "restock" $ do
    describe "initialShippingState restock" $ do
      it "has no orders in transit" $ getInTransit istate `shouldBe` []
      it "has no received orders" $ getReceived istate `shouldBe` []

    it "does nothing in particular when there are no orders waiting for stocks" $ do
      let nstock = M.singleton 3 10
          (stock', _, infos) = restock istock nstock istate
      infos `shouldBe` []
      stock' `shouldBe` increaseStock istock nstock
    it "serves waiting orders, scenario 1" $ do
      let nstock = M.singleton 3 10
          (_, stt_wait1, msg_wait1) = order order0 (OrderInformation (M.singleton 3 5) "" Standard) istock istate
      msg_wait1 `shouldBe` Nothing
      let (stock', _, events) = restock istock nstock stt_wait1
      events `shouldBe` [ShippingInformation "" order0 Standard]
      stock' `shouldBe` M.fromList [(1, 10), (2, 5), (3, 5)]
    let (_, stt_wait1, msg_wait1) = order order0 (OrderInformation (M.singleton 3 5) "" Standard) istock istate
        (_, stt_wait2, msg_wait2) = order order1 (OrderInformation (M.singleton 2 10) "" Standard) istock stt_wait1
        (_, stt_wait3, msg_wait3) = order order2 (OrderInformation (M.singleton 2 7) "" Standard) istock stt_wait2
    it "sanity testing" $ do
      msg_wait1 `shouldBe` Nothing
      msg_wait2 `shouldBe` Nothing
      msg_wait3 `shouldBe` Nothing
    it "serves waiting orders, scenario 2" $ do
      let (stock', _, events) = restock istock (M.singleton 3 10) stt_wait3
      events `shouldBe` [ShippingInformation "" order0 Standard]
      stock' `shouldBe` M.fromList [(1, 10), (2, 5), (3, 5)]
    it "serves waiting orders, scenario 3" $ do
      let (stock', _, events) = restock istock (M.singleton 2 20) stt_wait3
      events `shouldBe` [ShippingInformation "" order1 Standard, ShippingInformation "" order2 Standard]
      stock' `shouldBe` M.fromList [(1, 10), (2, 8)]
    it "serves waiting orders, scenario 4" $ do
      let (stock', _, events) = restock istock (M.singleton 2 2) stt_wait3
      events `shouldBe` [ShippingInformation "" order2 Standard]
      stock' `shouldBe` M.fromList [(1, 10)]
    it "serves waiting orders, scenario 5" $ do
      let (stock', _, events) = restock istock (M.singleton 2 5) stt_wait3
      events `shouldBe` [ShippingInformation "" order1 Standard]
      stock' `shouldBe` M.fromList [(1, 10)]

  describe "full scenarios" $ do
    describe "standard order" $ do
      let (stock_1, stt_1, msg_1) = handleMessages istock istate [NewOrder order0 (OrderInformation (M.singleton 2 3) "foo" Standard), NewDay]
      let (stock_2, stt_2, msg_2) = handleMessages stock_1 stt_1 [ParcelHandled order0 trackingA]
      let (stock_3, stt_3, msg_3) = handleMessages stock_2 stt_2 (replicate 6 NewDay)
      let (_      , stt_4, msg_4) = handleMessages stock_3 stt_3 [NewDay]
      it "initial order" $ do
        msg_1 `shouldBe` [Ship (ShippingInformation "foo" order0 Standard)]
        getWaitingTracking stt_1 `shouldBe` [(order0, 0)]
        getDay stt_1 `shouldBe` 1
      it "tracking order received" $ do
        msg_2 `shouldBe` []
        getWaitingTracking stt_2 `shouldBe` []
        getInTransit stt_2 `shouldBe` [(order0, 1)]
      it "is considered not received after 6 days" $ do
        msg_3 `shouldBe` []
        getInTransit stt_3 `shouldBe` [(order0, 1)]
        getDay stt_3 `shouldBe` 7
      it "is considered received after 7 days" $ do
        msg_4 `shouldBe` []
        getInTransit stt_4 `shouldBe` []
        getDay stt_4 `shouldBe` 8
    describe "tracked order" $ do
      let (stock_1, stt_1, msg_1) = handleMessages istock istate [NewOrder order0 (OrderInformation (M.singleton 2 3) "foo" Verified), NewDay]
      let (stock_2, stt_2, msg_2) = handleMessages stock_1 stt_1 [ParcelHandled order0 trackingA]
      let (stock_3, stt_3, msg_3) = handleMessages stock_2 stt_2 (replicate 8 NewDay)
      let (_      , stt_4, msg_4) = handleMessages stock_3 stt_3 [NewDay]
      it "initial order" $ do
        msg_1 `shouldBe` [Ship (ShippingInformation "foo" order0 Verified)]
        getWaitingTracking stt_1 `shouldBe` [(order0, 0)]
        getDay stt_1 `shouldBe` 1
      it "tracking order received" $ do
        msg_2 `shouldBe` []
        getWaitingTracking stt_2 `shouldBe` []
        getInTransit stt_2 `shouldBe` [(order0, 1)]



  describe "simulations" $ do
    propBot "no bot" []
    propBot "simple bot, standard delivery" [(Standard, NoComplication)]
    propBot "simple bot, verified delivery" [(Verified, NoComplication)]
    propBot "simple bot, maildrop" [(MailDrop, NoComplication)]
