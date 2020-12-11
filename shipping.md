# shipping assignment specification

You are in charge of an e-commerce site shipping division.
You must manage your stock and orders baded on messages received from your customers and from the shipping company.

The stock is represented as a `Map ItemId Int`, where the key is the item identification number, and the value is the quantity of items in stock.

  * invariant 1 : all quantities must be strictly positive. That means that there are no items with a 0 quantity. If an item stock becomes depleted, you must remove it from the map entirely.

The goal of this assignment is to write the function `handleMessage`, by writing several subfunctions and completing the stub in `Shipping.hs`.


The are the events you must handle:

```haskell
data InEvent
  = NewOrder OrderId OrderInformation
  | ParcelHandled OrderId TrackingId
  | ParcelReturned TrackingId
  | ParcelDelivered TrackingId
  | NewDay
  | Restock Stock
  deriving (Show, Eq)
```

For each message, you must update the current stock, keep a `ShippingState` that holds data about waiting orders, orders being shipped, etc. and return a list of `OutEvent`:

```haskell
handleMessage
  :: Stock -- ^ initial stock before message handling
  -> ShippingState -- ^ shipping state after message handling
  -> InEvent
  -> (Stock, ShippingState, [OutEvent]) -- ^ updated states, with messages
```

## The `ShippingState`

This structure is yours to make. It should probably hold:

 * the current day,
 * the list of orders for which there is no stock available,
 * the shipped orders, along with their tracking information

So, something like:

```haskell
data ShippingState
    = ShippingState
    { _today :: Day
    , _waitingForStocks :: [WaitingForStockOrders?]
    , _orders :: M.Map SomeKey SomeValue
    }
```

## Input events

### `NewOrder` (handled in the `order` function)

Parameters:
  * `OrderId` : the order id
  * `OrderInformation` : order information, including the ordered stock, and `DeliveryType`, which is important later!

When receiving a new order, you should check if there is enough stock to honor it *completely*.
No partial orders are ever sent!

When the order *can* be honored, you must:
 - update the stock accordingly (removing all sent merchandise)
 - send a `Ship` message, and update the `ShippingState` to remember you are waiting for a `TrackingId` (see `ParcelHandled`)
When the order *can't* be honored, you must:
 - store the order so that it can be sent later, when there is stock available
 - send an `OutOfOrder` message

NOTE: if you implement the `order` function properly, you do not have to worry about sending messages.

### `Restock` (handled in the `restock` function)

Parameter:

 * `Stock` : amount of merchandises to add to the stock

When receiving new stock, you must:
 - update your stock, adding the new items
 - for each waiting order (orders that were out of stock), in the order they were received,
   you must check if there is now stock for them.
    * if there is stock, you must behave just like if it was a `NewOrder`
    * otherwise, you must not do anything special

### `ParcelHandled`

Parameter:

 * `OrderId` : the order id that has been handled
 * `TrackingId` : the tracking id associated with this parcel

A parcel is being sent by the shipping company. From now on, the messages will be identified with the `TrackingId` parameter.
You must mark the parcel as being shipped, instead of waiting for a tracking id.

### `ParcelDelivered`

Parameter:

 * `TrackingId` : the tracking id associated with this parcel

The parcel has been delivered. You must delete all references of this parcel from the `ShippingState`.

### `NewDay`

No parameters.

A day has passed. You must check for all possible expirations (see next section).

### `ParcelReturned`

Parameter:

 * `TrackingId` : the tracking id associated with this parcel

The parcel has not been delivered. Add stock back and forget about this order.

Note: this is an optional assignment.

## Expirations and stuff to wait for

On receiving the `NewDay` message, several things must be checked:

 * requests for `TrackingId` that have been waiting for 5 days must be sent again ;
 * for shipped items, after 7 days without receiving the `ParcelDelivered` message, you must:

   - for "standard delivery" items, assume they have been received, just like if you received the `ParcelDelivered` message
   - for "verified delivery" items, you must act as if you received a new `Order` message