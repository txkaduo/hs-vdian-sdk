module VDian.Order
  ( module VDian.Order
  , module VDian.Types
  ) where

import           ClassyPrelude
import           Data.Aeson
import           Data.Aeson.Types    (Pair)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

import           VDian.Method
import           VDian.Types


-- | 特别用在订单列表接口中的订单类型入参
data ListOrderType = ListOrderUnpaid
                   | ListOrderToShip
                   | ListOrderRefunding
                   | ListOrderClosed
                   | ListOrderFinished
                   deriving (Show, Eq, Ord, Enum, Bounded)

instance ToJSON ListOrderType where
  toJSON ListOrderUnpaid    = "unpay"
  toJSON ListOrderToShip    = "pend"
  toJSON ListOrderRefunding = "refund"
  toJSON ListOrderClosed    = "close"
  toJSON ListOrderFinished  = "finish"

data ListOrderOpt = ListOrderAddStart UTCTime
                  | ListOrderAddEnd UTCTime
                  | ListOrderUpdateStart UTCTime
                  | ListOrderUpdateEnd UTCTime
                  | ListOrderPageSize Int
                  deriving (Show, Eq, Ord)

mkListOrderOptParam :: ListOrderOpt -> Pair
mkListOrderOptParam (ListOrderAddStart t)    = "add_start"    .= VDianTime t
mkListOrderOptParam (ListOrderAddEnd t)      = "add_end"      .= VDianTime t
mkListOrderOptParam (ListOrderUpdateStart t) = "update_start" .= VDianTime t
mkListOrderOptParam (ListOrderUpdateEnd t)   = "update_end"   .= VDianTime t
mkListOrderOptParam (ListOrderPageSize t)    = "page_size"    .= t


-- | 订单状态在不同的接口(列表, 详情)下有不同的取值范围, 还有不同的表示方式
-- 有时候用数字, 有时候用字串
-- 还有, 列表, 详情两个接口返回的订单状态取值范围不一样
data OrderStatus = OrderStatusJustCreated
                  | OrderStatusUnPaid
                  | OrderStatusPaid
                  | OrderStatusShipped
                  | OrderStatusRefund
                    -- ^ 列表接口返回的状态 7, 但不清楚与详情接口的状态有什么对应关系
                  | OrderStatusRefundUnship
                  | OrderStatusRefundAfterShip
                  | OrderStatusRefundAfterAccecpt
                  | OrderStatusAccept
                  | OrderStatusFinish
                  | OrderStatusClose
                  deriving (Show, Eq, Ord, Enum, Bounded)

fromOrderStatusNum :: Int -> Maybe OrderStatus
fromOrderStatusNum 0 = Just OrderStatusJustCreated
fromOrderStatusNum 1 = Just OrderStatusUnPaid
fromOrderStatusNum 2 = Just OrderStatusPaid
fromOrderStatusNum 3 = Just OrderStatusShipped
fromOrderStatusNum 7 = Just OrderStatusRefund
fromOrderStatusNum 8 = Just OrderStatusClose
fromOrderStatusNum _ = Nothing

fromOrderStatusNum' :: String -> Maybe OrderStatus
fromOrderStatusNum' s = readMay s >>= fromOrderStatusNum

toOrderStatusNum :: OrderStatus -> Maybe Int
toOrderStatusNum OrderStatusJustCreated = Just 0
toOrderStatusNum OrderStatusUnPaid      = Just 1
toOrderStatusNum OrderStatusPaid        = Just 2
toOrderStatusNum OrderStatusShipped     = Just 3
toOrderStatusNum OrderStatusRefund      = Just 7
toOrderStatusNum OrderStatusClose       = Just 8
toOrderStatusNum _                      = Nothing

-- | 解释订单详情的状态字串
fromOrderStatusStr :: Text -> Maybe OrderStatus
fromOrderStatusStr "unpay"            = Just OrderStatusUnPaid
fromOrderStatusStr "pay"              = Just OrderStatusPaid
fromOrderStatusStr "unship_refunding" = Just OrderStatusPaid
fromOrderStatusStr "ship"             = Just OrderStatusShipped
fromOrderStatusStr "shiped_refunding" = Just OrderStatusRefundAfterShip
fromOrderStatusStr "accept"           = Just OrderStatusAccept
fromOrderStatusStr "finish"           = Just OrderStatusFinish
fromOrderStatusStr "close"            = Just OrderStatusClose
fromOrderStatusStr _                  = Nothing


-- | 订单列表返回的订单信息
data SimpleOrderInfo = SimpleOrderInfo
  { simpleOrderBuyerInfo    :: BuyerInfo
  , simpleOrderOrderId      :: OrderId
  , simpleOrderStatus       :: OrderStatus
  , simpleOrderStatusText   :: Text
  , simpleOrderCreatedTime  :: VDianTime
  , simpleOrderUpdatedTime  :: VDianTime
  , simpleOrderImgUrl       :: Text
  , simpleOrderBuyerNote    :: Maybe Text
  , simpleOrderSellerNote   :: Maybe Text
  , simpleOrderExpressNo    :: Maybe Text
  , simpleOrderExpressType  :: Maybe Text
  }

instance FromJSON SimpleOrderInfo where
  parseJSON = withObject "SimpleOrderInfo" $ \o -> do
    SimpleOrderInfo <$> o .: "buyer_info"
                    <*> o .: "order_id"
                    <*> ( o .: "status" >>= (maybe mzero return . fromOrderStatusNum') )
                    <*> o .: "status2"
                    <*> o .: "time"
                    <*> o .: "update_time"
                    <*> o .: "img"
                    <*> o .:? "buyer_note"
                    <*> o .:? "seller_note"
                    <*> o .:? "express_no"
                    <*> o .:? "express_type"

instance ToJSON SimpleOrderInfo where
  toJSON order =
    object
    [ "buyer_info"   .= simpleOrderBuyerInfo order
    , "order_id"     .= simpleOrderOrderId order
    , "status"       .= toOrderStatusNum (simpleOrderStatus order)
    , "status2"      .= simpleOrderStatusText order
    , "time"         .= simpleOrderCreatedTime order
    , "update_time"  .= simpleOrderUpdatedTime order
    , "img"          .= simpleOrderImgUrl order
    , "buyer_note"   .= simpleOrderBuyerNote order
    , "seller_note"  .= simpleOrderSellerNote order
    , "express_no"   .= simpleOrderExpressNo order
    , "express_type" .= simpleOrderExpressType order
    ]


-- | 订单列表接口的返回报文内容
data ListOrders = ListOrders
                    Int
                    [SimpleOrderInfo]

instance FromJSON ListOrders where
  parseJSON = withObject "ListOrders" $ \o -> do
    ListOrders <$> o .: "total_num"
               <*> o .: "orders"

instance ToJSON ListOrders where
  toJSON (ListOrders total orders) =
    object
    [ "total_num" .= total
    , "orders"    .= orders
    ]

listOrders :: (ApiCallMonad m)
           => VDianApiConfig
           -> AccessToken
           -> ListOrderType
           -> Int
           -> [ListOrderOpt]
           -> m (ApiCallResult ListOrders)
listOrders conf atk lot page_num lo_opts = do
  callMethod conf atk "vdian.order.list.get" ApiVersion1_1 params
  where
    params = HM.insert "page_num" (toJSON page_num) $
              HM.insert "order_type" (toJSON lot) $
              HM.fromList $ map mkListOrderOptParam lo_opts


-- | 订单详情返回报文内容
data OrderDetails = OrderDetails
  { orderDetailsId          :: OrderId
  , orderDetailsBuyerInfo   :: BuyerInfo
  , orderDetailsItems       :: [OrderItem]
  , orderDetailsPrice       :: Price
  , orderDetailsTotalPrice  :: Price
  , orderDetailsStatus      :: OrderStatus
  , orderDetailsStatusText  :: Text
  , orderDetailsStatusDesc  :: Text
  , orderDetailsUserPhone   :: Text
  , orderDetailsBuyerNote   :: Maybe Text
  , orderDetailsCreatedTime :: VDianTime
  , orderDetailsPaidTime    :: Maybe VDianTime
  , orderDetailsShippedTime :: Maybe VDianTime
  }

instance FromJSON OrderDetails where
  parseJSON = withObject "OrderDetails" $ \o -> do
    OrderDetails <$> fmap OrderId (o .: "order_id" >>= parseStrInt)
                 <*> o .: "buyer_info"
                 <*> o .: "items"
                 <*> (o .: "price" >>= parsePrice)
                 <*> (o .: "total" >>= parsePrice)
                 <*> (o .: "status" >>= maybe mzero return . fromOrderStatusStr)
                 <*> o .: "status2"
                 <*> o .: "status_desc"
                 <*> o .: "user_phone"
                 <*> o .:? "note"
                 <*> o .: "add_time"
                 <*> (fmap nullIfEmptyString (o .: "pay_time") >>= parseJSON)
                 <*> (fmap nullIfEmptyString (o .: "send_time") >>= parseJSON)


getOrderDetails :: ApiCallMonad m
                => VDianApiConfig
                -> AccessToken
                -> OrderId
                -> m (ApiCallResult OrderDetails)
getOrderDetails conf atk order_id = do
  callMethod conf atk "vdian.order.get" ApiVersion1_0 params
  where
    params = HM.fromList [ "order_id" .= tshow (unOrderId order_id) ]



-- | 消息推送的类型
data OrderEventMsgType = OrderEvtMsgTypeClosed
                        | OrderEvtMsgTypeAwaitPayment
                        | OrderEvtMsgTypePaid
                        | OrderEvtMsgTypeShipped
                        | OrderEvtMsgTypeConfirmReceipt
                        | OrderEvtMsgTypeSellerSubmitRefund
                        | OrderEvtMsgTypeBuyersComplaints
                        | OrderEvtMsgTypeComplaintsProcessed
                        | OrderEvtMsgTypeBuyerSubmitRefund
                        | OrderEvtMsgTypeSellerRefuseRefund
                        | OrderEvtMsgTypeRefundSuccess
                        | OrderEvtMsgTypeFinished
                        deriving (Show, Eq, Ord, Enum, Bounded)


orderEventMsgTypeToStr :: OrderEventMsgType -> Text
orderEventMsgTypeToStr OrderEvtMsgTypeClosed              = "weidian.order.close"
orderEventMsgTypeToStr OrderEvtMsgTypeAwaitPayment        = "weidian.order.non_payment"
orderEventMsgTypeToStr OrderEvtMsgTypePaid                = "weidian.order.already_payment"
orderEventMsgTypeToStr OrderEvtMsgTypeShipped             = "weidian.order.delivered"
orderEventMsgTypeToStr OrderEvtMsgTypeConfirmReceipt      = "weidian.order.confirm_eceipt"
orderEventMsgTypeToStr OrderEvtMsgTypeSellerSubmitRefund  = "weidian.order.seller_submit_refund"
orderEventMsgTypeToStr OrderEvtMsgTypeBuyersComplaints    = "weidian.order.buyers_complaints"
orderEventMsgTypeToStr OrderEvtMsgTypeComplaintsProcessed = "weidian.order.complaints_have_been_processed"
orderEventMsgTypeToStr OrderEvtMsgTypeBuyerSubmitRefund   = "weidian.order.buyers_apply_refund"
orderEventMsgTypeToStr OrderEvtMsgTypeSellerRefuseRefund  = "weidian.order.sellers_refused_refund"
orderEventMsgTypeToStr OrderEvtMsgTypeRefundSuccess       = "weidian.order.refund_success"
orderEventMsgTypeToStr OrderEvtMsgTypeFinished            = "weidian.order.finished"


orderEventMsgTypeFromStr :: Text -> Maybe OrderEventMsgType
orderEventMsgTypeFromStr "weidian.order.close"                          = Just OrderEvtMsgTypeClosed
orderEventMsgTypeFromStr "weidian.order.non_payment"                    = Just OrderEvtMsgTypeAwaitPayment
orderEventMsgTypeFromStr "weidian.order.already_payment"                = Just OrderEvtMsgTypePaid
orderEventMsgTypeFromStr "weidian.order.delivered"                      = Just OrderEvtMsgTypeShipped
orderEventMsgTypeFromStr "weidian.order.confirm_eceipt"                 = Just OrderEvtMsgTypeConfirmReceipt
orderEventMsgTypeFromStr "weidian.order.confirm_receipt"                = Just OrderEvtMsgTypeConfirmReceipt
orderEventMsgTypeFromStr "weidian.order.seller_submit_refund"           = Just OrderEvtMsgTypeSellerSubmitRefund
orderEventMsgTypeFromStr "weidian.order.buyers_complaints"              = Just OrderEvtMsgTypeBuyersComplaints
orderEventMsgTypeFromStr "weidian.order.complaints_have_been_processed" = Just OrderEvtMsgTypeComplaintsProcessed
orderEventMsgTypeFromStr "weidian.order.buyers_apply_refund"            = Just OrderEvtMsgTypeBuyerSubmitRefund
orderEventMsgTypeFromStr "weidian.order.sellers_refused_refund"         = Just OrderEvtMsgTypeSellerRefuseRefund
orderEventMsgTypeFromStr "weidian.order.refund_success"                 = Just OrderEvtMsgTypeRefundSuccess
orderEventMsgTypeFromStr "weidian.order.finished"                       = Just OrderEvtMsgTypeFinished
orderEventMsgTypeFromStr _                                              = Nothing


instance ToJSON OrderEventMsgType where
  toJSON = toJSON . orderEventMsgTypeToStr

instance FromJSON OrderEventMsgType where
  parseJSON = withText "OrderEventMsgType" $ \t -> do
                maybe
                  (fail $ T.unpack $ "unknown message type '" <> t <> "'")
                  return
                  (orderEventMsgTypeFromStr t)


-- | 订单消息推送报文
data PushedOrderMsg = PushedOrderMsg
                        (Either Text OrderEventMsgType)
                        OrderDetails

instance FromJSON PushedOrderMsg where
  parseJSON = withObject "PushedOrderMsg" $ \o -> do
    PushedOrderMsg <$> (fmap Right (o .: "type") <|> fmap Left (o .: "type"))
                   <*> o .: "message"
