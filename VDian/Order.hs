module VDian.Order
  ( module VDian.Order
  , module VDian.Types
  ) where

import           ClassyPrelude
import           Database.Persist.Sql    (PersistField(..), PersistFieldSql(..), SqlType(SqlString))
import           Data.Aeson
import           Data.Aeson.Types         (Pair)
import qualified Data.ByteString.Lazy     as LB
import           Data.Conduit
import           Data.Conduit.Combinators (yieldMany)
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T
import           Network.Wreq             (Response)

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
                  | OrderStatusRefundAfterAccept
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
fromOrderStatusStr "unship_refunding" = Just OrderStatusRefundUnship
fromOrderStatusStr "ship"             = Just OrderStatusShipped
fromOrderStatusStr "shiped_refunding" = Just OrderStatusRefundAfterShip
fromOrderStatusStr "accept"           = Just OrderStatusAccept
fromOrderStatusStr "finish"           = Just OrderStatusFinish
fromOrderStatusStr "close"            = Just OrderStatusClose
fromOrderStatusStr _                  = Nothing


toOrderStatusStr :: IsString s => OrderStatus -> Maybe s
toOrderStatusStr OrderStatusUnPaid          = Just "unpay"
toOrderStatusStr OrderStatusPaid            = Just "pay"
toOrderStatusStr OrderStatusRefundUnship    = Just "unship_refunding"
toOrderStatusStr OrderStatusShipped         = Just "ship"
toOrderStatusStr OrderStatusRefundAfterShip = Just "shiped_refunding"
toOrderStatusStr OrderStatusAccept          = Just "accept"
toOrderStatusStr OrderStatusFinish          = Just "finish"
toOrderStatusStr OrderStatusClose           = Just "close"
toOrderStatusStr _                          = Nothing


-- | 因为 OrderStatus 有时候表示为数字, 有时候表示为字串
-- 而且范围还不一样
-- 在我们数据库里保存时, 让能保存全部值,对应的 SQL 类型就只能是字串
instance PersistFieldSql OrderStatus where
  sqlType _ = SqlString

instance PersistField OrderStatus where
  toPersistValue x = toPersistValue $ fromMaybe (error $ "cannot represent: " <> show x) $
                      fmap tshow (toOrderStatusNum x) <|> toOrderStatusStr x

  fromPersistValue v = do
    t <- fromPersistValue v
    let m_n = readMay t >>= fromOrderStatusNum
    case m_n of
      Just x -> return x
      Nothing -> maybe (Left $ "cannot parse VD.OrderStatus: " <> t) return $
                    fromOrderStatusStr t

instance ToJSON OrderStatus where
  toJSON x = toJSON $ fromMaybe (error $ "cannot represent: " <> show x) $
                      fmap tshow (toOrderStatusNum x) <|> toOrderStatusStr x

instance FromJSON OrderStatus where
  parseJSON = withText "OrderStatus" $ \t -> do
    let m_n = readMay t >>= fromOrderStatusNum
    case m_n of
      Just x -> return x
      Nothing -> maybe (fail $ "cannot parse VD.OrderStatus: " <> T.unpack t) return $
                    fromOrderStatusStr t


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
           -> [ListOrderOpt]
           -> Int
           -> m (ApiCallResult ListOrders)
listOrders conf atk lot lo_opts page_num = do
  callMethod conf atk "vdian.order.list.get" ApiVersion1_1 params
  where
    params = HM.insert "page_num" (toJSON page_num) $
              HM.insert "order_type" (toJSON lot) $
              HM.fromList $ map mkListOrderOptParam lo_opts


-- | Wrap 'listOrders' in a conduit interface
sourceOrders :: (ApiCallMonad m)
             => VDianApiConfig
             -> AccessToken
             -> ListOrderType
             -> [ListOrderOpt]
             -> Source m SimpleOrderInfo
sourceOrders conf atk lot lo_opts = go 0 1
  where
    go done_cnt page_num = do
        ListOrders total_num infos <- lift (listOrders conf atk lot lo_opts page_num)
                                        >>= extractResultData
        yieldMany infos
        let done_cnt2 = done_cnt + length infos

        when ( done_cnt2 < total_num ) $ do
          go done_cnt2 (page_num + 1)


-- | 订单详情返回报文内容
data OrderDetails = OrderDetails
  { orderDetailsId          :: OrderId
  , orderDetailsBuyerInfo   :: BuyerInfo
  , orderDetailsItems       :: [OrderItem]

  , orderDetailsPrice       :: Price
  -- ^ 商品总价, 不含运费
  , orderDetailsTotalPrice  :: Price
  -- ^ 订单总价, 包含运费
  , orderDetailsShipFee     :: Price
  -- ^ 快递费用
  , orderDetailsPromoteFee  :: Price
  -- ^ 推广分成佣金
  -- 对应 JSON 里的 total_fee 字段
  -- 不知 total 为何意

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
                 <*> (o .: "express_fee" >>= parsePrice)
                 <*> (o .: "total_fee" >>= parsePrice)
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
  fst <$> getOrderDetails' conf atk order_id


getOrderDetails' :: ApiCallMonad m
                 => VDianApiConfig
                 -> AccessToken
                 -> OrderId
                 -> m (ApiCallResult OrderDetails, Response LB.ByteString)
getOrderDetails' conf atk order_id = do
  callMethod' conf atk "vdian.order.get" ApiVersion1_0 params
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
