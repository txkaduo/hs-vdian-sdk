module VDian.Types where

import           ClassyPrelude
import           Control.Monad.Logger
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson
import qualified Data.Aeson.Types      as A
import           Data.Default          (Default(..))
import qualified Data.Scientific       as SC
import qualified Data.Text             as T
import           Data.Time             ( NominalDiffTime, hoursToTimeZone
                                       , utcToLocalTime, localTimeToUTC )
import           Data.Time.Format      (parseTimeM)
import           Database.Persist.Sql  (PersistField (..), PersistFieldSql (..))
import           Text.Blaze.Html       (ToMarkup (..))
import           Text.Shakespeare.I18N (ToMessage (..))


#define INT_FROM_JSON_INST(T) instance FromJSON T where { parseJSON = fmap T . parseStrInt }

#define INT_TO_JSON_INST(T) instance ToJSON T where { toJSON (T x) = toJSON x }


data VDianApiConfig = VDianApiConfig
  { vdianApiUrlBase :: String
  }

instance Default VDianApiConfig where
  def = VDianApiConfig "https://api.vdian.com"


-- | 接口格式
data ApiFormat = ApiFormatJson
                deriving (Show, Eq, Ord, Enum, Bounded)

instance ToJSON ApiFormat where
  toJSON ApiFormatJson = toJSON $ asText "json"


-- | 接口版本
data ApiVersion = ApiVersion1_0
                | ApiVersion1_1
                deriving (Show, Eq, Ord, Enum, Bounded)

instance ToJSON ApiVersion where
  toJSON ApiVersion1_0 = toJSON $ asText "1.0"
  toJSON ApiVersion1_1 = toJSON $ asText "1.1"


newtype AccessToken = AccessToken { unAccessToken :: Text }
  deriving ( Show, Eq, Ord, FromJSON, ToJSON, PersistField, PersistFieldSql
           , ToMessage, ToMarkup)

newtype AppKey = AppKey { unAppKey :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, PersistField, PersistFieldSql
           , ToMessage, ToMarkup)

newtype AppSecret = AppSecret { unAppSecret :: Text }
  deriving ( Show, Eq, Ord, FromJSON, ToJSON, PersistField, PersistFieldSql
           , ToMessage, ToMarkup)


type StatusCode = Int

data ApiCallStatus = ApiCallStatus
  { apiCallStatusCode   :: StatusCode
  , apiCallStatusReason :: Text
  }

instance FromJSON ApiCallStatus where
  parseJSON = withObject "ApiCallStatus" $ \o -> do
    ApiCallStatus <$> o .: "status_code"
                  <*> o .: "status_reason"

data ApiCallResult a = ApiCallResult
                        { apiCallResultStatus :: ApiCallStatus
                        , apiCallResultData   :: Maybe a
                        }

instance FromJSON a => FromJSON (ApiCallResult a) where
  parseJSON = withObject "ApiCallResult" $ \o -> do
    ApiCallResult <$> o .: "status"
                  <*> o .:? "result"


-- | 所有接口调用中都有的公有参数
data ApiPublicParam = ApiPublicParam
  { apiPubParamMethod       :: Text
  , apiPubParamAccessToken  :: AccessToken
  , apiPubParamFormat       :: ApiFormat
  , apiPubParamVersion      :: ApiVersion
  }
  deriving (Show)

instance ToJSON ApiPublicParam where
  toJSON (ApiPublicParam method atk format ver) =
    object
      [ "method"        .= method
      , "access_token"  .= atk
      , "format"        .= format
      , "version"       .= ver
      ]

type ApiPrivateParam = HashMap Text Value


type ApiCallMonad m = (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadLogger m)


data GetAccessTokenInfo = GetAccessTokenInfo
  { getAtkToken     :: AccessToken
  , getAtkExpireIn  :: NominalDiffTime
  }

instance FromJSON GetAccessTokenInfo where
  parseJSON = withObject "GetAccessTokenInfo" $ \o -> do
    GetAccessTokenInfo <$> o .: "access_token"
                       <*> fmap ( fromIntegral :: Int64 -> _) (o .: "expire_in")


data ApiCallException = ApiCallException StatusCode Text
  deriving (Show)

instance Exception ApiCallException


data ApiCallNoData = ApiCallNoData
  deriving (Show)

instance Exception ApiCallNoData


-- | 取订单列表时的 order_type 参数
data OrderType = OrderUnpaid
                | OrderPending
                | OrderRefund
                | OrderClose
                | OrderFinish
                deriving (Show, Eq, Ord, Enum, Bounded)

instance ToJSON OrderType where
  toJSON OrderUnpaid  = toJSON $ asText "unpay"
  toJSON OrderPending = toJSON $ asText "pend"
  toJSON OrderRefund  = toJSON $ asText "refund"
  toJSON OrderClose   = toJSON $ asText "close"
  toJSON OrderFinish  = toJSON $ asText "finish"


-- | 时间参数的格式
newtype VDianTime = VDianTime { unVDianTime :: UTCTime }
  deriving (Show, Eq, Ord)

instance ToJSON VDianTime where
  toJSON (VDianTime t) = toJSON $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lt
    where
      lt = utcToLocalTime tz t
      tz = hoursToTimeZone 8

instance FromJSON VDianTime where
  parseJSON = withText "VDianTime" $ \t -> do
    fmap (VDianTime . localTimeToUTC tz) $
      parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack t)
    where
      tz = hoursToTimeZone 8


-- | 订单号
-- 文档说是字串, 接口返回时也是字串格式, 但实际上看上去总是个数字
newtype OrderId = OrderId { unOrderId :: Int64 }
  deriving ( Show, Eq, Ord, PersistField, PersistFieldSql, ToMarkup)

INT_FROM_JSON_INST(OrderId)
INT_TO_JSON_INST(OrderId)


-- | 微店的用户ID
-- 文档说是字串, 接口返回时也是字串格式, 但实际上看上去总是个数字
newtype VDianUserId = VDianUserId { unVDianUserId :: Int64 }
  deriving ( Show, Eq, Ord, PersistField, PersistFieldSql, ToMarkup)

INT_FROM_JSON_INST(VDianUserId)
INT_TO_JSON_INST(VDianUserId)


-- | 买家对象
data BuyerInfo = BuyerInfo
  { buyerUserId :: Maybe VDianUserId
    -- ^ BuyerInfo 这个结构出现在两个接口的返回值中
    -- 在订单列表接口中, 文档没有说会返回 buyer_id
    -- 实际测试, 有返回这个字段, 但其实是 null
    -- 而在订单详情接口中, 如文档所述, buyer_id 有返回
    -- 为减少代码的重复, 这里只定义一种BuyerInfo数据类型
    -- 所以这个字段就只能是个 Maybe
  , buyerName   :: Maybe Text
  , buyerPhone  :: Maybe Text
  }

instance FromJSON BuyerInfo where
  parseJSON = withObject "BuyerInfo" $ \o -> do
    BuyerInfo <$> o .: "buyer_id"
              <*> o .: "name"
              <*> o .: "phone"

instance ToJSON BuyerInfo where
  toJSON bi =
    object
    [ "buyer_id" .= buyerUserId bi
    , "name"     .= buyerName bi
    , "phone"    .= buyerPhone bi
    ]


-- | 订单商品的ID
-- 文档说是字串, 接口返回时也是字串格式, 但实际上看上去总是个数字
newtype OrderItemId = OrderItemId { unOrderItemId :: Int64 }
  deriving ( Show, Eq, Ord, PersistField, PersistFieldSql, ToMarkup)

INT_FROM_JSON_INST(OrderItemId)
INT_TO_JSON_INST(OrderItemId)


type Price = Float


-- | SKU ID
-- 文档说是字串, 接口返回时也是字串格式, 但实际上看上去总是个数字
newtype SkuId = SkuId { unSkuId :: Int64 }
  deriving ( Show, Eq, Ord, PersistField, PersistFieldSql, ToMarkup)

INT_FROM_JSON_INST(SkuId)
INT_TO_JSON_INST(SkuId)


-- | 订单详情里的商品信息
data OrderItem = OrderItem
  { orderItemItemId          :: OrderItemId
  , orderItemPrice           :: Price
  , orderItemQuantity        :: Int
  , orderItemTotalPrice      :: Price
  , orderItemName            :: Text
  , orderItemImgUrl          :: Text
  , orderItemUrl             :: Text
  , orderItemMerchantCode    :: Maybe Text
  , orderItemSkuMerchantCode :: Maybe Text
  , orderItemSkuId           :: SkuId
  , orderItemSkuTitle        :: Text
  } 
  deriving (Show)

instance FromJSON OrderItem where
  parseJSON = withObject "OrderItem" $ \o -> do
    OrderItem <$> o .: "item_id"
              <*> ( o .: "price" >>= parsePrice )
              <*> ( o .: "quantity" >>= parseStrInt )
              <*> ( o .: "total_price" >>= parsePrice )
              <*> o .: "item_name"
              <*> o .: "img"
              <*> o .: "url"
              <*> o .:? "merchant_code"
              <*> o .:? "sku_merchant_code"
              <*> o .: "sku_id"
              <*> o .: "sku_title"


parsePrice :: Value -> A.Parser Price
parsePrice (Number sc) = return $ SC.toRealFloat sc
parsePrice (String t)  = maybe mzero return $ readMay t
parsePrice x           = A.typeMismatch "Price" x


-- | 有多个类型虽然json里是字串格式, 但看内容应该总是个整数
-- 不排除平台接口什么时候忽然这些字段就真的变成整数了
-- 这个解释器能接受字串或整数
parseStrInt :: (Integral i, Read i) => Value -> A.Parser i

parseStrInt (String t) =
  maybe (fail $ "invalid integer string: " <> T.unpack t) return $
    readMay t

parseStrInt (Number sc) =
  either (const $ fail $ "expecting integer but get floating: " <> show sc) return $
    (SC.floatingOrInteger sc :: Either Double _)

parseStrInt x = A.typeMismatch "Integer" x


nullIfEmptyString :: Value -> Value
nullIfEmptyString (String t) | null t = Null
nullIfEmptyString x                   = x
