module VDian.Method
  ( module VDian.Method
  , module VDian.Types
  ) where

import           ClassyPrelude
import           Control.Lens          hiding ((.=))
import           Control.Monad.Logger
import           Data.Aeson
import qualified Data.ByteString.Lazy  as LB
import qualified Data.Text.Lazy        as LT
import           Network.Wreq

import           VDian.Types


defaultVDianApiUrl :: VDianApiConfig -> String
defaultVDianApiUrl conf = vdianApiUrlBase conf <> "/api"


mkApiPublicParam :: AccessToken -> Text -> ApiVersion -> ApiPublicParam
mkApiPublicParam atk method ver = ApiPublicParam method atk ApiFormatJson ver

vdianSdkLogSource :: Text
vdianSdkLogSource = "VDIAN-SDK"

callMethod' :: (FromJSON a, ApiCallMonad m)
            => VDianApiConfig
            -> AccessToken
            -> Text
            -> ApiVersion
            -> ApiPrivateParam
            -> m (ApiCallResult a, Response LB.ByteString)
callMethod' conf atk method ver method_params = do
  let url = defaultVDianApiUrl conf
      pub_params = mkApiPublicParam atk method ver
      opts = defaults & param "public" .~ [ LT.toStrict (decodeUtf8 $ encode pub_params) ]
                      & param "param" .~ [ LT.toStrict ( decodeUtf8 $ encode $ Object method_params) ]

  r <- liftIO (postWith opts url (asByteString ""))
  fmap ((,r) . view responseBody) (asJSON $ alterContentTypeToJson r)
    `catch`
    \err@(JSONError errmsg) -> do
      $logErrorS vdianSdkLogSource $
        "failed to parse response of method '" <> method <> "': "
        <> fromString errmsg
      throwM err

callMethod :: (FromJSON a, ApiCallMonad m)
           => VDianApiConfig
           -> AccessToken
           -> Text
           -> ApiVersion
           -> ApiPrivateParam
           -> m (ApiCallResult a)
callMethod conf atk method ver method_params =
  fmap fst $ callMethod' conf atk method ver method_params


getAccessToken :: ApiCallMonad m
               => VDianApiConfig
               -> AppKey
               -> AppSecret
               -> m (ApiCallResult GetAccessTokenInfo)
getAccessToken conf key secret = do
  let url = vdianApiUrlBase conf <> "/token"
      opts = defaults & param "grant_type" .~ [ asText "client_credential" ]
                      & param "appkey" .~ [ unAppKey key ]
                      & param "secret" .~ [ unAppSecret secret ]

  liftM (view responseBody) $
    liftIO (getWith opts url) >>= asJSON


extractResultData :: MonadThrow m => ApiCallResult a -> m a
extractResultData r =
  if sc == 0
     then maybe (throwM ApiCallNoData) return $ apiCallResultData r
     else throwM $ ApiCallException sc reason
  where
    st     = apiCallResultStatus r
    sc     = apiCallStatusCode st
    reason = apiCallStatusReason st


-- | Workaround: 出错时, 服务器的响应的content type是text/html, 而不是json
alterContentTypeToJson :: Response body -> Response body
alterContentTypeToJson r =
  if "text/" `isPrefixOf` (r ^. responseHeader "Content-Type")
    then r & responseHeader "Content-Type" .~ "application/json"
    else r
