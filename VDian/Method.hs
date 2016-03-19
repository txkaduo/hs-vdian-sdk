module VDian.Method
  ( module VDian.Method
  , module VDian.Types
  ) where

import           ClassyPrelude
import           Control.Lens  hiding ((.=))
import           Control.Monad.Logger
import qualified Data.Text.Lazy as LT
import           Data.Aeson
import           Network.Wreq

import           VDian.Types


vdianApiUrlBase :: String
vdianApiUrlBase =
#if defined(VDIAN_API_URL_BASE)
#define XSTRING(v) STRING(v)
#define STRING(v) #v
-- XSTRING(VDIAN_API_URL_BASE)
  "http://127.0.0.1:8066"
#else
#error "https://api.vdian.com"
#endif

defaultVDianApiUrl :: String
defaultVDianApiUrl = vdianApiUrlBase <> "/api"


mkApiPublicParam :: AccessToken -> Text -> ApiVersion -> ApiPublicParam
mkApiPublicParam atk method ver = ApiPublicParam method atk ApiFormatJson ver

vdianSdkLogSource :: Text
vdianSdkLogSource = "VDIAN-SDK"

callMethod :: (FromJSON a, ApiCallMonad m)
           => AccessToken
           -> Text
           -> ApiVersion
           -> ApiPrivateParam
           -> m (ApiCallResult a)
callMethod atk method ver method_params = do
  let url = defaultVDianApiUrl
      pub_params = mkApiPublicParam atk method ver
      opts = defaults & param "public" .~ [ LT.toStrict (decodeUtf8 $ encode pub_params) ]
                      & param "param" .~ [ LT.toStrict ( decodeUtf8 $ encode $ Object method_params) ]

  r <- liftIO (postWith opts url (asByteString ""))
  fmap (view responseBody) (asJSON $ alterContentTypeToJson r)
    `catch`
    \err@(JSONError errmsg) -> do
      $logErrorS vdianSdkLogSource $
        "failed to parse response of method '" <> method <> "': "
        <> fromString errmsg
      throwM err



getAccessToken :: ApiCallMonad m => AppKey -> AppSecret -> m (ApiCallResult GetAccessTokenInfo)
getAccessToken key secret = do
  let url = vdianApiUrlBase <> "/token"
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
