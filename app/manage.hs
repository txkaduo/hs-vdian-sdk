module Main where

import           ClassyPrelude hiding ((<>))
import           Control.Monad.Logger
import qualified Data.Yaml             as Y
import qualified Data.ByteString       as B
import           Options.Applicative
import qualified Network.Wreq.Session  as WS

import           System.Log.FastLogger (LoggerSet, newStderrLoggerSet,
                                        pushLogStr)


import VDian

data ManageCmd = CmdListOrders
                deriving (Show, Eq, Ord)


data Options = Options {
                optAppKey       :: AppKey
                , optAppSecret  :: AppSecret
                , optEditor     :: Maybe Text
                , optVerbose    :: Int
                , optUrlBase    :: Maybe String
                , optCommand    :: ManageCmd
                }


manageCmdParser :: Parser ManageCmd
manageCmdParser = subparser $
    command "list-orders"
        (info (helper <*> pure CmdListOrders)
            (progDesc "列出订单"))

optionsParse :: Parser Options
optionsParse = Options
                <$> (AppKey . fromString <$> strOption (long "key"
                                                <> metavar "APP_KEY"
                                                <> help "App Key"))
                <*> (AppSecret . fromString <$> strOption (long "secret"
                                                <> metavar "APP_SECRET"
                                                <> help "App Secret String"))
                <*> (optional $ fromString <$> strOption
                                    (long "editor"
                                    <> metavar "EDITOR_CMD"
                                    <> help "external editor command"))
                <*> (option auto
                        $ long "verbose" <> short 'v' <> value 1
                        <> metavar "LEVEL"
                        <> help "Verbose Level (0 - 3)")
                <*> (optional $ strOption
                        $ long "url-base" <> short 'b'
                        <> metavar "URL_BASE"
                        <> help "API URL Base")
                <*> manageCmdParser

start :: (ApiCallMonad m) => WS.Session -> Options -> m ()
start sess opts = do
  let api_conf0 = mkDefaultVDianApiConfig sess
  let api_conf = case optUrlBase opts of
                  Nothing -> api_conf0
                  Just url -> api_conf0 { vdianApiUrlBase = url }

  let app_key = optAppKey opts
      app_secret = optAppSecret opts
      get_atk = do
        getAccessToken api_conf app_key app_secret >>= extractResultData

  case optCommand opts of
      CmdListOrders -> do
        atk <- fmap getAtkToken $ get_atk
        $logInfo $ "AccessToken is: " <> unAccessToken atk
        ListOrders total orders <- listOrders api_conf atk ListOrderUnpaid [ListOrderPageSize 1] 1
                                    >>= extractResultData
        putStrLn $ "Total order number: " <> tshow total
        liftIO $ B.putStr $ Y.encode orders

        forM_ orders $ \order -> do
          let order_id = simpleOrderOrderId order

          _details <- getOrderDetails api_conf atk order_id >>= extractResultData
          return ()


appLogger :: LoggerSet -> Int -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
appLogger logger_set verbose loc src level ls = do
  let should_log = case level of
                      LevelOther {}   -> True
                      _               -> level `elem` lv_by_v verbose

  if should_log
      then pushLogStr logger_set $ defaultLogStr loc src level ls
      else return ()
  where
      lv_by_v lv
          | lv <= 0   = [ LevelError]
          | lv == 1   = [ LevelError, LevelWarn ]
          | lv == 2   = [ LevelError, LevelWarn, LevelInfo ]
          | otherwise = [ LevelError, LevelWarn, LevelInfo, LevelDebug ]

start' :: Options -> IO ()
start' opts = WS.withAPISession $ \sess -> do
  logger_set <- newStderrLoggerSet 0
  runLoggingT
      (start sess opts)
      (appLogger logger_set (optVerbose opts))

main :: IO ()
main = execParser opts >>= start'
  where
      opts = info (helper <*> optionsParse)
              ( fullDesc
                  <> progDesc (unlines
                      [ "微店接口命令行测试工具"
                      ])
                  <> header "vdian-manage - 微店平台管理查询小工具"
                  )
