module VDian.Cache
  ( module VDian.Cache
  , module VDian.Types
  ) where

import           ClassyPrelude

import           VDian.Types


class VDianCacheReader a where
  vdianGetAccessToken :: a -> AppKey -> IO (Maybe (AccessToken, UTCTime))

data SomeVDianCacheReader = forall a. VDianCacheReader a => SomeVDianCacheReader a


class VDianCacheUpdater a where
  vdianUpdateAccessToken :: a -> AppKey -> AccessToken -> UTCTime -> IO ()

data SomeVDianCacheUpdater = forall a. VDianCacheUpdater a => SomeVDianCacheUpdater a
