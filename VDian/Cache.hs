module VDian.Cache
  ( module VDian.Cache
  , module VDian.Types
  ) where

import           ClassyPrelude

import           VDian.Types


class VDianCacheReader a where
  vdianCacheGetAccessToken :: a -> AppKey -> IO (Maybe AccessTokenInfo)

data SomeVDianCacheReader = forall a. VDianCacheReader a => SomeVDianCacheReader a


class VDianCacheRegister a where
  vdianCacheRegisterApp :: a -> AppKey -> AppSecret -> IO ()

data SomeVDianCacheRegister = forall a. VDianCacheRegister a => SomeVDianCacheRegister a
