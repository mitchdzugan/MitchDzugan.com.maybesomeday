{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Models.RefreshToken where

import Models.Import
import Prelude hiding (sum)
import qualified Shared.Api as Api

data RefreshToken' a b c = RefreshToken { rtToken :: a
                                        , rtUserId :: b
                                        , rtExpiration :: c
                                        } deriving (Eq, Show)
$(makeAdaptorAndInstance  "pRefreshToken" ''RefreshToken')

newtype RTToken' a = RTToken a
$(makeAdaptorAndInstance  "pRTToken" ''RTToken')
type RTTokenColumn = RTToken' (Column PGText)
type RTToken = RTToken' String

newtype RTUserId' b = RTUserId b
$(makeAdaptorAndInstance  "pRTUserId" ''RTUserId')
type RTUserIdColumn = RTUserId' (Column PGInt4)
type RTUserId = RTUserId' Int

newtype RTExpiration' c = RTExpiration c
$(makeAdaptorAndInstance  "pRTExpiration" ''RTExpiration')
type RTExpirationColumn = RTExpiration' (Column PGDate)
type RTExpiration = RTExpiration' Day

type RTColumn = RefreshToken' RTTokenColumn RTUserIdColumn RTExpirationColumn

type RefreshToken = RefreshToken' RTToken RTUserId RTExpiration

refreshTokenTable :: Table RTColumn RTColumn
refreshTokenTable = Table "refresh_tokens" $ pRefreshToken rtTable
  where rtTable = RefreshToken { rtToken = pRTToken (RTToken (required "token"))
                               , rtUserId = pRTUserId (RTUserId (required "user_id"))
                               , rtExpiration = pRTExpiration (RTExpiration (required "expiration"))
                               }

getByToken :: String -> Query RTColumn
getByToken token = proc () -> do
  refreshToken <- queryTable refreshTokenTable -< ()
  restrict -< (RTToken (pgString token)) .=== (rtToken refreshToken)
  returnA -< refreshToken

insertRefreshToken :: Connection -> Int -> Day -> String -> IO ()
insertRefreshToken conn userid expiration token = do
  runInsert conn refreshTokenTable rt 
  return ()
  where rt = RefreshToken { rtToken = RTToken (pgString token)
                          , rtUserId = RTUserId (pgInt4 userid)
                          , rtExpiration = RTExpiration (pgDay expiration)
                          }

createRefreshToken :: Connection -> Int -> Day -> IO ()
createRefreshToken conn userid expiration = createToken $ insertRefreshToken conn userid expiration