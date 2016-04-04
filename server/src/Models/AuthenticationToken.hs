{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Models.AuthenticationToken where

import Models.Import
import Prelude hiding (sum)
import qualified Shared.Api as Api

data AuthenticationToken' a b c = AuthenticationToken { atToken :: a
                                                      , atUserId :: b
                                                      , atExpiration :: c
                                                      } deriving (Eq, Show)
$(makeAdaptorAndInstance  "pAuthenticationToken" ''AuthenticationToken')

newtype ATToken' a = ATToken a
$(makeAdaptorAndInstance  "pATToken" ''ATToken')
type ATTokenColumn = ATToken' (Column PGText)
type ATToken = ATToken' String

newtype ATUserId' b = ATUserId b
$(makeAdaptorAndInstance  "pATUserId" ''ATUserId')
type ATUserIdColumn = ATUserId' (Column PGInt4)
type ATUserId = ATUserId' Int

newtype ATExpiration' c = ATExpiration c
$(makeAdaptorAndInstance  "pATExpiration" ''ATExpiration')
type ATExpirationColumn = ATExpiration' (Column PGDate)
type ATExpiration = ATExpiration' Day

type ATColumn = AuthenticationToken' ATTokenColumn ATUserIdColumn ATExpirationColumn

type AuthenticationToken = AuthenticationToken' ATToken ATUserId ATExpiration

authenticationTokenTable :: Table ATColumn ATColumn
authenticationTokenTable = Table "authentication_tokens" $ pAuthenticationToken atTable
  where atTable = AuthenticationToken { atToken = pATToken (ATToken (required "token"))
                                      , atUserId = pATUserId (ATUserId (required "user_id"))
                                      , atExpiration = pATExpiration (ATExpiration (required "expiration"))
                                      }

getByToken :: String -> Query ATColumn
getByToken token = proc () -> do
  authToken <- queryTable authenticationTokenTable -< ()
  restrict -< (ATToken (pgString token)) .=== (atToken authToken)
  returnA -< authToken

insertAuthenticationToken :: Connection -> Int -> Day -> String -> IO ()
insertAuthenticationToken conn userid expiration token = do
  runInsert conn authenticationTokenTable at 
  return ()
  where at = AuthenticationToken { atToken = ATToken (pgString token)
                                 , atUserId = ATUserId (pgInt4 userid)
                                 , atExpiration = ATExpiration (pgDay expiration)
                                 }

createAuthenticationToken :: Connection -> Int -> Day -> IO ()
createAuthenticationToken conn userid expiration = createToken $ insertAuthenticationToken conn userid expiration