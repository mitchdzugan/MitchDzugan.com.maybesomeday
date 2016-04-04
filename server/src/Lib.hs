{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    ) where

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Shared.Api
import qualified Data.Text as T
import Database.PostgreSQL.Simple.Internal 
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.Time.Calendar

import           Opaleye (Column, Nullable, matchNullable, isNull,
                         Table(Table), required, queryTable,
                         Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                         (.===), pgDay, pgInt4,
                         (.++), ifThenElse, pgString, aggregate, groupBy,
                         count, avg, sum, leftJoin, runQuery, runInsert,
                         showSqlForPostgres, Unpackspec,
                         PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool)

import           Data.Profunctor.Product (p2, p3)
import Models.AuthenticationToken
import Models.RefreshToken

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

appFile _ respond = respond index

index :: Response
index = responseFile
    status200
    [("Content-Type", "text/html")]
    "server/static/index.html"
    Nothing

postUser :: String -> String -> EitherT ServantErr IO User
postUser user pass = do
  c <- liftIO $ connect defaultConnectInfo { connectDatabase = "mitchdzugancom" }
  liftIO $ createAuthenticationToken c 123 $ fromGregorian 2015 1 1
  liftIO $ createRefreshToken c 123 $ fromGregorian 2015 1 1
  liftIO $ print "Hello World"
  return $ User 0 user

server :: Server API
server = getUsers :<|> postUser :<|> getAdd :<|> getSub :<|> getMult :<|> getDiv :<|> getTest :<|> rep :<|> static :<|> home
  where getUsers = return users
        getAdd a b = return (a + b)
        getSub a b = return (a - b)
        getMult a b = return (a * b)
        getDiv a b = return (a `div` b)
        getTest = getLol :<|> getNoo
        getLol = return 2
        getNoo = return 3
        rep (Just s) = return ((T.unpack s) ++ (T.unpack s))
        rep (Nothing) = return "Doh"
        static = serveDirectory "./server/static"
        home = appFile

users :: [User]
users = [ User 1 "Isaac"
        , User 2 "Albert"
        ]
