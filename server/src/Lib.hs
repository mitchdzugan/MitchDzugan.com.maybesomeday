{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Shared.Api

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getUsers :<|> getAdd :<|> getSub :<|> getMult :<|> getDiv :<|> static
  where getUsers = return users
        getAdd a b = return (a + b)
        getSub a b = return (a - b)
        getMult a b = return (a * b)
        getDiv a b = return (a `div` b)
        static = serveDirectory "./static"

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
