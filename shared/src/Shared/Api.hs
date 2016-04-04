{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Shared.Api where

import Data.Aeson
import Data.Aeson.TH
import Servant.API
import Data.Text
import Data.Time.Calendar (Day)

data AuthenticationTokenTrans = AuthenticationToken { atToken :: String
                                                    , atUserId :: Int
                                                    } deriving (Eq, Show)
$(deriveJSON defaultOptions ''AuthenticationTokenTrans)

data BlogPost = BlogPost Int
data Comment = Comment Int

data User = User
  { userId :: Int
  , username :: String
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "user" String :> Capture "pass" String :> Post '[JSON] User
  :<|> "add" :> Capture "a" Integer :> Capture "b" Integer :> Get '[JSON] Integer
  :<|> "sub" :> Capture "a" Integer :> Capture "b" Integer :> Get '[JSON] Integer
  :<|> "mult" :> Capture "a" Integer :> Capture "b" Integer :> Get '[JSON] Integer
  :<|> "div" :> Capture "a" Integer :> Capture "b" Integer :> Get '[JSON] Integer
  :<|> "test" :> 
       ( "lol" :> Get '[JSON] Integer
    :<|> "noo" :> Get '[JSON] Integer
       )
  :<|> "repeat" :> Header "Custom-Field" Text :> Get '[JSON] String
  :<|> "static" :> Raw
  :<|> Raw

type NewEditDelete a =
       "new" :> Post '[JSON] a
  :<|> Capture "id" Integer :>
       ( "edit" :> Post '[JSON] a
    :<|> "delete" :> Delete '[] ()
       )

type ApiAuth =
       "blog" :> NewEditDelete BlogPost
  :<|> "comment" :> NewEditDelete Comment
