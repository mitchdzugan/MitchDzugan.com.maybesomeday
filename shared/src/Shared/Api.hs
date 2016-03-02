{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Shared.Api where

import Data.Aeson
import Data.Aeson.TH
import Servant.API
import Data.Text

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]
  :<|> "add" :> Capture "a" Integer :> Capture "b" Integer :> Get '[JSON] Integer
  :<|> "sub" :> Capture "a" Integer :> Capture "b" Integer :> Get '[JSON] Integer
  :<|> "mult" :> Capture "a" Integer :> Capture "b" Integer :> Get '[JSON] Integer
  :<|> "div" :> Capture "a" Integer :> Capture "b" Integer :> Get '[JSON] Integer
  :<|> "repeat" :> Header "Custom-Field" Text :> Get '[JSON] String
  :<|> "static" :> Raw
  :<|> Raw
