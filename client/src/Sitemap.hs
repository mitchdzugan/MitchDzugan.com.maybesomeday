{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, OverloadedStrings #-}

module Sitemap where

import Prelude hiding ((.))
import Control.Category 
import Text.Boomerang.TH
import Web.Routes.Boomerang ((<>),(</>),integer,anyText,boomerangSite)

data Sitemap 
  = HomeR
  | LoginR
  | LogoutR
  | RegisterR
  | BlogR
  | NewBlogPostR
  | ViewBlogPostR Integer 
  | EditBlogPostR Integer
  deriving (Eq,Show)
makeBoomerangs ''Sitemap


sitemap =  rHomeR
        <> "login" . rLoginR
        <> "logout" . rLogoutR
        <> "register" . rRegisterR
        <> "blog" . (  rBlogR
                    <> rNewBlogPostR </> "new"
                    <> rViewBlogPostR </> integer 
                    <> rEditBlogPostR </> integer </> "edit"
                    )

bs = boomerangSite (\_ url -> url) sitemap