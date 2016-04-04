{-# LANGUAGE ScopedTypeVariables #-}

module Models.Import
    ( module Models.Import
    ) where

import Opaleye as Models.Import (Column, Nullable, matchNullable, isNull,
                                 Table(Table), required, queryTable,
                                 Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                                 (.===), pgDay, pgInt4,
                                 (.++), ifThenElse, pgString, aggregate, groupBy,
                                 count, avg, sum, leftJoin, runQuery, runInsert,
                                 showSqlForPostgres, Unpackspec,
                                 PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool)
import Database.PostgreSQL.Simple as Models.Import (Connection,SqlError)
import Data.Profunctor.Product as Models.Import (p2, p3)
import Data.Profunctor.Product.Default as Models.Import (Default)
import Data.Profunctor.Product.TH as Models.Import (makeAdaptorAndInstance)
import Data.Time.Calendar as Models.Import (Day)
import Control.Arrow as Models.Import (returnA, (<<<))

import Control.Exception
import Control.Applicative ((<$>))
import System.Random
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS64
import Data.Text.Encoding (decodeUtf8)

createToken :: (String -> IO ()) -> IO ()
createToken inserter = do
  let base64String = T.unpack . decodeUtf8 . BS64.encode . BS.pack
  let randomToken = base64String . take 32 . randomRs (0, 255) <$> newStdGen
  let createToken' = do {
    token <- randomToken;
    inserter token;
    return ();
  }
  let createTokenHandler (e :: SqlError) = do {
    print e;
    createToken inserter;
  }
  catch createToken' (\e -> createTokenHandler e)
