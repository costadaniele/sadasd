{-# LANGUAGE LambdaCase, GADTs, OverloadedStrings, ScopedTypeVariables #-}

module Backend where

import Common.Route
import Obelisk.Backend
import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Common.Api
import Data.Aeson.Text

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS cliente\
          \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

migrationProd :: Query
migrationProd = "CREATE TABLE IF NOT EXISTS produto\
                \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, valor DOUBLE PRECISION, qt INTEGER NOT NULL)"

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-52-72-99-110.compute-1.amazonaws.com"
                      5432 -- porta
                      "gqtibiwnwspkyr"
                      "e23689914c801917b1ad815a1bf5f1b8440aa97700b86a45bbb94f308839d882"
                      "d2bomkhqj5vrrt"


backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do 
        \case
          BackendRoute_Cliente :/ () -> do
              Just nome <- A.decode <$> readRequestBody 2000
              liftIO $ do
                  execute_ dbcon migration
                  execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
              modifyResponse $ setResponseStatus 200 "OK"
          BackendRoute_Listar :/ () -> method GET $ do
              res :: [Produto] <- liftIO $ do
                  execute_ dbcon migrationProd
                  query_ dbcon "SELECT * from produto"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)
          BackendRoute_Buscar :/ pid -> method GET $ do
              res :: [Produto] <- liftIO $ do
                  execute_ dbcon migrationProd
                  query dbcon "SELECT * from produto where id=?" (Only (pid :: Int))
              if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
              else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
          BackendRoute_Produto :/ () -> method POST $ do
              produto <- A.decode <$> readRequestBody 2000
              case produto of
                  Just prod -> do
                      liftIO $ do
                          execute_ dbcon migrationProd
                          execute dbcon "INSERT INTO produto (nome,valor,qt) VALUES (?,?,?)" (produtoNome prod, produtoValor prod, produtoQt prod)
                      modifyResponse $ setResponseStatus 200 "OK"
                  _ -> modifyResponse $ setResponseStatus 500 "Erro"
          _ -> return ()

  , _backend_routeEncoder = fullRouteEncoder
  }
