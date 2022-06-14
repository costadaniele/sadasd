{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core
import Data.Aeson
import Common.Api
import Common.Route
import Auxiliar
import Menu
import Control.Monad.Fix

reqProd :: ( DomBuilder t m, Prerender t m) => m ()
reqProd = do
        nome <- inputElement def
        vl <- numberInput
        qt <- numberInput
        let prod = fmap (\((n,v),q) -> Produto 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) vl) qt)
        (submitBtn,_) <- el' "button" (text "Inserir")
        let click = domEvent Click submitBtn
        let prodEvt = tag (current prod) click
        _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
            (pure never)
            (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Produto :/ ()) <$> prodEvt))
        return ()

getPath :: R BackendRoute -> T.Text
getPath p = renderBackendRoute checFullREnc p

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

getPath' :: T.Text
getPath' = renderBackendRoute checFullREnc $ BackendRoute_Cliente :/ ()

nomeRequest :: T.Text -> XhrRequest T.Text
nomeRequest s = postJson getPath (Cliente s)

req :: ( DomBuilder t m, Prerender t m) => m ()
req = do
    inputEl <- inputElement def
    (submitBtn,_) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let nm = tag (current $ _inputElement_value inputEl) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (nomeRequest <$> nm))
    return ()

getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_Listar :/ ())) def

tabProduto :: DomBuilder t m => Produto -> m ()
tabProduto pr = do
    el "tr" $ do
        el "td" (text $ T.pack $ show $ produtoId pr)
        el "td" (text $ produtoNome pr)
        el "td" (text $ T.pack $ show $ produtoValor pr)
        el "td" (text $ T.pack $ show $ produtoQt pr)

reqLista :: ( DomBuilder t m
            , Prerender t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqLista = do
    (btn, _) <- el' "button" (text "Listar")
    let click = domEvent Click btn
    prods :: Dynamic t (Event t (Maybe [Produto])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> click))
    dynP <- foldDyn (\ps d -> case ps of
                        Nothing -> []
                        Just p -> d++p) [] (switchDyn prods)

    el "table" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Id")
                el "th" (text "Nome")
                el "th" (text "Valor")
                el "th" (text "Qt")

        el "tbody" $ do
            dyn_ (fmap sequence (ffor dynP (fmap tabProduto)))



-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Portfólio Criptomoeda"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      elAttr "img" ("src" =: $(static "image3.jpg") <> "id" =: "logo") blank
      
      el "h1" $ text "PortCripto"
      mainPag 
      reqProd
      reqLista
      req
      return ()
  }
