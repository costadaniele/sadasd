{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Auxiliar where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core
import Data.Map.Strict
import Common.Api
import Common.Route
import Control.Monad.Fix

import Text.Read
import Data.Maybe


revText :: T.Text -> T.Text
-- revText t = T.pack (reverse (T.unpack t))
revText t = T.pack (T.unpack t)

countClick :: DomBuilder t m => m (Event t Int)
countClick = do
    (ev, _) <- el' "button" (text "+")
    return ((const 1) <$> domEvent Click ev)

home :: DomBuilder t m => m () 
home = do
    el "h2" $ text $ T.pack commonStuff
    el "p" $ text "Uma criptomoeda ou cibermoeda é um meio de troca, podendo ser centralizado ou descentralizado que se utiliza da tecnologia de blockchain e da criptografia para assegurar a validade das transações e a criação de novas unidades da moeda."
    el "p" $ text "O Bitcoin, a primeira criptomoeda descentralizada, foi criado em 2009 por um usuário que usou o pseudônimo Satoshi Nakamoto."  
    el "p" $ text "Desde então, muitas outras criptomoedas foram criadas."  
    el "h6" $ text "fonte: wikipédia"

wallet :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
wallet = do
    el "h2" $ text "De olho no mercado"
    elAttr "img" ("src" =: $(static "img/crip.png") <> "id" =: "deOlho") blank
    
aboutUs :: (MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m) => m ()
aboutUs = do
    el "h2" $ text "Sobre nós"
    el "p" $ text "A Portcripto nasceu da necessidade de atender de forma clara os clientes interessados em comprar criptomoedas, mas que não possuíam um entendimento sobre o assunto. As moedas digitais mesmo em crescimento global impressionante ainda estão sendo pouco comercializadas pela maioria das pessoas, por se sentirem inseguras e sem as informações necessárias para reverter esse quadro."
    el "p" $ text "Nosso intuito além de propagar o entendimento sobre criptomoedas é mostrar o quanto esse mercado é promissor e seja percebido como mais uma fonte de investimento, apesar de todo risco que uma operação financeira apresenta, pode ser acessível a qualquer pessoa."
    el "p" $ text "Pelo nosso sistema, as pessoas terão informações por exemplo de onde surgiram as criptomoedas, como elas podem comprar os valores e assim fazer suas próprias escolhas de compra e o funcionamento de uma carteira de moedas digitais, principalmente como esse processo pode se tornar seguro para quem pretende investir, tornando a sua experiência em negociações de modo simples e seguro."
    el "p" $ text "Os sócios Daniela, Jonatas e Rosângela formados na Faculdade de Tecnologia Rubens Lara investiram na melhor tecnologia pensando em oferecer a maior segurança e melhor experiência, levando confiança a você."
    el "p" $ text "Maiores informações, entre em contato com nossa Central de Ajuda."

contact :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
contact = do
    evt <- buttonClick
    hl <- holdDyn "" evt -- Event -> Dynamic
    el "div" (dynText hl)

-- constroi o evento de clique
buttonClick :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Event t T.Text)
buttonClick = do
    el "label" (text "Digite seu nome: ")
    t <- inputElement def
    (e,_) <- el' "button" (text "OK")
    el "label" (text "Olá: ")
    return $ attachPromptlyDynWith const (fmap revText (_inputElement_value t)) (domEvent Click e)
