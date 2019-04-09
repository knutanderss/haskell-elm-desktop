{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Api
import qualified Clay
import           Control.Concurrent
import           Control.Concurrent.BoundedChan
                                               as CCBC
import           Control.Lens
import           Control.Monad
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Text
import           Data.Text.Format.Heavy
import qualified Data.Text.Lazy                as DTL
import qualified Graphics.UI.Webviewhs         as WHS
import           Language.Javascript.JMacro
import           Paths_desktop

port = 8000

startUI :: IO ()
startUI = WHS.createWindowAndBlock WHS.WindowParams
  { WHS.windowParamsTitle      = "MD Notes"
  , WHS.windowParamsUri        = pack $ "http://localhost:" ++ show port
  , WHS.windowParamsWidth      = 400
  , WHS.windowParamsHeight     = 600
  , WHS.windowParamsResizable  = False
  , WHS.windowParamsDebuggable = True
  }

main :: IO ()
main = do
  forkOS $ Api.startServer port
  startUI
