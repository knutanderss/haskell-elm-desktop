{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Additional
import Control.Concurrent
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.STM as STM
import Control.Monad.Trans.Reader
import Control.Monad.Writer.Lazy
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Elm.Derive (deriveBoth)
import Elm.Module
import GHC.Generics
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Paths_desktop
import Servant
import Servant.API.Generic
import Servant.Elm
import Servant.Server.Generic (AsServerT, genericServerT)
import System.Environment (getArgs)
import qualified Text.InterpolatedString.Perl6 as P6

data CreateUserForm = CreateUserForm
  { cufName :: Text
  , cufSurname :: Text
  } deriving (Show, Generic)

deriveBoth (jsonOpts 3) ''CreateUserForm

data UpdateUserForm = UpdateUserForm
  { uufName :: Text
  , uufSurname :: Text
  } deriving (Show, Generic)

deriveBoth (jsonOpts 3) ''UpdateUserForm

data UserInfo = UserInfo
  { uinId :: Int
  , uinName :: Text
  , uinSurname :: Text
  } deriving (Show, Generic)

deriveBoth (jsonOpts 3) ''UserInfo

data FullAPI route = FullAPI
  { _api :: route :- ToServantApi API
  , _static :: route :- Raw
  } deriving (Generic)

data API route = API
  { _ping :: route :- "api" :> "ping" :> Get '[ JSON] Text
  , _listUsers :: route :- "api" :> "users" :> "list.json" :> Get '[ JSON] [UserInfo]
  , _createUser :: route :- "api" :> "users" :> "create.json" :> ReqBody '[ JSON] CreateUserForm :> Post '[ JSON] UserInfo
  , _updateUser :: route :- "api" :> "users" :> Capture "user-id" UserId :> "update.json" :> ReqBody '[ JSON] UpdateUserForm :> Put '[ JSON] UserInfo
  , _deleteUser :: route :- "api" :> "users" :> Capture "user-id" UserId :> "delete.json" :> Delete '[ JSON] ()
  } deriving (Generic)

api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)

fullAPI :: Proxy (ToServantApi FullAPI)
fullAPI = genericApi (Proxy :: Proxy FullAPI)

fullAPIServer :: FilePath -> FullAPI (AsServerT AppM)
fullAPIServer filePath =
  FullAPI
    {_api = genericServerT server, _static = serveDirectoryFileServer filePath}

server :: API (AsServerT AppM)
server =
  API
    { _ping = pure "pong"
    , _listUsers = listUsers
    , _createUser = createUser
    , _updateUser = updateUser
    , _deleteUser = deleteUser
    }

listUsers :: AppM [UserInfo]
listUsers = do
  env <- ask
  users <- liftIO $ TVar.readTVarIO (envUsers env)
  forM (Map.toList users) $ \(userId, User {..}) ->
    pure $ UserInfo userId usrName usrSurname

createUser :: CreateUserForm -> AppM UserInfo
createUser CreateUserForm {..} = do
  env <- ask
  c <-
    liftIO $
    STM.atomically $ do
      c <- TVar.readTVar (envCounter env)
      TVar.modifyTVar (envCounter env) (+ 1)
      let user = User {usrName = cufName, usrSurname = cufSurname}
      TVar.modifyTVar (envUsers env) (Map.insert c user)
      pure c
  pure $ UserInfo c cufName cufSurname

updateUser :: UserId -> UpdateUserForm -> AppM UserInfo
updateUser userId UpdateUserForm {..} = do
  env <- ask
  liftIO $
    STM.atomically $ do
      let user = User {usrName = uufName, usrSurname = uufSurname}
      TVar.modifyTVar (envUsers env) (Map.insert userId user)
  pure $ UserInfo userId uufName uufSurname

deleteUser :: UserId -> AppM ()
deleteUser userId = do
  env <- ask
  liftIO $ STM.atomically $ TVar.modifyTVar (envUsers env) (Map.delete userId)

type UserId = Int

data User = User
  { usrName :: Text
  , usrSurname :: Text
  }

data Env = Env
  { envUsers :: TVar (Map UserId User)
  , envCounter :: TVar Int
  }

type AppM = ReaderT Env Servant.Handler

nt :: Env -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

generateElm :: IO ()
generateElm = writeFile "ui/src/Api.elm" $ execWriter writeElmCode

writeElmCode :: Writer String ()
writeElmCode = do
  tell elmHeader
  tell "\n"
  let defs =
        makeModuleContent
          [ DefineElm (Proxy :: Proxy UserInfo)
          , DefineElm (Proxy :: Proxy CreateUserForm)
          , DefineElm (Proxy :: Proxy UpdateUserForm)
          ]
  tell defs
  tell "\n"
  forM_ (generateElmForAPI (Proxy :: Proxy (ToServantApi API))) $ \t -> do
    tell (T.unpack t)
    tell "\n"

startServer :: Int -> IO ()
startServer port = do
  staticUri <- getDataFileName "res/ui"
  putStrLn "Starting server..."
  tv <- STM.atomically $ TVar.newTVar (Map.fromList [])
  counter <- TVar.newTVarIO 1
  let env = Env tv counter
  Warp.run
    port
    (simpleCors
       (serve
          (Proxy :: Proxy (ToServantApi FullAPI))
          (hoistServer
             (Proxy :: Proxy (ToServantApi FullAPI))
             (nt env)
             (genericServerT $ fullAPIServer staticUri))))

elmHeader :: String
elmHeader =
  [P6.q|module Api exposing (..)

import Http
import Json.Decode exposing (Value)
import Json.Encode
import Json.Decode.Pipeline exposing (required)
import Url.Builder

type alias Text = String
jsonDecText = Json.Decode.string

|]
