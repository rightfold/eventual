module Eventual.Executables.Eventual
  ( main
  ) where

import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Semigroup ((<>))
import System.Posix.Env.ByteString (getEnv)

import qualified Brick as Brick
import qualified Data.Aeson as Aeson
import qualified Eventual.Dashboard as Dashboard
import qualified Graphics.Vty as Vty
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types as HTTP

main :: IO ()
main = do
  Just sentryToken <- getSentryToken

  httpManager <- HTTP.newTlsManager

  projects <- do
    let request = makeRequest sentryToken "/projects"
    response <- HTTP.httpLbs request httpManager
    either fail pure . Aeson.eitherDecode' $ HTTP.responseBody response

  let app :: Brick.App (Dashboard.State Int) e Int
      app = Brick.App
        { Brick.appDraw = pure . Dashboard.render
        , Brick.appChooseCursor = \_ _ -> Nothing
        , Brick.appHandleEvent = \s e ->
            case e of
              Brick.VtyEvent (Vty.EvKey (Vty.KChar 'q') []) -> Brick.halt s
              Brick.VtyEvent e' -> Brick.continue =<< Dashboard.handle e' s
              _ -> Brick.continue s
        , Brick.appStartEvent = \s -> pure s
        , Brick.appAttrMap = \_ -> Brick.attrMap Vty.defAttr []
        }

  let initial :: Dashboard.State Int
      initial = Dashboard.initial (0, projects) (1, [])

  void $ Brick.defaultMain app initial

newtype SentryToken = SentryToken ByteString

getSentryToken :: IO (Maybe SentryToken)
getSentryToken = fmap (fmap SentryToken) $ getEnv "EVENTUAL_SENTRY_TOKEN"

makeRequest :: SentryToken -> String -> HTTP.Request
makeRequest (SentryToken sentryToken) path =
  let
    base :: HTTP.Request
    base = HTTP.parseRequest_ ("https://sentry.io/api/0" <> path <> "/")

    headers :: [HTTP.Header]
    headers = [("Authorization", "Bearer " <> sentryToken)]

    request :: HTTP.Request
    request = base { HTTP.requestHeaders = headers }
  in
    request
