{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main (main) where

import Bilge hiding (header, body)
import Cassandra as Cql
import Cassandra.Settings as Cql
import Control.Lens
import Control.Monad (join)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import GHC.Generics
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import OpenSSL (withOpenSSL)
import Options.Applicative
import System.Environment (getArgs)
import System.Logger (Logger)
import Test.Tasty
import Util.Options
import Util.Options.Common
import Util.Test
import Util (TestSetup (..))

import qualified API                 as User
import qualified API.Provider        as Provider
import qualified API.Search          as Search
import qualified API.Team            as Team
import qualified API.TURN            as TURN
import qualified API.User.Auth       as UserAuth
import qualified Brig.Options        as Opts
import qualified System.Logger       as Logger

data IntegrationConfig = IntegrationConfig
  -- internal endpoints
  { brig     :: Endpoint
  , cannon   :: Endpoint
  , galley   :: Endpoint
  -- external provider
  , provider :: Provider.Config
  } deriving (Show, Generic)

instance FromJSON IntegrationConfig

-- runTests :: Maybe Config -> Maybe Opts.Opts -> IO ()
-- runTests iConf bConf = do
--     let local p = Endpoint { _epHost = "127.0.0.1", _epPort = p }
--     b <- mkRequest <$> optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
--     c <- mkRequest <$> optOrEnv cannon iConf (local . read) "CANNON_WEB_PORT"
--     g <- mkRequest <$> optOrEnv galley iConf (local . read) "GALLEY_WEB_PORT"
--     turnFile <- optOrEnv (Opts.servers . Opts.turn) bConf id "TURN_SERVERS"
--     casHost  <- optOrEnv (\v -> (Opts.cassandra v)^.casEndpoint.epHost) bConf pack "BRIG_CASSANDRA_HOST"
--     casPort  <- optOrEnv (\v -> (Opts.cassandra v)^.casEndpoint.epPort) bConf read "BRIG_CASSANDRA_PORT"

--     lg <- Logger.new Logger.defSettings
--     db <- initCassandra (Endpoint casHost casPort) lg
--     mg <- newManager tlsManagerSettings

--     userApi     <- User.tests bConf mg b c g
--     userAuthApi <- UserAuth.tests bConf mg lg b
--     providerApi <- Provider.tests (provider <$> iConf) mg db b c g
--     searchApis  <- Search.tests mg b
--     teamApis    <- Team.tests mg b c g
--     turnApi     <- TURN.tests mg b turnFile

--     defaultMain $ testGroup "Brig API Integration"
--         [ userApi
--         , userAuthApi
--         , providerApi
--         , searchApis
--         , teamApis
--         , turnApi
--         ]

main = withOpenSSL $ runTests go
  where
    go b i = withResource (getOpts b i) releaseOpts $ \(b, opts) -> do
        testGroup "Brig API Integration" [ UserAuth.tests b opts ]

    getOpts bFile iFile = do
        m <- newManager tlsManagerSettings {
            managerResponseTimeout = responseTimeoutMicro 300000000
        }
        let local p = Endpoint { _epHost = "127.0.0.1", _epPort = p }
        bConf <- handleParseError =<< decodeFileEither bFile
        iConf <- handleParseError =<< decodeFileEither iFile
        b <- mkRequest <$> optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
        c <- mkRequest <$> optOrEnv cannon iConf (local . read) "CANNON_WEB_PORT"
        g <- mkRequest <$> optOrEnv galley iConf (local . read) "GALLEY_WEB_PORT"
        
        -- Taken from brig's config
        t <- optOrEnv (Opts.servers . Opts.turn) bConf id "TURN_SERVERS"
        bh <- optOrEnv (\v -> (Opts.cassandra v)^.casEndpoint.epHost) bConf pack "BRIG_CASSANDRA_HOST"
        bp <- optOrEnv (\v -> (Opts.cassandra v)^.casEndpoint.epPort) bConf read "BRIG_CASSANDRA_PORT"

        lg <- Logger.new Logger.defSettings
        db <- initCassandra (Endpoint bh bp) lg

        return (bConf, TestSetup m g c b db)

    releaseOpts _ = return ()

initCassandra :: Endpoint -> Logger -> IO Cql.ClientState
initCassandra ep lg =
    Cql.init lg $ Cql.setPortNumber (fromIntegral $ ep^.epPort)
                . Cql.setContacts (unpack (ep^.epHost)) []
                . Cql.setKeyspace (Cql.Keyspace "brig_test")
                $ Cql.defSettings

mkRequest :: Endpoint -> Request -> Request
mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p

handleParseError :: (Show a) => Either a b -> IO (Maybe b)
handleParseError (Left err) = do
  putStrLn $ "Parse failed: " ++ show err ++ "\nFalling back to environment variables"
  pure Nothing
handleParseError (Right val) = pure $ Just val
