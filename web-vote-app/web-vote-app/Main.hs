{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Monoid ((<>))
import Data.List
import Data.Char
import Control.Applicative
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe

import Text.Hastache 
import Text.Hastache.Context 
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Web.Scotty as Scotty
import Network.HTTP.Types.Status
import qualified Web.Scotty.Trans as ST
import Network.HTTP.Types
import System.Environment
import qualified Data.Text as DT
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as TL
import qualified Database.Redis as Redis
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import System.IO
import Web.Scotty.Cookie
import qualified Data.ByteString as BS
import Data.Word8
import System.Random
import  Data.Aeson
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy as B
import Network.HostName

insertar :: Redis.Connection -> [BS.ByteString] -> IO ()
insertar conn js = Redis.runRedis conn $ do
               Redis.rpush "votes" js
               return ()

env :: IO L.Text
env = do
        ambiente <- getEnvironment
        return $ L.pack $ foldl toString "" ambiente
    where 
     toString :: String -> (String, String) -> String
     toString xs (llave, valor)  = xs++" "++llave++"="++valor

conectarRedis :: String -> IO Redis.Connection
conectarRedis ip = liftIO $ Redis.connect $ Redis.defaultConnectInfo { Redis.connectHost = ip}
        
getAmbiente :: Scotty.ActionM ()
getAmbiente = do
           ambiente <- liftIO env
           ST.text ambiente

getIndex :: String  -> Info ->  Scotty.ActionM ()
getIndex plantilla info = do 
            x <- hastacheStr defaultConfig (encodeStr plantilla) (mkStrContext (\ x -> context x info))
            ST.html x
           where
            context "option_a" info = MuVariable ((option_a info) :: String)
            context "option_b" info = MuVariable ((option_b info) :: String)
            context "hostname" info = MuVariable ((hostname info) :: String)
            context "node" info = MuVariable ((node info) :: String)
            context "vote" info = MuVariable ("aa" :: String)
            context _ _ =  MuVariable ("-----No Encontrado-----" :: String)

data Voto = Voto { voter_id :: DT.Text , vote :: DT.Text , ts :: DT.Text  } deriving (Show)

instance ToJSON Voto where
  toJSON (Voto voter_id vote ts) = object [ "voter_id" .= voter_id, "vote" .= vote,  "ts" .= ts]

  toEncoding Voto{..} = pairs $
    "voter_id" .= voter_id <> "vote" .= vote <>  "ts" .= ts

instance FromJSON Voto where
  parseJSON (Object v) = Voto <$>
        v .: "voter_id"
    <*> v .: "vote"
    <*> v .: "ts"
  parseJSON _ = empty

            
postIndex :: String -> Redis.Connection -> Info ->Scotty.ActionM ()
postIndex plantilla conn info = do
             str <- liftIO $ fmap (DT.pack) (getRandomString 32)
             
             voter_id <- liftM (fromMaybe $ str) $ getCookie ( "voter_id" :: DT.Text)
             vote <- (Scotty.param ("vote" :: L.Text) :: Scotty.ActionM L.Text ) `Scotty.rescue` (const Scotty.next)
             
             setSimpleCookie "voter_id" voter_id
             
             epoch_time_ms <- liftIO $ (DT.pack . show . round)  `fmap` getPOSIXTime
             let voto = Voto voter_id (L.toStrict vote) epoch_time_ms
             liftIO $ insertar conn [( B.toStrict $ encode voto)]
             
             y <- liftIO $ p plantilla vote info
             
             ST.html y
             
             where 
                getRandomString :: Int -> IO [Char]
                getRandomString n = do
                    seed  <- newStdGen
                    let rs = randomlist n seed
                    return $  map ( chr . (\ x -> (mod x 27)+97) . abs )rs
 
                randomlist :: Int -> StdGen -> [Int]
                randomlist n = take n . unfoldr (Just . random)
                
                context voto "option_a" info = MuVariable ((option_a info) :: String)
                context voto "option_b" info = MuVariable ((option_b info) :: String)
                context voto "hostname" info = MuVariable ((hostname info) :: String)
                context voto "node" info = MuVariable ((node info) :: String)
                context voto "vote" info = MuVariable (voto :: L.Text)
                context _ _ _=  MuVariable ("-----No Encontrado-----" :: String)

                p :: Control.Monad.IO.Class.MonadIO m => String -> L.Text-> Info -> m L.Text
                p plantilla voto info = liftIO $ hastacheStr defaultConfig (encodeStr plantilla) (mkStrContext (\ x -> context voto x info))

data Info = Info { option_a :: String, option_b :: String, hostname:: String, node :: String }
             
router :: String -> Redis.Connection-> Info -> Scotty.ScottyM ()
router plantilla conn info = do
        ST.middleware logStdoutDev
        ST.middleware static
        ST.get "/env" getAmbiente
        ST.get "/" $ getIndex plantilla info
        ST.post "/" $ postIndex plantilla conn info

main :: IO()
main = do
    webVoteNumber <- fmap (fromMaybe "01") $ lookupEnv "WEB_VOTE_NUMBER"
    opcionA <- fmap (fromMaybe "Haskell") $ lookupEnv "OPTION_A"
    opcionB <- fmap (fromMaybe "Racket") $ lookupEnv "OPTION_B"
    puerto <- fmap read $ fmap (fromMaybe "8080") $ lookupEnv "PORT"
    hostName <- getHostName
    conn <- conectarRedis $ "redis" ++ webVoteNumber
    plantilla <- abrirArchivo "templates/index.html"
    Scotty.scotty puerto $ router plantilla conn $ Info opcionA opcionB hostName webVoteNumber
    where
        abrirArchivo :: String -> IO String
        abrirArchivo str = do 
            -- openFile :: FilePath -> IOMode -> IO Handle
            hd <- openFile str ReadMode
            -- hGetContents :: Handle -> IO String
            template <- hGetContents hd
            return template
