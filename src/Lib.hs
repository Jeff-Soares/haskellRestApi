{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Database.SQLite.Simple
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.IO.Class
import Data.String
import qualified Data.Text as T

import Product

$(deriveJSON defaultOptions ''Product)
$(deriveJSON defaultOptions ''ProdNoId)

instance FromRow Product where
    fromRow = Product <$> field <*> field <*> field
instance FromRow ProdNoId where
    fromRow = ProdNoId <$> field <*> field

type API = "products" :> 
  (
    Get '[JSON] [Product]
  :<|>
    Capture "id" Int :> Get '[JSON] [Product]
  :<|> 
    ReqBody '[JSON] ProdNoId :> Post '[JSON] NoContent
  )

api :: Proxy API
api = Proxy

app :: Server API -> Application
app server = serve api server

server :: FilePath  -> Server API
server dbfile = fullProducts :<|> singleProduct :<|> addProduct
    where
        fullProducts :: Handler [Product]
        fullProducts = liftIO $
            withConnection dbfile $ \conn ->
                query_ conn "SELECT * FROM products" :: IO [Product]

        singleProduct :: Int -> Handler [Product]
        singleProduct n = liftIO $ 
            withConnection dbfile $ \conn ->
                query_ conn $ fromString $ "SELECT * FROM products where id=" ++ show n

        addProduct :: ProdNoId -> Handler NoContent
        addProduct p = do
            liftIO . withConnection dbfile $ \conn -> 
                execute_ conn $
                    fromString $
                        "INSERT INTO products (name, price) VALUES ('"
                        ++ Product.name p ++ "',"
                        ++ show (Product.price p) ++ ")"
            return NoContent
            

startApp :: FilePath -> IO ()
startApp dbfile = do
    putStrLn "Running on port: 8080"
    run 8080 (app $ server dbfile )
