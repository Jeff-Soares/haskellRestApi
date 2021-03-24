module Product where

data Product = Product
    { id    :: Int
    , nome  :: String
    , preco :: Double
    } deriving (Eq, Show) 

data ProdNoId = ProdNoId
    { name  :: String
    , price :: Double
    } deriving (Eq, Show) 
