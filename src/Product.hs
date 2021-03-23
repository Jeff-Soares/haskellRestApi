module Product where

data Product = Product
  { id    :: Int
  , name  :: String
  , price :: Double
  } deriving (Eq, Show)

products :: [Product]
products = [ Product 1 "Phone" 129
           , Product 2 "Usb Cable" 9.99
           ]