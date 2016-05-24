module Main where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
-------------------------------------------
data MyContext = MyContext
  { foo :: String
  , bar :: Int
  } deriving (Show)

computation :: Int -> Reader MyContext (Maybe String)
computation a = do
  n <- asks bar
  x <- asks foo
  if n > a
    then return (Just x)
    else return Nothing

ex1 :: Maybe String
ex1 = runReader (computation 2) $ MyContext "hello" 1

ex2 :: Maybe String
ex2 = runReader (computation 4) $ MyContext "haskell" 0
------
data Conf = Conf {
    puerto :: String,
    host :: String,
    otros :: [String]
}

conectar :: Int -> Reader Conf String
conectar p = do
    conf <- ask
    pu <- asks puerto
    return pu 
    
example :: Writer String String
example  = do
  tell "Hoy hice"
  tell " Nada"
  tell " Adios"
  return $ ""

output :: (String, String)
output = runWriter example
----------------------------------

test :: State Conf Int
test = do
  modify $ (\ st -> st {puerto = "4505"})
  return 5

main :: IO ()
main = print $ evalState test $ Conf {puerto = "",host = "",otros = [""]}
