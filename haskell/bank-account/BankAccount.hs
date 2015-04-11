module BankAccount (BankAccount,openAccount,closeAccount, getBalance, incrementBalance ) where

import Data.IORef

data BankAccount = BankAccount { balance :: IORef (Maybe Integer) }

openAccount :: IO BankAccount
openAccount = do
  n <- newIORef (Just 0)
  return (BankAccount n)

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readIORef . balance

closeAccount :: BankAccount -> IO ()
closeAccount acct = writeBalance acct Nothing

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance acct n = do
  m <- getBalance acct
  writeBalance acct (fmap (+n) m)
  getBalance acct

writeBalance :: BankAccount -> Maybe Integer -> IO ()
writeBalance acct mi = writeIORef (balance acct) mi
