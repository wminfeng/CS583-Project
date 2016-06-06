module Password where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

-- * A simple program to get a new password from the user and "save" it.

-- | Get a new password from the user.
getPass :: IO Char
getPass = do
  putStr "Input Y/N"
  getChar

-- * Only accept good passwords.

-- | Check to see if the provided password is a good one.
isGood :: Char -> Bool
isGood p = p == 'Y' || p == 'N'

-- | Get a good password from the user, or Nothing.
getGoodPass :: MaybeT IO Char
getGoodPass = do
  p <- lift getPass
  guard (isGood p)
  return p

-- | The password was bad, try again.
tryAgain :: MaybeT IO Char
tryAgain = do
  lift $ putStrLn "Wrong input value, please try again."
  getGoodPass

-- | Get a new good password from the user and save it.
newGoodPass :: MaybeT IO Char
newGoodPass = msum (getGoodPass : repeat tryAgain)
