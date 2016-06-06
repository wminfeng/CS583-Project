module RobotSimulation where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Prelude hiding (drop)
import Password 

-- ToDo: Based on the combinator pattern in our lecture, this should be changed to functions that return "Robot m a" as results
-- Reason: Such implemetation lacks of extensibility
data Action = MoveBy Pos
      | Pickup Object
      | Drop
      | Getdata
      | DoNothing
--      | If Object Action 
      deriving (Show,Eq)
data Object = Sand | Stone | Empty deriving (Show,Eq)

ifStone Sand = False


-- ToDo :Since Robot needs both State and IO effects, we can modularize this as a type class that should have 3 methods: 
-- 1. getState
-- 2. putState
-- 3. IO interaction
--class (MonadState s m, MonadIO m) => MonadStateIO s m where

-- ToDo: How to add a type constraint like "sensor a" in the data type definition for Robot? 
-- data (Sensor b) => RobotState b = Robot (Energy, Pos, Load, Schedule, b) 
type Robot m a = StateT RobotState m a
type RobotE a = Robot (MaybeT IO) a


type Energy = Int
type Pos = (Int, Int)
type Load = Object
type Schedule = [Action]

data RobotState = RobotState { energy :: Energy, pos :: Pos, load :: Load, schedule :: Schedule, world :: World }

instance Show RobotState where
  show (RobotState e p l s w) = "Current Energy is " ++ show e ++
                                ".\nCurrent Position is " ++ show p ++
                                ".\nCurrent Load is " ++ show l ++
                                ".\nCurrent Schedule is " ++ show s ++
                                ".\n\n" ++ show w 


data World = World { temperature :: Int, pressure :: Float, humidity :: Float, picture :: String }

instance Show World where
  show (World t p h ph) = "Temperature is " ++ show t ++
                         ".\nPressure is " ++ show p ++
                         ".\nHumidity is " ++ show h ++
                         ".\nPrinting a picture : \n " ++ show ph 



type Sensor a = World -> a

thermometer :: Sensor Int
thermometer w = temperature w

barometer :: Sensor Float
barometer w = pressure w

hygrometer :: Sensor Float
hygrometer w = humidity w

camera :: Sensor String
camera w = picture w

--moveTo :: Monad m => Pos -> Robot m ()
--moveTo i = do
--  (e,p,l,s)

-- Since all the Sensors should have the same behavior: get data from the real world, it should be like IO Monad. 
-- we want to use IO Monad to simulate it.
-- ToDo: 1. a is never used but helps to pass the typechecker
--       2. Whether there is a nice way?
--class Sensor a where
--  getInfo :: a -> IO String 

--instance Sensor Thermometer where
--  getInfo a = getLine

--data Thermometer = Thermometer

--instance Sensor Hygrometer where
--  getInfo a = getLine

--data Hygrometer = Hygrometer

--instance Sensor Barometer where
--  getInfo a = getLine

--data Barometer = Barometer

--instance Sensor Camera where
--  getInfo a = getLine

--data Camera = Camera

run :: Show a => Energy -> Pos -> RobotE a -> IO ()
run e p m = do 
  a <- runMaybeT (runStateT m (RobotState e p Empty [] (World 0 0.0 0.0 "")))
  case a of
    Just (a,s) -> putStrLn ("Final state is " ++ show s ++ ".")
    Nothing -> putStrLn ""

runAction :: Action -> RobotE ()
runAction (MoveBy p) = moveBy p
runAction (Pickup o) = pickUp o
runAction Drop = drop
runAction Getdata = getWorld >> return ()
runAction DoNothing = return () 

runSchedule :: Schedule -> RobotE ()
runSchedule (a:as) = runAction a >> runSchedule as
runSchedule _ = return () 

run' :: Energy -> Pos -> Schedule -> IO ()
run' e p sc = do 
  a <- runMaybeT (runStateT (runSchedule sc) (RobotState e p Empty [] (World 0 0.0 0.0 "")))
  case a of
    Just (a,s) -> putStrLn ("Final state is " ++ show s ++ ".")
    Nothing -> putStrLn ""

moveBy :: (Int, Int) -> RobotE ()
moveBy (i,j) = do
    e <- getEnergy
    (px,py) <- getPos
    if i+j < e 
      then do
        setEnergy (e-i-j)
        setPos (px+i, py+j)
      else 
        ((lift.lift.putStrLn) ("Error: Not Enough Energy!" ++ "Current Energy is " ++ show e ++ ", need reCharge 10 ? ") >> 
        lift newGoodPass >>= 
                          (\a -> if a == 'Y' 
                                 then setEnergy (e+10) >> moveBy (i,j)
                                 else ( get >>= (\currentState -> (lift.lift.putStrLn.show) currentState) >> lift mzero)))
--  (e,(px,py),l,s) <- get
--if i+j <e then put (e-i-j,(px+i,py+j),l,s)
  --RobotState e (px,py) l s w <- get
  --if i+j <e 
  --then put (RobotState (e-i-j) (px+i,py+j) l s w)
  --else  
  --    ((lift.lift.putStrLn) ("Error: Not Enough Energy!" ++ "Current Energy is " ++ show e ++ ", need reCharge 10 ? ") >> 
  --     lift newGoodPass >>= 
  --                        (\a -> if a == 'Y' 
  --                               then put (RobotState (e+10) (px,py) l s w) >> moveBy (i,j)
  --                               else ( get >>= (\currentState -> (lift.lift.putStrLn.show) currentState) >> lift mzero)))

pickUp :: Object -> RobotE ()
pickUp o = do
  l <- getLoad
  if l == Empty then setLoad o
                else lift . lift . putStrLn $ "Error: Already have an object!"

--  (e,p,l,s) <- get
--if l == Empty then put (e,p,o,s)
  --RobotState e p l s w <- get
--  if l == Empty then put (RobotState e p o s w)
--                else lift.lift.putStrLn $ "Error: Already have an object!"

--stateT maybeT IO

drop :: RobotE ()
drop = do
  setLoad Empty

setEnergy :: Energy -> RobotE ()
setEnergy e = do
  RobotState _ p l s w <- get
  put (RobotState e p l s w)

getEnergy :: RobotE Energy
--getEnergy = do
--  RobotState e _ _ _ _ <- get
--  return e
getEnergy = do
  e <- liftM energy get
  lift.lift.putStrLn.show $ e
  return e

--printEnergy :: Robot IO ()
----printEnergy = do
----RobotState e _ _ _ _ <- get
----lift $ putStrLn (show e)
--printEnergy = get >>= (lift.putStrLn.show.energy)

setPos :: Pos -> RobotE ()
setPos p = do
  RobotState e _ l s w <- get
  put (RobotState e p l s w)

setLoad :: Load -> RobotE ()
setLoad l = do
  RobotState e p _ s w <- get
  put (RobotState e p l s w)

getPos :: RobotE Pos
--getPos = do
--  RobotState _ p,_,_) <- get
--  return p
getPos = do
  p <- liftM pos get
  lift.lift.putStrLn.show $ p
  return p

--printPos :: Robot IO ()
----printPos = do
----  (_,p,_,_) <- get
----  lift $ putStrLn (show p)
--printPos = get >>= (lift.putStrLn.show.pos)

getLoad :: RobotE Load
--getLoad = do
--  (_,_,l,_) <- get
--  return l
getLoad = do
  l <- liftM load get
  lift.lift.putStrLn.show $ l
  return l

--printLoad :: Robot IO ()
----printLoad = do
----  (_,_,l,_) <- get
----  lift $ putStrLn (show l)
--printLoad = get >>= (lift.putStrLn.show.load)

setSchedule :: Schedule -> RobotE ()
setSchedule s = do
  RobotState e p l _ w <- get
  put (RobotState e p l s w)

getSchedule :: RobotE Schedule
--getSchedule = do
--  (_,_,_,s) <- get
--  return s
getSchedule = do
  s <- liftM schedule get
  lift . lift . putStrLn . show $ s
  return s


--TODO fix this
--printSchedule :: Robot IO ()
----printSchedule = do
----  (_,_,_,s) <- get
----  lift $ putStrLn (show s)
--printSchedule = get >>= (lift.putStrLn.show.schedule)

setWorld :: World -> RobotE ()
setWorld w = do
  RobotState e p l s _ <- get
  put (RobotState e p l s w)

getWorld :: RobotE World
getWorld = do
  w <- liftM world get
  lift . lift . putStrLn . show $ w
  return w


--printWorld :: Robot IO ()
--printWorld = get >>= (lift.putStrLn.show.world)

addAction :: Action -> RobotE ()
addAction a = do
  RobotState e p l s w <- get
  put (RobotState e p l (s ++ [a]) w)

peekAction :: RobotE Action
peekAction = do
  s <- getSchedule
  case s of
    [] -> return DoNothing
    (s:ss)  -> return s

removeTopAction :: RobotE ()
removeTopAction = do
  s <- getSchedule
  case s of
    [] -> return ()
    (s:ss) -> setSchedule ss

popAction :: RobotE Action
popAction = do 
  a <- peekAction
  removeTopAction
  return a

--setTemp :: Monad m => Int -> Robot m ()
--setTemp t = do
--  World _ p h pic <- getWorld
--  setWorld (World t p h pic)

--getTemp :: Monad m => Robot m Int
--getTemp = liftM temperature getWorld


--setPressure :: Monad m => Float -> Robot m ()
--setPressure p = do
--  World t _ h pic <- getWorld
--  setWorld (World t p h pic)

--getPressure :: Monad m => Robot m Float
--getPressure = liftM pressure getWorld

--setHumidity :: Monad m => Float -> Robot m ()
--setHumidity h = do
--  World t p _ pic <- getWorld
--  setWorld (World t p h pic)

--getHumidity :: Monad m => Robot m Float
--getHumidity = liftM humidity getWorld

--setPicture :: Monad m => String -> Robot m ()
--setPicture pic = do
--  World t p h _ <- getWorld
--  setWorld (World t p h pic)

--getPicture :: Monad m => Robot m String
--getPicture = liftM picture getWorld

