module RobotSimulation where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

-- ToDo: Based on the combinator pattern in our lecture, this should be changed to functions that return "Robot m a" as results
-- Reason: Such implemetation lacks of extensibility
data Action = Moveto Int Int
      | Pickup
      | Drop
      | GetTemp
      | GetHumidity
      | GetPressure
      | GetPicture
      | SendBackdata
      | DoNothing
      | Seq Action Action
      | If Object Action deriving (Show,Eq)

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

type Energy = Int
type Pos = (Int, Int)
type Load = Object
type Schedule = [Action]

data RobotState = RobotState { energy :: Int, pos :: (Int, Int), load :: Object, schedule :: [Action], world :: World }


data World = World { temperature :: Int, pressure :: Float, humidity :: Float, picture :: String } deriving Show

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

--run m e as = do 
--  (a,s) <- runStateT m (e,(0,0),Empty,as)


moveBy :: MonadPlus m => (Int, Int) -> Robot m ()
moveBy (i,j) = do
  RobotState e (px,py) l s w <- get
  if i+j <e then put (RobotState (e-i-j) (px+i,py+j) l s w)
            else lift mzero

pickUp :: MonadPlus m => Object -> Robot m ()
pickUp o = do
  RobotState e p l s w <- get
  if l == Empty then put (RobotState e p o s w)
                else lift mzero

drop :: MonadPlus m => Robot m ()
drop = do
  RobotState e p l s w <- get
  put (RobotState e p Empty s w)


getEnergy :: Monad m => Robot m Energy
getEnergy = liftM energy get


printEnergy :: Robot IO ()
printEnergy = get >>= (lift.putStrLn.show.energy)


getPos :: Monad m => Robot m Pos
getPos = liftM pos get

printPos :: Robot IO ()
printPos = get >>= (lift.putStrLn.show.pos)

getLoad :: Monad m => Robot m Load
getLoad = liftM load get

printLoad :: Robot IO ()
printLoad = get >>= (lift.putStrLn.show.load)

setSchedule :: Monad m => Schedule -> Robot m ()
setSchedule s = do
  RobotState e p l _ w <- get
  put (RobotState e p l s w)

getSchedule :: Monad m => Robot m Schedule
getSchedule = liftM schedule get

--TODO fix this
printSchedule :: Robot IO ()
printSchedule = get >>= (lift.putStrLn.show.schedule)
--printSchedule = peekAction >>= (lift.putStrLn.show)
--printSchedule = getSchedule >>= (lift.putStrLn.show)



getWorld :: Monad m => Robot m World
getWorld = liftM world get

printWorld :: Robot IO ()
printWorld = get >>= (lift.putStrLn.show.world)

addAction :: Monad m => Action -> Robot m ()
addAction a = do
  RobotState e p l s w <- get
  put (RobotState e p l (s ++ [a]) w)

peekAction :: Monad m => Robot m Action
peekAction = do
  s <- getSchedule
  case s of
    [] -> return DoNothing
    (s:ss)  -> return s

removeTopAction :: Monad m => Robot m ()
removeTopAction = do
  s <- getSchedule
  case s of
    [] -> return ()
    (s:ss) -> setSchedule ss

popAction :: Monad m => Robot m Action
popAction = do 
  a <- peekAction
  removeTopAction
  return a