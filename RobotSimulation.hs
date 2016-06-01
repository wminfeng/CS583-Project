module RobotSimulation where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

--sem :: Move -> Pos
--sem []      = (0,0)
--sem (Go d i:ms) = (dx*i+x,dy*i+y)
--where
-- (dx,dy) = vector d
-- (x,y)   = sem ms

--vector :: Dir -> (Int,Int)
--vector Lft = (-1,0)
--vector Rgt = (1,0)
--vector Up  = (0,1)
--vector Dwn = (0,-1)


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

--Moveto 3 4 'Seq' If Sand Pickup 'Seq' Moveto 0 0 'Seq' Drop

ifStone Sand = False

--data Robot d a = Robot {device :: d;
--            schedule :: [Action];}


-- ToDo :Since Robot needs both State and IO effects, we can modularize this as a type class that should have 3 methods: 
-- 1. getState
-- 2. putState
-- 3. IO interaction
--class (MonadState s m, MonadIO m) => MonadStateIO s m where

-- ToDo: How to add a type constraint like "sensor a" in the data type definition for Robot? 
-- data (Sensor b) => RobotState b = Robot (Energy, Pos, Load, Schedule, b) 
type Robot m a = StateT RobotState m a

--type RobotState = (Energy, Pos, Load, Schedule)
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
--  (e,(px,py),l,s) <- get
--if i+j <e then put (e-i-j,(px+i,py+j),l,s)
  RobotState e (px,py) l s w <- get
  if i+j <e then put (RobotState (e-i-j) (px+i,py+j) l s w)
            else lift mzero

pickUp :: MonadPlus m => Object -> Robot m ()
pickUp o = do
--  (e,p,l,s) <- get
--if l == Empty then put (e,p,o,s)
  RobotState e p l s w <- get
  if l == Empty then put (RobotState e p o s w)
                else lift mzero

drop :: MonadPlus m => Robot m ()
drop = do
--  (e,p,l,s) <- get
--put (e,p,Empty,s)
  RobotState e p l s w <- get
  put (RobotState e p Empty s w)


getEnergy :: Monad m => Robot m Energy
--getEnergy = do
--  RobotState e _ _ _ _ <- get
--  return e
getEnergy = liftM energy get


printEnergy :: Robot IO ()
--printEnergy = do
--RobotState e _ _ _ _ <- get
--lift $ putStrLn (show e)
printEnergy = get >>= (lift.putStrLn.show.energy)


getPos :: Monad m => Robot m Pos
--getPos = do
--  RobotState _ p,_,_) <- get
--  return p
getPos = liftM pos get

printPos :: Robot IO ()
--printPos = do
--  (_,p,_,_) <- get
--  lift $ putStrLn (show p)
printPos = get >>= (lift.putStrLn.show.pos)

getLoad :: Monad m => Robot m Load
--getLoad = do
--  (_,_,l,_) <- get
--  return l
getLoad = liftM load get

printLoad :: Robot IO ()
--printLoad = do
--  (_,_,l,_) <- get
--  lift $ putStrLn (show l)
printLoad = get >>= (lift.putStrLn.show.load)

setSchedule :: Monad m => Schedule -> Robot m ()
setSchedule s = do
  --(e,p,l,_) <- get
  --put (e,p,l,s)
  RobotState e p l _ w <- get
  put (RobotState e p l s w)

getSchedule :: Monad m => Robot m Schedule
--getSchedule = do
--  (_,_,_,s) <- get
--  return s
getSchedule = liftM schedule get

--TODO fix this
printSchedule :: Robot IO ()
--printSchedule = do
--  (_,_,_,s) <- get
--  lift $ putStrLn (show s)
printSchedule = get >>= (lift.putStrLn.show.schedule)

getWorld :: Monad m => Robot m World
getWorld = liftM world get

printWorld :: Robot IO ()
printWorld = get >>= (lift.putStrLn.show.world)

addAction :: Monad m => Action -> Robot m ()
addAction a = do
--  (e,p,l,s) <- get
--  put (e,p,l,(s ++ [a]))
--  return ()
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

--popAction :: Monad m => Robot m (Maybe Action)
--popAction = let s = getSchedule in
--              case s of
--                [] -> Nothing
--                _  -> do
--                  h <- Just (head s)
--                  --TODO: Remove the top element from the schedule
--                  h

--runSchedule