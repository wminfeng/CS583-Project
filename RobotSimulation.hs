module RobotSimulation where
	
import Control.Monad
import Control.Monad.State 
import Control.Monad.Trans


--sem :: Move -> Pos
--sem []          = (0,0)
--sem (Go d i:ms) = (dx*i+x,dy*i+y) 
--where
-- (dx,dy) = vector d
-- (x,y)   = sem ms

--vector :: Dir -> (Int,Int)
--vector Lft = (-1,0)
--vector Rgt = (1,0)
--vector Up  = (0,1)
--vector Dwn = (0,-1)

data Action = Moveto Int Int 
			| Pickup 
			| Drop 
			| GetTemp 
			| GetHumidity 
			| GetPressure 
			| GetPicture
			| SendBackdata
			| Seq Action Action
			| If Object Action deriving (Show,Eq)
data Object = Sand | Stone deriving (Show,Eq)

--Moveto 3 4 'Seq' If Sand Pickup 'Seq' Moveto 0 0 'Seq' Drop

ifStone Sand = False

--data Robot d a = Robot {device :: d;
--						schedule :: [Action];}

--class Device a where 
	--getInfo String

type Robot m a = StateT RobotState m a
type RobotState = (Energy, Pos, Load, Schedule)

type Energy = Int
type Pos = (Int, Int)
type Load = Object
type Schedule = [Action]

getEnergy :: Monad m => Robot m Energy
getEnergy = do
	(e,_,_,_) <- get
	return e

printEnergy :: Robot IO ()
printEnergy = do
	(e,_,_,_) <- get
	lift $ putStrLn (show e)

getPos :: Monad m => Robot m Pos
getPos = do
	(_,p,_,_) <- get
	return p

printPos :: Robot IO ()
printPos = do
	(_,p,_,_) <- get
	lift $ putStrLn (show p)

getLoad :: Monad m => Robot m Load
getLoad = do
	(_,_,l,_) <- get
	return l

printLoad :: Robot IO ()
printLoad = do
	(_,_,l,_) <- get
	lift $ putStrLn (show l)

getSchedule :: Monad m => Robot m Schedule
getSchedule = do 
	(_,_,_,s) <- get
	return s 

printSchedule :: Robot IO ()
printSchedule = do
	(_,_,_,s) <- get
	lift $ putStrLn (show s)

--example = put (0,(0,0),Sand,[Pickup]) >> printSchedule

--getfunc :: Robot IO ()
--getfunc = do
--	s <- lift $ getChar
--	lift $ putStrLn (show s)








