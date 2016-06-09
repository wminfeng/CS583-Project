CS583 Project

GitHub Link: https://github.com/wminfeng/CS583-Project

Goal: This project's goal is to create a Domain Specific Language (DSL) for a robot. More specifically, the robot in question is similar to the Mars Rover. It moves, collects soil, temperature, pressure, etc and sends data back to its operators.

Building: Building this project is very easy, at the moment. Simply start up GHCI and load the file.


-- Example 1:
-- >>> run 20 (1,1) $ moveBy (10,10) >> pickUp Sand >> getWorld
-- 20
-- (1,1) 
-- Error: Not Enough Energy! Current Energy is 20, need reCharge 10 ?
-- Input Y/N
-- >>> Y
-- 30
-- (1,1)
-- Empty
-- Temperature is 0.
-- pressure is 0.0.
-- Humidity is 0.0.
-- Printing a picture: ""
--
-- Final State:
-- Current Energy is 10.
-- Current Position is (11,11).
-- Current Load is Sand.
-- Current Schedule is [].
-- Temperature is 0.
-- Pressure is 0.0.
-- Humidity is 0.0.
-- Printing a picture: "".

-- Example 2:
-- >>> run' 20 (1,1) [MoveBy (9,9), PickUp Sand, PickUp Stone, GetData]
-- 20
-- (1,1)
-- Empty
-- Sand
-- Error: Already has an object!
-- Temperature is 0.
-- pressure is 0.0.
-- Humidity is 0.0.
-- Printing a picture: ""
--
-- Final State:
-- Current Energy is 2.
-- Current Position is (10,10).
-- Current Load is Sand.
-- Current Schedule is [].
-- Temperature is 0.
-- Pressure is 0.0.
-- Humidity is 0.0.
-- Printing a picture: "".

-- Example 3:
-- >>> run 20 (1,1) $ moveBy (2,2) >> moveBy (5,5) >> moveBy (7,7) >> pickUp Stone
-- 20
-- (1,1)
-- 16
-- (3,3)
-- 6
-- (8,8)
-- Error: Not Enough Energy! Current Energy is 20, need reCharge 10 ?
-- Input Y/N
-- >>> N
-- 
-- Current Energy is 6.
-- Current Position is (8,8).
-- Current Load is Empty.
-- Current Schedule is [].
-- Temperature is 0.
-- Pressure is 0.0.
-- Humidity is 0.0.
-- Printing a picture: ""