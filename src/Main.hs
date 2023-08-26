module Main where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

data Player = Player { playerPosition :: Float }
data Obstacle = Obstacle { obstaclePosition :: Float, obstacleY :: Float }
type GameState = (Player, [Obstacle], StdGen, Float)

windowWidth :: Float
windowWidth = 800

playerSpeed :: Float
playerSpeed = 5

obstacleSpeed :: Float
obstacleSpeed = 5

initialState :: StdGen -> GameState
initialState g = (Player 0, [], g, 0)

render :: GameState -> Picture
render (player, obstacles, _, _) = 
    pictures $ playerPic : map obstaclePic obstacles
  where
    playerPic = translate (playerPosition player) (-340) $ color red $ rectangleSolid 50 50
    obstaclePic obs = translate (obstaclePosition obs) (obstacleY obs) $ color blue $ rectangleSolid 50 100

update :: Float -> GameState -> GameState
update t (player, obstacles, gen, time) 
    | time > 1  = (player, newObstacle : movedObstacles, newGen, 0)
    | otherwise = (player, movedObstacles, gen, time + t)
  where
    movedObstacles = filter (\o -> obstacleY o > -400) $ map moveObstacle obstacles
    moveObstacle obs = obs { obstacleY = obstacleY obs - obstacleSpeed }

    (obstaclePos, newGen) = randomR (-windowWidth / 2 + 25, windowWidth / 2 - 25) gen
    newObstacle = Obstacle obstaclePos 400

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (player, obs, gen, t) 
    | playerPosition player > (-windowWidth / 2 + 25) = (player { playerPosition = playerPosition player - playerSpeed * 10 }, obs, gen, t)
    | otherwise = (player, obs, gen, t)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (player, obs, gen, t) 
    | playerPosition player < (windowWidth / 2 - 25) = (player { playerPosition = playerPosition player + playerSpeed * 10 }, obs, gen, t)
    | otherwise = (player, obs, gen, t)
handleKeys _ s = s

main :: IO ()
main = do
    g <- getStdGen
    let state = initialState g
    play (InWindow "Corrida Infinita" (round windowWidth, 600) (10, 10)) white 60 state render handleKeys update
