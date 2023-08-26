module Main where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

data Player = Player { playerPosition :: Float }
data Obstacle = Obstacle { obstaclePosition :: Float, obstacleY :: Float }
data GameStatus = Running | GameOver deriving Eq
type GameState = (Player, [Obstacle], StdGen, Float, GameStatus)

windowWidth :: Float
windowWidth = 800

playerSpeed :: Float
playerSpeed = 5

obstacleSpeed :: Float
obstacleSpeed = 5

initialState :: StdGen -> GameState
initialState g = (Player 0, [], g, 0, Running)

render :: GameState -> Picture
render (player, obstacles, _, _, status) = 
    case status of
        Running -> pictures $ playerPic : map obstaclePic obstacles
        GameOver -> scale 0.3 0.3 $ translate (-400) 0 $ color red $ text "Game Over"
  where
    playerPic = translate (playerPosition player) (-340) $ color red $ rectangleSolid 50 50
    obstaclePic obs = translate (obstaclePosition obs) (obstacleY obs) $ color blue $ rectangleSolid 50 100

update :: Float -> GameState -> GameState
update t (player, obstacles, gen, time, status) 
    | status == GameOver = (player, obstacles, gen, time, GameOver)
    | any (collides player) obstacles = (player, obstacles, gen, time, GameOver)
    | time > 1  = (player, newObstacle : movedObstacles, newGen, 0, Running)
    | otherwise = (player, movedObstacles, gen, time + t, Running)
  where
    movedObstacles = filter (\o -> obstacleY o > -400) $ map moveObstacle obstacles
    moveObstacle obs = obs { obstacleY = obstacleY obs - obstacleSpeed }

    (obstaclePos, newGen) = randomR (-windowWidth / 2 + 25, windowWidth / 2 - 25) gen
    newObstacle = Obstacle obstaclePos 400

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (player, obs, gen, t, status)
    | status == Running && playerPosition player > (-windowWidth / 2 + 25) = 
        (player { playerPosition = playerPosition player - playerSpeed * 10 }, obs, gen, t, Running)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (player, obs, gen, t, status) 
    | status == Running && playerPosition player < (windowWidth / 2 - 25) = 
        (player { playerPosition = playerPosition player + playerSpeed * 10 }, obs, gen, t, Running)
handleKeys (EventKey (Char ' ') Down _ _) (_, _, gen, _, GameOver) = 
    initialState gen
handleKeys _ s = s

collides :: Player -> Obstacle -> Bool
collides player obstacle = 
    abs (playerPosition player - obstaclePosition obstacle) < 50 && abs (-340 - obstacleY obstacle) < 75

main :: IO ()
main = do
    g <- getStdGen
    let state = initialState g
    play (InWindow "Corrida Infinita" (round windowWidth, 600) (10, 10)) white 60 state render handleKeys update
