module Main where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

data Player = Player { playerPosition :: Float }
data Obstacle = Obstacle { obstaclePosition :: Float, obstacleY :: Float }
data GameStatus = Running | GameOver deriving Eq
type GameState = (Player, [Obstacle], StdGen, Float, GameStatus, Int)

windowWidth :: Float
windowWidth = 800

windowHeigth :: Float
windowHeigth = 600

obstacleSpeed :: Float
obstacleSpeed = 5

playerSpeed :: Float
playerSpeed = obstacleSpeed    

initialState :: StdGen -> GameState
initialState g = (Player 0, [], g, 0, Running, 0)

render :: GameState -> Picture
render (player, obstacles, _, _, status, points) =
    case status of
        Running ->
            pictures [scorePic, playerPic, obstaclesPic]
        GameOver ->
            pictures [scorePic, gameOverPic]
  where
    scorePic = translate (-windowWidth/2 + 20) (windowHeigth/2) $ scale 0.2 0.2 $ color black $ text $ "Score: " ++ show points
    playerPic = translate (playerPosition player) (-340) $ color red $ rectangleSolid 50 50
    obstaclePic obs = translate (obstaclePosition obs) (obstacleY obs) $ color blue $ rectangleSolid 50 100
    obstaclesPic = pictures $ map obstaclePic obstacles
    gameOverPic = scale 0.3 0.3 $ translate (-400) 0 $ color red $ text "Game Over"


update :: Float -> GameState -> GameState
update t (player, obstacles, gen, time, status, points) 
    | status == GameOver = (player, obstacles, gen, time, GameOver, points)
    | any (collides player) obstacles = (player, obstacles, gen, time, GameOver, points)
    | time > 1  = (player, newObstacle : movedObstacles, newGen, 0, Running, newPoints)
    | otherwise = (player, movedObstacles, gen, time + t, Running, points)
  where
    movedObstacles = filter (\o -> obstacleY o > -400) $ map moveObstacle obstacles
    moveObstacle obs = obs { obstacleY = obstacleY obs - currentObstacleSpeed }

    (obstaclePos, newGen) = randomR (-windowWidth / 2 + 25, windowWidth / 2 - 25) gen
    newObstacle = Obstacle obstaclePos 400
    
    currentObstacleSpeed = if points `mod` 10 == 0 then obstacleSpeed + 3 else obstacleSpeed --aumenta a velocidade dos obstáculos em 3 a cada 10 pts
    curret
    updatedPoints = points + 1

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (player, obs, gen, t, status, points)
    | status == Running && playerPosition player > (-windowWidth / 2 + 25) = 
        (player { playerPosition = playerPosition player - playerSpeed * 10 }, obs, gen, t, status, points)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (player, obs, gen, t, status, points) 
    | status == Running && playerPosition player < (windowWidth / 2 - 25) = 
        (player { playerPosition = playerPosition player + playerSpeed * 10 }, obs, gen, t, status, points)
handleKeys (EventKey (Char ' ') Down _ _) (_, _, gen, _, GameOver, points) = 
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
