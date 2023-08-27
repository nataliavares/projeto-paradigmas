import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

--
-- Definição dos tipos de dados
--

-- teste

data Player = Player { playerPosition :: Float }

data Obstacle = Obstacle { obstaclePosition :: Float
                , obstacleY :: Float }

data GameStatus = Menu | Running | GameOver
      deriving Eq

type GameState = (Player, [Obstacle], StdGen, Float, GameStatus, Int, Float) 
-- Int é a pontuação e Float é a velocidade atual dos obstáculos


--
-- Dimensões da janela do jogo
--

windowWidth :: Float
windowWidth = 800

windowHeigth :: Float
windowHeigth = 600

obstacleSpeed :: Float
obstacleSpeed = 5

playerSpeed :: Float
playerSpeed = 5

currentObstacleSpeed :: Int -> Float
currentObstacleSpeed score 
    |score < 10 = obstacleSpeed
    |otherwise = obstacleSpeed + fromIntegral (score `div` 10)

--
-- Função para o estado inicial
--

initialState :: StdGen -> GameState
initialState g = (Player 0, [], g, 0, Menu, 0, obstacleSpeed)

--
-- Função para leitura dos inputs do teclado
--

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (_, obs, gen, t, Menu, score, obsSpeed) = 
    (Player 0, obs, gen, t, Running, score, obsSpeed)
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (_, _, gen, _, GameOver, _, _) = 
    initialState gen
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (player, obs, gen, t, status, score, obsSpeed)
    | status == Running && playerPosition player > (-windowWidth / 2 + 25) = 
        (player { playerPosition = playerPosition player - playerSpeed * 10 }, obs, gen, t, status, score, obsSpeed)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (player, obs, gen, t, status, score, obsSpeed) 
    | status == Running && playerPosition player < (windowWidth / 2 - 25) = 
        (player { playerPosition = playerPosition player + playerSpeed * 10 }, obs, gen, t, status, score, obsSpeed)
handleKeys (EventKey (Char ' ') Down _ _) (_, _, gen, _, GameOver, _, _) = 
    initialState gen
handleKeys _ s = s


--
-- Função para detectar a colisão com o obstáculo
--

collisionDetected :: Player -> Obstacle -> Bool
collisionDetected player obstacle = 
    abs (playerPosition player - obstaclePosition obstacle) < 50 && abs (-340 - obstacleY obstacle) < 75

--
-- Função que renderiza o jogo
--

render :: GameState -> Picture
render (player, obstacles, _, _, status, score, _) =  -- Adicionamos um _ para ignorar o currentObstacleSpeed
    case status of
        Menu     -> pictures [menuPic, menuStart]
        Running  -> pictures [scorePic, playerPic, obstaclesPic]
        GameOver -> pictures [scorePic, gameOverPic, gameOverRestart]
  where
    scorePic = translate (-windowWidth/2 + 20) (windowHeigth/2 - 20) $ scale 0.2 0.2 $ color black $ text $ "Score: " ++ show score
    playerPic = translate (playerPosition player) (-340) $ color red $ rectangleSolid 50 50
    obstaclePic obs = translate (obstaclePosition obs) (obstacleY obs) $ color blue $ rectangleSolid 50 100
    obstaclesPic = pictures $ map obstaclePic obstacles
    gameOverPic = boldText (-100) 0 red "Game Over"
    gameOverRestart = scale 0.2 0.2 $ translate (-700) (-200) $ color black $ text "Press SPACE to restart"
    menuPic = boldText (-165) 0 black "Infinite Run Game"
    menuStart = scale 0.2 0.2 $ translate (-700) (-200) $ color black $ text "Press SPACE to Start"

--
-- Função auxiliar para transformar o texto em bold
--

boldText :: Float -> Float -> Color -> String -> Picture
boldText x y c str = 
    pictures 
    [ translate (x + dx) (y + dy) $ scale 0.3 0.3 $ color c $ text str
    | dx <- [-1, 0, 1], dy <- [-1, 0, 1]
    ]


--
-- Função que atualiza o jogo
--

update :: Float -> GameState -> GameState
update t state@(_, _, _, _, Menu, _) = state
update t (player, obstacles, gen, time, status, score)
    | status == GameOver = (player, obstacles, gen, time, GameOver, score)
    | any (collisionDetected player) obstacles = (player, obstacles, gen, time, GameOver, score)
    | time > 1  = (player, newObstacle : movedObstacles, newGen, 0, Running, newScore)
    | otherwise = (player, movedObstacles, gen, time + t, Running, score)
  where
    movedObstacles = filter (\o -> obstacleY o > -400) $ map moveObstacle obstacles
    moveObstacle obs = obs { obstacleY = obstacleY obs - currentSpeed }

    (obstaclePos, newGen) = randomR (-windowWidth / 2 + 25, windowWidth / 2 - 25) gen
    newObstacle = Obstacle obstaclePos 400
    
    newScore = score + 1
    currentSpeed = currentObstacleSpeed newScore --aumenta a velocidade dos obstáculos em 3 a cada 10 pts



main :: IO ()
main = do
    g <- getStdGen
    let state = initialState g
    play (InWindow "Infinite Run Game" (round windowWidth, 400) (10, 10)) white 60 state render handleKeys update

