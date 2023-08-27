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
    | status == Running && playerPosition player > (-windowWidth / 2 + 200) = 
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
-- Função  delimitar linha vertical
--

drawVerticalLine :: Picture
drawVerticalLine = color white $ line [(marginX, -windowHeigth / 2), (marginX, windowHeigth / 2)]
  where
    marginX = -windowWidth / 2 + 175

--
-- Função que renderiza o jogo
--

render (player, obstacles, _, _, status, score, obsSpeed) =  
    case status of
        Menu     -> pictures [menuPic, menuStart]
        Running  -> pictures [scorePic, levelPic, playerPic, obstaclesPic, drawVerticalLine]
        GameOver -> pictures [scorePic, levelPic, gameOverPic, gameOverRestart]
  where
    scorePic = translate (-windowWidth/2 + 5) (windowHeigth/2 - 30) $ scale 0.2 0.2 $ color white $ text $ "Score: " ++ show score
    levelPic = translate (-windowWidth/2 + 5) (windowHeigth/2 - 60) $ scale 0.2 0.2 $ color white $ text $ "Nivel: " ++ show (calculateLevel obsSpeed)
    -- playerPic = translate (playerPosition player) (-250) $ color blue $ rectangleSolid 50 80
    
    playerPic = translate (playerPosition player) (-250) $ color blue $ pictures
      [ rectangleSolid 40 70 -- Corpo do carro
      , translate (-20) (-25) $ color white $ rectangleSolid 10 20 -- Roda esquerda traseira
      , translate 20 (-25) $ color white $ rectangleSolid 10 20 -- Roda direita traseira
      , translate (-20) 25 $ color white $ rectangleSolid 10 20 -- Roda esquerda dianteira
      , translate 20 25 $ color white $ rectangleSolid 10 20 -- Roda direita dianteira
      , translate 0 0 $ color blue $ rectangleSolid 40 70
      , translate 13 73 $ color (withAlpha 0.5 yellow) $ polygon [ (-10, 0), (0, -40), (10, 0) ] -- Farol direito
      , translate (-13) 73 $ color (withAlpha 0.5 yellow) $ polygon [ (-10, 0), (0, -40), (10, 0) ] -- Farol esquerdo
      , translate 0 20 $ color (withAlpha 0.5 white) $ rectangleSolid 30 10 -- Vidro dianteiro
      , translate 0 (-15) $ color black $ rectangleSolid 30 30
      ]
    
    obstaclePic obs = translate (obstaclePosition obs) (obstacleY obs) $ color red $ rectangleSolid 50 100
    obstaclesPic = pictures $ map obstaclePic obstacles
    gameOverPic = boldText (-100) 0 white "Game Over"
    gameOverRestart = scale 0.2 0.2 $ translate (-700) (-200) $ color white $ text "Press SPACE to restart"
    menuPic = boldText (-165) 0 white "Infinite Run Game"
    menuStart = scale 0.2 0.2 $ translate (-700) (-200) $ color white $ text "Press SPACE to Start"

--
-- Função que atualiza o nível conforme velocidade dos obstáculos
--

calculateLevel :: Float -> Int
calculateLevel obsSpeed = round ((obsSpeed - initialObsSpeed) / levelIncrease) + 1
  where
    initialObsSpeed = 5
    levelIncrease = 3

--
-- Função auxiliar para transformar o texto em bold
--

boldText :: Float -> Float -> Color -> String -> Picture
boldText x y c str = 
    pictures 
    [ translate (x + dx) (y + dy) $ scale 0.3 0.3 $ color white $ text str
    | dx <- [-1, 0, 1], dy <- [-1, 0, 1]
    ]


--
-- Função que atualiza o jogo
--

update :: Float -> GameState -> GameState
update t state@(_, _, _, _, Menu, _, _) = state
update t (player, obstacles, gen, time, status, score, obsSpeed) 
    | status == GameOver = (player, obstacles, gen, time, GameOver, score, obsSpeed)
    | any (collisionDetected player) obstacles = (player, obstacles, gen, time, GameOver, score, obsSpeed)
    | time > 1  = (player, newObstacle : movedObstacles, newGen, 0, Running, newScore, newObsSpeed)
    | otherwise = (player, movedObstacles, gen, time + t, Running, score, obsSpeed)
  where
    movedObstacles = filter (\o -> obstacleY o > -400) $ map moveObstacle obstacles
    moveObstacle obs = obs { obstacleY = obstacleY obs - obsSpeed }

    (obstaclePos, newGen) = randomR (-windowWidth / 2 + 200, windowWidth / 2 - 25) gen
    newObstacle = Obstacle obstaclePos 400
    
    newScore = score + 1
    newObsSpeed = if newScore `mod` 10 == 0 then obsSpeed + 3 else obsSpeed

main :: IO ()
main = do
    g <- getStdGen
    let state = initialState g
    play (InWindow "Infinite Run Game" (round windowWidth, round windowHeigth) (10, 10)) black 60 state render handleKeys update
