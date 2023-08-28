import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

--
-- Definição dos tipos de dados
--

data Player = Player { playerX :: Float }

data Obstacle = Obstacle { obstacleX :: Float
                , obstacleY :: Float }

data GameStatus = Menu | Running | GameOver
      deriving Eq

type GameState = (Player
                 , [Obstacle]
                 , StdGen -- gerador de números aleatórios
                 , Float -- tempo
                 , GameStatus
                 , Int -- score
                 , Float -- velocidade atual
                 , Int) -- score record

--
-- Dimensões da janela do jogo
--

windowWidth :: Float
windowWidth = 800

windowHeigth :: Float
windowHeigth = 600

--
-- Velocidade do jogador e dos obstáculos
--

obstacleSpeed :: Float
obstacleSpeed = 5

playerSpeed :: Float
playerSpeed = 5

--
-- Score Record inicial (começa em zero sempre)
--

restartRecord :: IO Int
restartRecord = return 0

--------------------------------
-- FUNÇÕES PRINCIPAIS DO JOGO --
--------------------------------

--
-- Função para o estado inicial
--

initialState :: StdGen -> Int -> GameState
initialState g record = (Player 0, [], g, 0, Menu, 0, obstacleSpeed, record)

--
-- Função para leitura dos inputs do teclado
--

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (_, obs, gen, t, Menu, score, obsSpeed, record) = 
    (Player 0, obs, gen, t, Running, score, obsSpeed, record)
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (_, _, gen, _, GameOver, _, _, record) = 
    initialState gen record
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (player, obs, gen, t, status, score, obsSpeed, record)
    | status == Running && playerX player > (-windowWidth / 2 + 200) = 
        (player { playerX = playerX player - playerSpeed * 10 }, obs, gen, t, status, score, obsSpeed, record)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (player, obs, gen, t, status, score, obsSpeed, record) 
    | status == Running && playerX player < (windowWidth / 2 - 25) = 
        (player { playerX = playerX player + playerSpeed * 10 }, obs, gen, t, status, score, obsSpeed, record)
handleKeys _ s = s


--
-- Função que retorna o carrinho do player com a cor desejada
--

drawCarPlayer :: Player -> Color -> Picture
drawCarPlayer p c = translate (playerX p) (-250) $ pictures
      [ translate (-20) (-25) $ color white $ rectangleSolid 10 20 -- Roda esquerda traseira
      , translate 20 (-25) $ color white $ rectangleSolid 10 20 -- Roda direita traseira
      , translate (-20) 25 $ color white $ rectangleSolid 10 20 -- Roda esquerda dianteira
      , translate 20 25 $ color white $ rectangleSolid 10 20 -- Roda direita dianteira
      , translate 0 0 $ color c $ rectangleSolid 40 70 -- Corpo do carro
      , translate 13 73 $ color (withAlpha 0.5 yellow) $ polygon [ (-10, 0), (0, -40), (10, 0) ] -- Farol direito
      , translate (-13) 73 $ color (withAlpha 0.5 yellow) $ polygon [ (-10, 0), (0, -40), (10, 0) ] -- Farol esquerdo
      , translate 0 20 $ color (withAlpha 0.5 white) $ rectangleSolid 30 10 -- Vidro dianteiro
      , translate 0 (-15) $ color black $ rectangleSolid 30 30 -- Teto do carro
      ]    
--
-- Função que retorna o carrinho obstáculo
--

drawCarObstacle :: Obstacle -> Picture
drawCarObstacle o = translate (obstacleX o) (obstacleY o) $ pictures
      [ translate (-20) (-25) $ color white $ rectangleSolid 10 20 -- Roda esquerda traseira
      , translate 20 (-25) $ color white $ rectangleSolid 10 20 -- Roda direita traseira
      , translate (-20) 25 $ color white $ rectangleSolid 10 20 -- Roda esquerda dianteira
      , translate 20 25 $ color white $ rectangleSolid 10 20 -- Roda direita dianteira
      , translate 0 0 $ color red $ rectangleSolid 40 70 -- Corpo do carro
      , translate 13 (-73) $ color (withAlpha 0.5 yellow) $ polygon [ (-10, 0), (0, 40), (10, 0) ] -- Farol direito
      , translate (-13) (-73) $ color (withAlpha 0.5 yellow) $ polygon [ (-10, 0), (0, 40), (10, 0) ] -- Farol esquerdo
      , translate 0 (-20) $ color (withAlpha 0.5 white) $ rectangleSolid 30 10 -- Vidro dianteiro
      , translate 0 15 $ color black $ rectangleSolid 30 30 -- Teto do carro
      ]

--
-- Função que imprime o jogo completo
--

drawGame :: GameState -> Picture
drawGame (player, obstacles, _, _, status, score, obsSpeed, record) =  
    case status of
        Menu     -> pictures [menuPic, menuStart]
        Running  -> pictures [scorePic, levelPic, recordPic, playerPic, obstaclesPic, drawVerticalLine]
        GameOver -> pictures [scorePic, levelPic, recordPic, gameOverPic, gameOverRestart]
  where
    scorePic = normalText (-windowWidth/2 + 5) (windowHeigth/2 - 30) white ("Score: " ++ show score)
    levelPic = normalText (-windowWidth/2 + 5) (windowHeigth/2 - 60) white ("Level: " ++ show (calculateLevel obsSpeed))
    recordPic = normalText (-windowWidth/2 + 5) (windowHeigth/2 - 120) white ("Record: " ++ show record)
    playerPic = drawCarPlayer player blue
    obstaclesPic = pictures $ map drawCarObstacle obstacles
    gameOverPic = boldText (-100) 0 white "Game Over"
    gameOverRestart = normalText (-145) (-50) white "Press SPACE to restart"
    menuPic = boldText (-165) 0 white "Infinite Run Game"
    menuStart = normalText (-135) (-50) white "Press SPACE to Start"

--
-- Função que atualiza o jogo
--

updateGame :: Float -> GameState -> GameState
updateGame _ state@(_, _, _, _, Menu, _, _, _) = state
updateGame t (player, obstacles, gen, time, status, score, obsSpeed, record) 
    | status == GameOver = (player, obstacles, gen, time, GameOver, score, obsSpeed, newRecord)
    | any (collisionDetected player) obstacles = (player, obstacles, gen, time, GameOver, score, obsSpeed, record)
    | time > 1  = (player, newObstacle : movedObstacles, newGen, 0, Running, newScore, newObsSpeed, record)
    | otherwise = (player, movedObstacles, gen, time + t, Running, score, obsSpeed, record)
  where
    movedObstacles = filter (\o -> obstacleY o > -400) $ map moveObstacle obstacles
    moveObstacle obs = obs { obstacleY = obstacleY obs - obsSpeed }

    (obstaclePos, newGen) = randomR (-windowWidth / 2 + 200, windowWidth / 2 - 25) gen
    newObstacle = Obstacle obstaclePos 400
    newObsSpeed = if newScore `mod` 10 == 0 then obsSpeed + 2 else obsSpeed

    newScore = score + 1
    newRecord = max score record

---------------------------------------------------
-- FUNÇÕES AUXILIARES PARA IMPLEMENTAÇÃO DO JOGO --
---------------------------------------------------

--
-- Função para detectar a colisão com o obstáculo
--

collisionDetected :: Player -> Obstacle -> Bool
collisionDetected player obstacle = 
    abs (playerX player - obstacleX obstacle) < 50 && abs (-340 - obstacleY obstacle) < 170

--
-- Função que atualiza o nível conforme velocidade dos obstáculos
--

calculateLevel :: Float -> Int
calculateLevel obsSpeed = round ((obsSpeed - initialObsSpeed) / levelIncrease) + 1
  where
    initialObsSpeed = 5
    levelIncrease = 3

----------------------------------
-- FUNÇÕES AUXILIARES DE LAYOUT --
----------------------------------

--
-- Função auxiliar para texto em formato normal
--

normalText :: Float -> Float -> Color -> String -> Picture
normalText x y c str = 
    translate x y $ scale 0.2 0.2 $ color c $ text str

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
-- Função  delimitar linha vertical
--

drawVerticalLine :: Picture
drawVerticalLine = color white $ line [(marginX, -windowHeigth / 2), (marginX, windowHeigth / 2)]
  where
    marginX = -windowWidth / 2 + 175


----------------------------
-- JOGO CORRIDA INFINITA  --
----------------------------


main :: IO ()
main = do
    g <- getStdGen
    record <- restartRecord
    let state = initialState g record
    play (InWindow "Infinite Run Game" (round windowWidth, round windowHeigth) (10, 10)) black 60 state drawGame handleKeys updateGame
