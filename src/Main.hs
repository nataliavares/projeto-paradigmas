import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

--
-- Definição dos tipos de dados
--

data Player = Player { playerX :: Float
                     , playerColor :: Color }


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

returnPlayer :: IO Player
returnPlayer = return (Player 0 blue)

--------------------------------
-- FUNÇÕES PRINCIPAIS DO JOGO --
--------------------------------

--
-- Função para o estado inicial
--

initialState :: Player -> StdGen -> Int -> GameState
initialState player g record = (player {playerX = 0} , [], g, 0, Menu, 0, obstacleSpeed, record)


--
-- Função para leitura dos inputs do teclado
--

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (player, obs, gen, t, Menu, score, obsSpeed, record) = 
    (player, obs, gen, t, Running, score, obsSpeed, record) -- Cor padrão ao iniciar
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (player, _, gen, _, GameOver, _, _, record) = 
    initialState player gen record
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (player, obs, gen, t, status, score, obsSpeed, record)
    | status == Running && playerX player > (-windowWidth / 2 + 200) = 
        (player { playerX = playerX player - playerSpeed * 10 }, obs, gen, t, status, score, obsSpeed, record)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (player, obs, gen, t, status, score, obsSpeed, record) 
    | status == Running && playerX player < (windowWidth / 2 - 25) = 
        (player { playerX = playerX player + playerSpeed * 10 }, obs, gen, t, status, score, obsSpeed, record)
handleKeys (EventKey (Char 'b') Down _ _) (player, obs, gen, t, Menu, score, obsSpeed, record) = 
    (player { playerColor = blue }, obs, gen, t, Menu, score, obsSpeed, record)
handleKeys (EventKey (Char 'g') Down _ _) (player, obs, gen, t, Menu, score, obsSpeed, record) = 
    (player { playerColor = green }, obs, gen, t, Menu, score, obsSpeed, record)
handleKeys (EventKey (Char 'y') Down _ _) (player, obs, gen, t, Menu, score, obsSpeed, record) = 
    (player { playerColor = yellow }, obs, gen, t, Menu, score, obsSpeed, record)
handleKeys (EventKey (Char 'B') Down _ _) (player, obs, gen, t, Menu, score, obsSpeed, record) = 
    (player { playerColor = blue }, obs, gen, t, Menu, score, obsSpeed, record)
handleKeys (EventKey (Char 'G') Down _ _) (player, obs, gen, t, Menu, score, obsSpeed, record) = 
    (player { playerColor = green }, obs, gen, t, Menu, score, obsSpeed, record)
handleKeys (EventKey (Char 'Y') Down _ _) (player, obs, gen, t, Menu, score, obsSpeed, record) = 
    (player { playerColor = yellow }, obs, gen, t, Menu, score, obsSpeed, record)
handleKeys _ s = s


--
-- Função que retorna o carrinho do player com a cor desejada
--

drawCarPlayer :: Player -> Picture
drawCarPlayer p = translate (playerX p) (-250) $ pictures
      [ translate (-20) (-25) $ color white $ rectangleSolid 10 20 -- Roda esquerda traseira
      , translate 20 (-25) $ color white $ rectangleSolid 10 20 -- Roda direita traseira
      , translate (-20) 25 $ color white $ rectangleSolid 10 20 -- Roda esquerda dianteira
      , translate 20 25 $ color white $ rectangleSolid 10 20 -- Roda direita dianteira
      , translate 0 0 $ color (playerColor p) $ rectangleSolid 40 70 -- Corpo do carro
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
    playerPic = drawCarPlayer player
    obstaclesPic = pictures $ map drawCarObstacle obstacles
    -- Texto tela Game Over e Menu (score,level e record)    
    scorePic = normalText (0.2) (-windowWidth/2 + 5) (windowHeigth/2 - 30) white ("Score: " ++ show score)
    levelPic = normalText (0.2) (-windowWidth/2 + 5) (windowHeigth/2 - 60) white ("Level: " ++ show (calculateLevel obsSpeed))
    recordPic = normalText (0.2) (-windowWidth/2 + 5) (windowHeigth/2 - 120) white ("Record: " ++ show record)
    -- Texto tela Game Over
    gameOverPic = boldText (-100) 0 white "Game Over"
    gameOverRestart = normalText (0.2) (-145) (-50) white "Press SPACE to restart"
    -- Texto tela menu
    menuPic = boldText (-165) 40 white "Infinite Run Game"
    menuStart = pictures 
          [ normalText (0.15) (-120) (-10) white "To change car color press:"
          , normalText (0.15) (-35) (-40) blue "B - blue"
          , normalText (0.15) (-42) (-60) green "G - green"
          , normalText (0.15) (-37) (-80) yellow "Y - yellow"
          , normalText (0.10) (-40) (-100) white "(default is blue)"
          , normalText (0.2) (-130) (-140) white "Press SPACE to Start"
          ]

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

normalText :: Float -> Float -> Float -> Color -> String -> Picture
normalText e x y c str = 
    translate x y $ scale e e $ color c $ text str

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
    p <-  returnPlayer
    let state = initialState p g record
    play 
      window
      black 
      60 
      state 
      drawGame 
      handleKeys 
      updateGame
        where
            window = InWindow "Infinite Run Game" (round windowWidth, round windowHeigth) (10, 10)