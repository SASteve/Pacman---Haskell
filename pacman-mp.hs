import Data.Set                                            ( Set )
import qualified Data.Set as S                             ( delete
                                                           , empty
                                                           , insert
                                                           , member
                                                           , size
                                                           )
import Data.List                                          
import Control.Concurrent                                  ( threadDelay )
import System.IO                                           ( hFlush
                                                           , stdout
                                                           )
import System.Console.ANSI                                 ( clearScreen )
import System.IO.Streams                                   ( InputStream )
import qualified System.IO.Streams as SIOS                 ( fromList
                                                           , read
                                                           )
import Control.Monad                                       ( when )
import System.Exit                                         ( exitSuccess )
import Control.Monad.Loops                                 ( iterateM_ )
import System.Console.Haskeline                            ( runInputT, defaultSettings, getInputChar )
import Data.List.Split                                     ( chunksOf )
import Control.Monad                                       ( forever )
import Control.Concurrent                                  ( forkIO )
import Network                                             ( PortID(PortNumber)
                                                           , connectTo
                                                           , withSocketsDo
                                                           )
import System.Environment                                  ( getArgs )
import System.IO                                           ( BufferMode(LineBuffering)
                                                           , hClose
                                                           , hFlush
                                                           , hGetLine
                                                           , hPutStrLn
                                                           , hSetBuffering
                                                           )


data Block = Empty | Solid | Pacman | Ghost deriving (Show, Read, Eq)



type Entity = (Int, Int, Int)
type Board = [[Block]]

type GameState = (Board, [Entity])

map_size::Int
map_size = 7


first::Entity->Int
first (a,b,c) = a

second::Entity->Int
second (a,b,c) = b

third::Entity->Int
third (a,b,c) = c
{-/_\-} --Random Itachi appears

getBoardBlock::Board->Entity->Block
getBoardBlock gameBoard selector = gameBoard !! (first selector) !! (second selector)

initialBoard::Board
initialBoard = [ [Solid , Solid  , Solid , Solid , Solid , Solid , Solid]
               , [Solid , Pacman , Empty , Empty , Empty , Empty , Solid]
               , [Solid , Empty  , Empty , Empty , Empty , Empty , Solid]
               , [Solid , Empty  , Empty , Ghost , Empty , Empty , Solid]
               , [Solid , Empty  , Empty , Empty , Empty , Empty , Solid]
               , [Solid , Empty  , Empty , Empty , Empty , Empty , Solid]
               , [Solid , Solid  , Solid , Solid , Solid , Solid , Solid]
               ]


initialPacmanPosition::Entity
initialPacmanPosition = (1,1,0)
initialGhostPosition::Entity
initialGhostPosition = (3,3,1)

modifyRightEntity::Entity->Entity
modifyRightEntity ent = (first ent, second ent + 1, third ent)

modifyLeftEntity::Entity->Entity
modifyLeftEntity ent = (first ent, second ent - 1, third ent)

modifyDownEntity::Entity->Entity
modifyDownEntity ent = (first ent + 1, second ent, third ent)

modifyUpEntity::Entity->Entity
modifyUpEntity ent = (first ent - 1, second ent, third ent)

getEntityBlock::Entity->Block
getEntityBlock (a,b,0) = Pacman
getEntityBlock (a,b,1) = Ghost


moveRightEntity::Board-> Entity->Board
moveRightEntity inputList ent =
	let aux1 = zip [0..] inputList
	    aux2 = map (\(i,l) -> (i,zip [0..] l)) aux1
	    aux3 = map (\(i,l) -> map (\(j,x) -> ((i,j),x)) l) aux2
	    aux4 = map (\((a,b),z) -> if (a,b) == (first ent, second ent) then ((a,b), Empty)  else  if (a,b) == (first ent, second ent + 1) then ((a,b), getEntityBlock ent) else ((a,b),z)) $ concat aux3
	in  chunksOf map_size $ map snd aux4

moveLeftEntity::Board-> Entity->Board
moveLeftEntity inputList ent =
  let aux1 = zip [0..] inputList
      aux2 = map (\(i,l) -> (i,zip [0..] l)) aux1
      aux3 = map (\(i,l) -> map (\(j,x) -> ((i,j),x)) l) aux2
      aux4 = map (\((a,b),z) -> if (a,b) == (first ent, second ent) then ((a,b), Empty)  else  if (a,b) == (first ent, second ent - 1) then ((a,b),  getEntityBlock ent) else ((a,b),z)) $ concat aux3
  in  chunksOf map_size $ map snd aux4


moveDownEntity::Board-> Entity->Board
moveDownEntity inputList ent =
  let aux1 = zip [0..] inputList
      aux2 = map (\(i,l) -> (i,zip [0..] l)) aux1
      aux3 = map (\(i,l) -> map (\(j,x) -> ((i,j),x)) l) aux2
      aux4 = map (\((a,b),z) -> if (a,b) == (first ent, second ent) then ((a,b), Empty)  else  if (a,b) == (first ent + 1, second ent) then ((a,b),  getEntityBlock ent) else ((a,b),z)) $ concat aux3
  in  chunksOf map_size $ map snd aux4

moveUpEntity::Board-> Entity->Board
moveUpEntity inputList ent =
  let aux1 = zip [0..] inputList
      aux2 = map (\(i,l) -> (i,zip [0..] l)) aux1
      aux3 = map (\(i,l) -> map (\(j,x) -> ((i,j),x)) l) aux2
      aux4 = map (\((a,b),z) -> if (a,b) == (first ent, second ent) then ((a,b), Empty)  else  if (a,b) == (first ent - 1, second ent) then ((a,b),  getEntityBlock ent) else ((a,b),z)) $ concat aux3
  in  chunksOf map_size $ map snd aux4




entityStringPusher::Int->String
entityStringPusher x = case x of 0 -> "P"--"▲"
                                 1 -> "G"--"▼"
                               
{-
0->Empty
1->Pacman Up
2->Pacman Down
3->Pacman Left
4->Pacman Right
5->Globe
6->Ghost
-}



printBlock::Block->Entity->String
printBlock Solid  _ = " [X] "
printBlock Empty  _ = " [ ] "
printBlock Pacman _ = " [P] "
printBlock Ghost  _ = " [G] "
printBlock _ ent    = " [" ++ (entityStringPusher $ third ent) ++ "] "

printRow::[Block]->Entity->String
printRow boardRow ent = ( concat $ map (\x -> printBlock x ent) boardRow ) ++ "\n"

printBoard::Board->Entity->IO()
printBoard gameBoard ent = putStrLn (concat $ map (\x -> printRow x ent) gameBoard)

pureStepperFunction :: Board -> Entity -> Maybe Char -> Board
pureStepperFunction board ent (Just 'd') = if(board!!(first ent)!!(second ent+1)/=Solid) then moveRightEntity board ent else board
pureStepperFunction board ent (Just 's') = if(board!!(first ent+1)!!(second ent)/=Solid) then  moveDownEntity board ent else board
pureStepperFunction board ent (Just 'a') = if(board!!(first ent)!!(second ent - 1)/=Solid) then moveLeftEntity board ent else board
pureStepperFunction board ent (Just 'w') = if(board!!(first ent-1)!!(second ent)/=Solid) then moveUpEntity board ent else board
pureStepperFunction board _ _ = board

entityStepperFunction :: Board-> Entity -> Maybe Char -> Entity
entityStepperFunction board ent (Just 'd') = if(board!!(first ent)!!(second ent+1)/=Solid) then modifyRightEntity ent else ent
entityStepperFunction board ent (Just 's') = if(board!!(first ent+1)!!(second ent)/=Solid) then modifyDownEntity ent else ent
entityStepperFunction board ent (Just 'a') = if(board!!(first ent)!!(second ent - 1)/=Solid) then modifyLeftEntity ent else ent
entityStepperFunction board ent (Just 'w') = if(board!!(first ent-1)!!(second ent)/=Solid) then modifyUpEntity ent else ent
entityStepperFunction board ent _ = ent

generateNewEntityList::[Entity]->Entity->Int->[Entity]
generateNewEntityList entity_list new_entity position = 
  let aux1 = zip entity_list [0..]
      aux2 = map (\(ent,index) -> if index == position then (new_entity, index) else (ent, index)) aux1
  in  map (fst) aux2

firstRun::Int->GameState->IO GameState
firstRun frecv gameState = do
  clearScreen
  printBoard (fst gameState) ((snd gameState)!!frecv)
  maybeKeyboardInput <- runInputT defaultSettings $ getInputChar ""
  when ( maybeKeyboardInput == Just 'q' ) exitSuccess
  let incompleteGameState = pureStepperFunction (fst gameState) ((snd gameState)!!frecv) maybeKeyboardInput
  let new_entity          = entityStepperFunction (fst gameState) ((snd gameState)!!frecv)  maybeKeyboardInput
  return ((incompleteGameState, generateNewEntityList (snd gameState) new_entity frecv) :: GameState)


main :: IO ()
main = do
  clearScreen
  putStrLn ""
  putStrLn "     Who do you want to play with? : "
  putStrLn "0 - Pacman "
  putStrLn "1 - Ghost "
  hFlush stdout
  input <- getLine
  let howOften = read input :: Int
  iterateM_ (firstRun howOften) ((initialBoard, [initialPacmanPosition, initialGhostPosition])::GameState)
