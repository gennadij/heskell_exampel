import qualified Data.List as L
-- Datatypen
type Description = String
type Inventory = [GameObject]
type GameMap = [Room]
type GameState = (GameMap, Inventory)
-- Data
data GameObject = Player | Acorn deriving (Eq, Show)
data Room = Room Description [GameObject] deriving (Show)

initialState :: GameState
initialState = (
    [ 
        Room "You are inside a tree." [Player]
      , Room "You are outside a tree." [Acorn]
    ],
    [] 
  )

main :: IO ()
main = do
  putStrLn "Welcome to Skwak the Squirrel."
  putStrLn "You are a squirrel"
  gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop (rooms, currentInv)= do
  let currentRoom = 
        case findRoomWithPlayer rooms of
          Just r -> r
          Nothing -> error $ "Somehow the player "
                       ++ "ended up outside the map!"
      possibleCmds =
        validCommands currentRoom currentInv
  if playerWon (rooms, currentInv)
  then gameOverRestart
  else do
    discribeWorld currentRoom currentInv possibleCmds
    takeActionThenLoop
      currentRoom currentInv possibleCmds rooms

findRoomWithPlayer :: [Room] -> Maybe Room
findRoomWithPlayer = 
  L.find (\(Room _ obs) -> any (== Player) obs)

validCommands :: Room -> Inventory -> [String]
validCommands (Room _ gameObjs) invItems = 
  ["go"] ++ takeComandList ++ 
  dropComandList ++ ["quit"]
  where 
    takeComandList =
      if somethingsToTake gameObjs
      then ["take"]
      else []
    dropComandList = 
      if length invItems > 0
      then ["put"] 
      else []

somethingsToTake :: [GameObject] -> Bool 
somethingsToTake = any (/= Player) 

playerWon :: GameState -> Bool 
playerWon (rooms, currentInv) = any hasAcornAndInside rooms
  where hasAcornAndInside (Room desc objs) = 
          desc == "You are inside a tree"
          && any (== Acorn) objs

gameOverRestart :: IO ()
gameOverRestart = do 
  putStrLn $ "You won!"
    ++ "You have syccessful stored the acorn"
    ++ "for winter. Well done"
  putStrLn "Do you want to play again? y = yes"
  playAgain <- getLine
  if playAgain == "y"
  then gameLoop initialState
  else putStrLn "Thanks for palaying"

getCommand :: IO String

getCommand = do
  putStrLn "What do you want todo?"
  getLine

takeActionThenLoop :: Room -> 
                      Inventory ->
                      [String] -> 
                      [Room] -> 
                      IO ()
takeActionThenLoop currentRoom
                   currentInv
                   possibleCmds
                   rooms = 
  do
    command <- getCommand
    if any (==command) possibleCmds
    then case command of
      "go" -> 
        do
          putStrLn "You go ..."
          gameLoop $ movePlayer (rooms, currentInv)
      "take" ->
        do
          putStrLn "You take the acorn..."
          gameLoop $ moveAcornToInventory (rooms, currentInv)
      "put" -> 
        do   
          putStrLn "You put the acorn down..."
          gameLoop $ moveAcornToInventory (rooms, currentInv)
      "quit" -> 
        putStrLn $ "You decide to give up."
                 ++  "Tnanks for playing"
      _ -> 
        do 
          putStrLn "That is not a command"
          gameLoop (rooms, currentInv)
    else do
      putStrLn $ "Command not possible here,"
               ++ " or that is not a command."
      gameLoop (rooms, currentInv)

movePlayer :: GameState -> GameState
movePlayer (rooms, inv) = 
  (newRooms, inv) 
  where 
    newRooms = map adjustRooms rooms
    adjustRooms (Room d objs) = 
      if any (==Player) objs
      then (Room d (filter (/=Player) objs))
      else (Room d (Player : objs))

moveAcornToInventory :: GameState -> GameState
moveAcornToInventory (rooms, inv) = 
  (newRooms, newInv)
  where 
    newInv = filter (/=Acorn) inv
    newRooms = map adjustRooms rooms 
    adjustRooms (Room d objs) =
      if any (==Player) objs
      then Room d (Acorn : objs)
      else Room d objs
discribeWorld :: Room -> 
                 Inventory -> 
                 [String] -> 
                 IO ()
discribeWorld currentRoom
              currentInv  
              possibleCmds =
  do 
    putStrLn $ discribeRoom currentRoom
    putStrLn $ discribeInventory currentInv
    putStrLn $ discribeCommands possibleCmds

discribeRoom :: Room -> String 
discribeRoom (Room desc objs) = 
  desc ++ if any (==Acorn) objs
          then "There is acorn here"
          else ""

discribeInventory :: Inventory -> String 
discribeInventory [] = "You are holding nothing"
discribeInventory inv = "You are holding: " ++ (concat $ map show inv)

discribeCommands :: [String] -> String 
discribeCommands commands = "Commands: "
                            ++ (L.intercalate ", " commands)
