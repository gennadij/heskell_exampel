-- Datatypen
type Description = String
type Inventory = [GameObject]
type GameMap = [Room]
type GameState = (GameMap, Inventory)
data GameObject = Player | Acorn deriving (Eq, Show)
data Room = Room Description [GameObject] deriving (Show)

