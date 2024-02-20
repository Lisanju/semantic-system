module Relations where

stems,suffixes,adjectives :: [String]

stems    = ["use","faith"]
suffixes = ["ful","less"]

adjectives = [ x ++ y | x <- stems, y <- suffixes]

data Entity = A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z | Unspec
            deriving (Eq,Show,Bounded,Enum)

entities = [minBound..maxBound]

girl,boy,princess,dwarf,giant,wizard,child,brave,laugh,cheer,shudder :: [Entity]

girl     = [A,D,G,S]
boy      = [M,Y]
princess = [E]
dwarf    = [B,R]
giant    = [T]
wizard   = [W,V]

child    = girl ++ boy

brave    = [D,Y]
laugh    = [A,G,E]
cheer    = [M,D]
shudder  = [S]


is,does :: (Eq a) => a -> [a] -> Bool
is   = elem
does = elem


help,admire,defeat :: [(Entity,Entity)]

help     = [(W,W),(V,V),(S,B),(D,M)]
admire   = [(x,G) | x <- entities, x `is` child]
defeat   = [(x,y) | x <- entities,
                    y <- entities,
                    x `is` dwarf && y `is` giant]

laugh' :: Entity -> Bool
laugh' x = x `elem` [A,G,E]

help' :: (Entity,Entity) -> Bool
help' (x,y) = (x,y) `elem` [(W,W),(V,V),(S,B),(D,M)]

help'' :: Entity -> Entity -> Bool
help'' x y = (x,y) `elem` [(W,W),(V,V),(S,B),(D,M)]

help''' :: Entity -> Entity -> Bool
help''' y x = (x,y) `elem` [(W,W),(V,V),(S,B),(D,M)]


eat :: Entity -> Entity -> Bool
eat = curry (`elem` [(A,I),(D,Unspec)]) 

passivize :: (Entity -> Entity -> Bool) -> (Entity -> Bool)
passivize r = \ x -> r Unspec x

close :: [(Entity,Entity)] -> [(Entity,Entity)]
close r = r ++ [ (Unspec,y) | x <- entities, y <- entities, (x,y) `elem` r ]
            ++ [ (x,Unspec) | x <- entities, y <- entities, (x,y) `elem` r ]                             

eat' :: Entity -> Entity -> Bool
eat' = curry (`elem` (close [(A,I),(D,Unspec)]))

self :: (a -> a -> b) -> (a -> b)
self r = \ x -> r x x
