--Eight off Solitaire
--Written by Andrew Cook

module EightOff where
 
 import Data.List
 import System.Random
 
 --Type definitions for the game's assets.
 data Suit = Hearts | Clubs | Spades | Diamonds
             deriving (Eq, Show)
 data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
            deriving (Eq, Show, Enum)
 type Card = (Pip, Suit)
 type Deck = [Card]
 type Columns = [[Card]]
 type Foundations = [Card]
 type Reserve = [Card]
 type EOBoard = (Foundations,Columns,Reserve)
 
 --pack, a pack of cards
 --plis, a list of pips
 --slis, a list of suits
 plis = [Ace .. King]
 slis = [Hearts,Clubs,Spades,Diamonds]
 pack = [(p,s)|p<-plis,s<-slis]
 
 --pCard, takes a card and returns its predecessor
 --x, the card who's predecessor we wish to find
 pCard :: Card -> Card
 pCard x = (pred(fst(x)), snd(x))
 
 --sCard, takes a card and returns its successor
 --x, the card who's successor we wish to find
 sCard :: Card -> Card
 sCard x = (succ(fst(x)), snd(x))
  
 --isAce, takes a card and returns TRUE if it is an Ace of any suit
 --x, the card we are testing
 isAce :: Card -> Bool
 isAce x
  | fst(x) == Ace = True
  | otherwise = False
 
 --isKing, takes a card and returns TRUE if it is a King of any suit
 --x, the card we are testing
 isKing :: Card -> Bool
 isKing x
  | fst(x) == King = True
  | otherwise = False
 
 --shuffle, takes a deck and returns a shuffled deck
 --clis is a list of cards, AKA the deck
 --rlis is a list of random numbers
 --seed is the seed for the random number generator
 shuffle :: Deck -> Int -> Deck
 shuffle [] _ = []
 shuffle clis seed = map fst slis
    where rlis = take (length clis) (randoms (mkStdGen seed) :: [Int])
          zlis = zip clis rlis
          slis = mergesort (\(_,n1) (_,n2)->n1<n2) zlis 

 
 --parseDeck, takes a deck and returns the same deck, organised in a way so that the columns and the reserve can be filtered out of it
 --The parsed deck is a list of lists, therefore will return a column type.
 --col, the columns
 --clis, the deck (or "list of cards")
 -- hlis, tlis, the head and tail of the columns
 parseDeck :: Deck -> Columns -> Columns
 parseDeck [] col = col
 parseDeck (h:t) [] = parseDeck t [[h]]
 parseDeck clis@(h:t) (hlis:tlis)
    | (length hlis)==6 = (hlis: parseDeck clis tlis)
    |otherwise = parseDeck t ((insertCard h hlis):tlis)
 
 -- insertCard, takes a card, a deck, and returns the card inserted onto the head of the deck
 -- Auxiliary function to makeColumns
 -- c, the card to be inserted
 -- clis, the deck, or list of cards
 insertCard :: Card -> Deck -> Deck
 insertCard c clis = (c:clis)
 
 --parseColumns, extracts the columns from the parsed Deck
 --col, the columns we want, filtered from the parsed deck
 --decks of length six are the starting columns
 parseColumns :: Columns -> Columns
 parseColumns col = (filter (\n -> (length n)==6) col)

 -- parseReserve, extracts the reserve list from the parsed deck
 -- col, the columns we want, in this case just one so we only want the head, filtered from the parsed deck
 -- the single deck of length 4 is returned.
 parseReserve :: Columns -> Deck
 parseReserve col = head (filter (\n -> (length n)==4) col)
 
 --eODeal, takes a seed for randomisation as an Int, and returns a shuffled EOboard
 eODeal :: Int -> EOBoard
 eODeal seed = ([], parseColumns (parseDeck (shuffle pack seed) []), parseReserve (parseDeck (shuffle pack seed) []))
 
 --toFoundations, takes an EOBoard and puts all immediately available Aces in the foundations, to give the autoPlay function something to work with.
 --This is so we can immediately return the board as it is if no Aces are at the heads of the columns.
 --(f,c,r), the board broken down into its elements
 toFoundations :: EOBoard -> EOBoard
 toFoundations (f,c,r) 
   | acesToFoundations == [] = (f,c,r)
   | otherwise = autoPlay(f++acesToFoundations,(filterOutCards acesToFoundations c),(filter(\n -> (notElem n acesToFoundations)) r))
    where acesToFoundations = [ace|ace<-(getHeadsOfColumns c)++r,isAce(ace)]
 
 --autoPlay takes an EOboard and essentially plays the game as far as it can. If there are no aces immediately available it looks for any successors of foundation cards in the columns.
 --If there are no Aces or successors at the heads of the columns or in the reserves, then just return the board as is.
 --acesToFoundations, a list comprehension that compiles 
 autoPlay :: EOBoard -> EOBoard
 autoPlay (f,[],[]) = (f,[],[])
 autoPlay ([],c,r) = ([],c,r)
 autoPlay (f,[],r) = (f,[],r)
 autoPlay (f,c,[]) = (f,c,[])
 autoPlay (f,c,r)
   | acesToFoundations == [] = if successorsToFoundations == [] then (f,c,r) else autoPlay (successorsToFoundations,filterOutCards successorsToFoundations c,(filter(\n -> (notElem n successorsToFoundations)) r))
   | otherwise = autoPlay(f++acesToFoundations,(filterOutCards acesToFoundations c),(filter(\n -> (notElem n acesToFoundations)) r))
    where acesToFoundations = [ace|ace<-(getHeadsOfColumns c)++r,isAce(ace)]
          successorsToFoundations = [cards|cards<-(getHeadsOfColumns c)++r, elem cards (map (\n -> sCard(n)) f)]
 
 --filterOutCards takes a deck and columns and takes ever card in the deck out of the columns.
 --This function is used to give the illusion of cards "moving" from the columns/reserves to the foundations in autoPlay
 -- col, the columns
 -- clis, the deck or list of cards
 filterOutCards :: Deck -> Columns -> Columns
 filterOutCards [] [] = []
 filterOutCards [] col = col
 filterOutCards clis [] = []
 filterOutCards clis ([]:tlis) = filterOutCards clis tlis
 filterOutCards clis ((h:t):tlis)
   | elem h clis = (t:filterOutCards clis tlis)
   | otherwise = ((h:t):filterOutCards clis tlis)

 getHeadsOfColumns :: Columns -> Deck
 getHeadsOfColumns [] = []
 getHeadsOfColumns ([]:tlist) = getHeadsOfColumns tlist
 getHeadsOfColumns (hlist:tlist) = ((head hlist):getHeadsOfColumns tlist)

--mergesort
--We make each item in a list into a sublist of length 1
--We merge successive pairs of lists
--until we end up with a list of one list containing the sorted data
 mergesort :: (a->a->Bool)->[a]->[a]
 mergesort compfn [] = []
 mergesort compfn dlis = (mergesortA compfn (map (\e -> [e]) dlis))

 mergesortA _ [lis] = lis
 mergesortA compfn mlis = mergesortA compfn (mergesortpass compfn mlis)

 mergesortpass :: (a->a->Bool)->[[a]] -> [[a]]
 mergesortpass _ [] = []
 mergesortpass _ [l] = [l]
 mergesortpass compfn (lis1:(lis2:rest)) = (my_merge compfn lis1 lis2): mergesortpass compfn rest

-- --merge
-- --merging we have two sorted lists, we want to make one sorted list
-- --compfn is a comparison function in the form (a->a->Bool)
-- --[a] are the two sorted list
-- --returns [a] which is the final list containing every "a" from the original two lists in sorted order
 my_merge :: (a->a->Bool)->[a]->[a]->[a]
 my_merge compfn [] lis = lis
 my_merge compfn lis [] = lis
 my_merge compfn (h1:t1) (h2:t2)
    | compfn h1 h2 = (h1:my_merge compfn t1 (h2:t2))
    | otherwise = (h2:my_merge compfn (h1:t1) t2)
