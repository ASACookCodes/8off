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
 pack = [(p,s)|p<-[(Ace)..(King)],s<-[Hearts,Clubs,Spades,Diamonds]]
 
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
 -- col, the columns we want, in this case we return a deck so we only want the head, filtered from the parsed deck
 -- the single deck of length 4 is returned.
 parseReserve :: Columns -> Deck
 parseReserve col = head (filter (\n -> (length n)==4) col)
 
 --eODeal, takes a seed for randomisation as an Int, and returns a shuffled EOboard
 eODeal :: Int -> EOBoard
 eODeal seed = ([], parseColumns (parseDeck (shuffle pack seed) []), parseReserve (parseDeck (shuffle pack seed) []))

 --toFoundations takes an EOboard and essentially plays the game as far as it can. If there are no aces immediately available it looks for any successors of foundation cards in the columns.
 --If there are no Aces or successors at the heads of the columns or in the reserves, then just return the board as is.
 --Otherwise, put the aces/successor cards into the foundations as appropriate, and filter them out of the columns/reserves
 --Any newly uncovered Aces are appended to the foundations, and any successor cards replace their predecessor cards in the foundation.
 --acesToFoundations, a list comprehension that compiles any aces at the heads of the columns
 --successorsToFoundations a list comprehension that compiles any cards whose predecessor cards are currently present in the foundations
 toFoundations :: EOBoard -> EOBoard
 toFoundations (f,[],[]) = (f,[],[]) -- The game is complete
 toFoundations (f,c,r)
   | acesToFoundations == [] = if successorsToFoundations == [] then (f,c,r) else toFoundations (successorsToFoundations,filterOutCards successorsToFoundations c,(filter(\n -> (notElem n successorsToFoundations)) r))
   | otherwise = toFoundations (f++acesToFoundations,(filterOutCards acesToFoundations c),(filter(\n -> (notElem n acesToFoundations)) r))
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
 filterOutCards clis ([]:tlis) = ([]:filterOutCards clis tlis)
 filterOutCards clis ((h:t):tlis)
   | elem h clis = (t:filterOutCards clis tlis)
   | otherwise = ((h:t):filterOutCards clis tlis)

 --getHeadsOfColumns, takes the columns and returns a deck containing the heads of each columns
 --(hlist:tlist)the columns split into the head column and the rest of the columns.
 getHeadsOfColumns :: Columns -> Deck
 getHeadsOfColumns [] = []
 getHeadsOfColumns ([]:tlist) = getHeadsOfColumns tlist
 getHeadsOfColumns (hlist:tlist) = ((head hlist):getHeadsOfColumns tlist)
 

 --findMoves, takes a board and returns a list of possible boards after making all possible moves
 findMoves :: EOBoard -> [EOBoard]
 findMoves (f,[],[]) = []
 findMoves board = ((columnsToReserveMoves board)++(reserveToColumnsMoves board)++(columnsToFoundationMoves board)++(reserveToFoundationMoves board)++(columnsToColumnsMoves board))
 
 --columnsToReserveMove, takes an EOBoard and returns all the boards where the top of a column has moved to the reserve
 columnsToReserveMoves :: EOBoard -> [EOBoard]
 columnsToReserveMoves (_,[],_)= []
 columnsToReserveMoves (f,c,r)
   | length(r) == 8 = []
   | otherwise = [(f,(filterOutCards [cmoved] c),r++[cmoved])|cmoved<-(getHeadsOfColumns notEmptyCols)]
    where notEmptyCols = (filter(\n -> n/=[]) c)
 
 --columnsToReserveMove, takes an EOBoard and returns all the boards where the top of a column has moved to the reserve
 reserveToColumnsMoves :: EOBoard -> [EOBoard]
 reserveToColumnsMoves (_,_,[]) = []
 reserveToColumnsMoves (f,c,r) = [(f,(filter(\n -> n/=(tail cmoved)) c)++[cmoved],(filter(\n -> n/=(head cmoved)) r))|cmoved<-appendedCols++startedCols]
    where emptyCols = (filter(\n -> n==[]) c)
          notEmptyCols = (filter(\n -> n/=[]) c)
          freecols = (filter(\n -> fst((head n))/=Ace) notEmptyCols) -- Free columns are only those that do not have an Ace card on top
          appendedCols = [(hcard:(hcol:tcol))|hcard<-r,(hcol:tcol)<-freecols,hcard==pCard(hcol)]
          startedCols = if (length emptyCols > 0) then [[startCard]|startCard<-(filter (\n -> isKing(n)==True) r)] else []
 
 --colunmsToFoundationMoves - takes an EOBoard and returns all the boards where the top of a column has moved to the foundations
 columnsToFoundationMoves :: EOBoard -> [EOBoard]
 columnsToFoundationMoves (_,[],_)=[]
 columnsToFoundationMoves (f,c,r) = [(putFoundation cmoved f,filterOutCards [cmoved] c,r)|cmoved<-(filter(\n -> isAce(n) || (elem (pCard n) f)) (getHeadsOfColumns c))]
 
 --putFoundation, takes a card and puts it in the foundations
 putFoundation :: Card -> Foundations -> Foundations
 putFoundation card f
   | isAce(card) = f++[card]
   | otherwise = [card]++filter(\n -> n/=pCard(card)) f

 --reserveToFoundationMoves, takes an EOBoard and returns all the boards where a reserve has moved to the foundations
 reserveToFoundationMoves :: EOBoard -> [EOBoard]
 reserveToFoundationMoves (_,_,[]) = []
 reserveToFoundationMoves (f,c,r) = [(putFoundation rmoved f,c,filter(\n -> n/=rmoved) r)|rmoved<-filter(\n -> isAce(n) || (elem (pCard n) f)) r]
 
 --columnsToColumnsMoves, takes an EOBoard and returns all the boards where the tops of columns have moved to other tops of columns
 columnsToColumnsMoves :: EOBoard -> [EOBoard]
 columnsToColumnsMoves (_,[],_)=[]
 columnsToColumnsMoves (f,c,r) = [(f,(filterOutCards [head cmoved] (filter(\n -> n/=(tail cmoved))c))++[cmoved],r)|cmoved<-appendedCols++startedCols]
    where emptyCols = (filter(\n -> n==[]) c)
          notEmptyCols = (filter(\n -> n/=[]) c)
          freecols = (filter(\n -> fst((head n))/=Ace) notEmptyCols)
          appendedCols = [(hcard:(hcol:tcol))|hcard<-(getHeadsOfColumns notEmptyCols),(hcol:tcol)<-freecols,hcard==pCard(hcol)]
          startedCols = if (length emptyCols > 0) then [[startCard]|startCard<-(filter (\n -> isKing(n)==True) (getHeadsOfColumns freecols))] else []

 --chooseMove, takes an EOBoard and returns the best EOBoard from the list of possible moves
 --This is done by zipping all the possible EOBoards with a weight, based on how good that move is
 chooseMove :: EOBoard -> Maybe (Foundations,Columns,Reserve)
 chooseMove (f,[[],[],[],[],[],[],[],[]],[]) = Just (f,[[],[],[],[],[],[],[],[]],[])
 chooseMove (f,c,r)
   | (findMoves (f,c,r)) == [] = Nothing
   | otherwise = Just (fst(maximumBy (\(_,x) (_,y) -> compare x y) (weighMoves (findMoves (f,c,r)) (f,c,r) []))) --Best move will be sorted to be at the head of the returned EOBList
 
 --weighMoves, takes a list of EOBoards, the current EOBoard, and zips the list of boards with an integer based on comparing the current board with the possible boards
 weighMoves :: [EOBoard] -> EOBoard -> [((Foundations,Columns,Reserve),Int)] -> [((Foundations,Columns,Reserve),Int)]
 weighMoves [] (f,c,r) weighedList = weighedList --We've weight everything
 weighMoves ((movedf,movedc,movedr):tBoard) (currentf,currentc,currentr) weighedList
   | (length (filter(\n -> fst(n)==Ace) movedf)) > (length (filter(\n -> fst(n)==Ace) currentf)) = weighMoves tBoard (currentf,currentc,currentr) (zip [(movedf,movedc,movedr)] [6])++weighedList --Ace has been moved to foundations, zip it with 4, keep it in moveLis, call weighMoves on tail
   | (length (filter(\n -> fst(n)==King) movedr)) < (length (filter(\n -> fst(n)==King) currentr)) = weighMoves tBoard (currentf,currentc,currentr) (zip [(movedf,movedc,movedr)] [5])++weighedList -- A new column has been created
   | movedf/=currentf = weighMoves tBoard (currentf,currentc,currentr) (zip [(movedf,movedc,movedr)] [4])++weighedList --A card has moved to the foundations
   | ((length movedr) < 4) && (movedr/=currentr) = weighMoves tBoard (currentf,currentc,currentr) (zip [(movedf,movedc,movedr)] [3])++weighedList --Reserves have not more than three cards
   | (movedc/=currentc) && (movedr==currentr) && (movedf==currentf) = weighMoves tBoard (currentf,currentc,currentr) (zip [(movedf,movedc,movedr)] [2])++weighedList --A card has moved from column to column
   | otherwise = if (reserveToColumnsMoves (movedf,movedc,movedr))/=[] then weighMoves tBoard (currentf,currentc,currentr) (zip [(movedf,movedc,movedr)] [1])++weighedList else weighMoves tBoard (currentf,currentc,currentr) (zip [(movedf,movedc,movedr)] [0])++weighedList --Look ahead to determine whether moving a column to a reserve will allow us to make a reserveToColumn move afterwards
 
 
 --eOExpt, plays 100 games of eight off, returns the mean score, and the number of wins
 eOExpt :: Int -> (Int,Int)
 eOExpt seed = (meanScore rlis [],numberOfWins rlis 0)
    where rlis = take 100 (randoms (mkStdGen seed) :: [Int])
 
 --meanScore, takes a list of seeds, plays games for all the seeds, and returns the mean score from those games
 meanScore :: [Int] -> [Int] -> Int
 meanScore [] scoreLis = div (foldr (+) 0 scoreLis) (length scoreLis)
 meanScore (hseed:tlis) scoreLis = meanScore tlis ((eOGame hseed):scoreLis)
 
 --numberOfWins, takes a list of seeds, plays games for all the seeds, and returns the number of games that result in a win
 numberOfWins :: [Int] -> Int -> Int
 numberOfWins [] winList = winList
 numberOfWins (hseed:tlist) winList
   | eOGame hseed == 52 = numberOfWins tlist (winList + 1)
   | otherwise = numberOfWins tlist winList
 
 --eOGame, takes a seed, and returns a score
 eOGame :: Int -> Int
 eOGame seed = playGameToCompletion (eODeal seed) []
 
 --playGameToCompletion, takes a starting board, and a list of moves made so far, and returns a score
 playGameToCompletion :: EOBoard -> [EOBoard] -> Int
 playGameToCompletion board moveHistory
   | chooseMove board == Nothing = calculateScore board
   | otherwise = if elem (resMaybe (chooseMove board)) moveHistory then calculateScore board else playGameToCompletion (resMaybe (chooseMove board)) ((resMaybe (chooseMove board)):moveHistory) -- Do not allow the game to continue if we have made a move that we have already made before
 
 --calculateScore, takes an EOBoard, complete or uncomplete, and returns its score
 calculateScore :: EOBoard -> Int
 calculateScore (f,c,r) = foldr (+) 0 [(countBackCards n 0)|n<-f]
 
 --countBackCards, takes a card in the foundation, and finds the number of cards underneath it
 countBackCards :: Card -> Int -> Int
 countBackCards (Ace,_) foundationHeight = (foundationHeight + 1)
 countBackCards topCard foundationHeight = countBackCards (pCard(topCard)) (foundationHeight + 1)

 --Maybe helper functions
 isJust :: (Maybe a) -> Bool
 isJust (Just _) = True
 isJust Nothing = False

 resMaybe :: (Maybe a) -> a
 resMaybe (Just x) = x

 --Display an EOBoard in a neat format
 displayEOB :: EOBoard -> IO String
 displayEOB (fnds,cols,res) = do
  let colStr = colsToString cols
  putStr "EOBoard\nFoundations  "
  putStrLn (show fnds)
  putStr  "Columns"
  putStr colStr
  putStr "\n\nReserve     "
  putStrLn (show res)
  putStr "\n---------------------------------------------\n"
  return ""

 colsToString :: Columns->String -- prepare String to print columns on separate lines
 colsToString cols =
  foldr (++) "" ["\n             "++(show col) |col<-cols]


 displayEOBList :: [EOBoard]-> IO String
 displayEOBList eobl =  -- @ notation doesn't seem to work correctly
  do
   if (null eobl) then do (return "")
                  else do
                        displayEOB (head eobl)
                        displayEOBList (tail eobl)

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