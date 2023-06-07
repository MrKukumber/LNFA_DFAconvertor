import System.IO
import Data.List
import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Data.Char
import Data.List.Split (chunksOf)

newtype StateID a = StateID a
    deriving(Eq, Ord)
type InSymbID = Char
type Alphabet = [InSymbID]

type State_DFA a = a
type States_DFA a = [State_DFA a]
data Trans_DFA a =
    Trans_DFA {
        inputSymb_DFA :: InSymbID,
        outGoingS_DFA :: State_DFA a,
        inGoingS_DFA :: State_DFA a
    }
    deriving(Eq, Show, Ord)

type State_LNFA a = a
type States_LNFA a = [State_LNFA a]
data Trans_LNFA a =
    Lambda_LNFA {
        outGoingS_LNFA :: State_LNFA a,
        inGoingS_LNFA :: State_LNFA a
    }|
    Trans_LNFA {
        inputSymb_LNFA :: InSymbID,
        outGoingS_LNFA :: State_LNFA a,
        inGoingS_LNFA :: State_LNFA a
    }
    deriving(Eq, Show, Ord)

data Automaton_DFA a = Automaton_DFA {
    states_DFA :: States_DFA a,
    alphabet_DFA :: Alphabet,
    transFunc_DFA :: [Trans_DFA a],
    initialS_DFA :: State_DFA a,
    acceptS_DFA :: States_DFA a
}

data Automaton_LNFA a = Automaton_LNFA {
    states_LNFA :: States_LNFA a,
    alphabet_LNFA :: Alphabet,
    transFunc_LNFA :: [Trans_LNFA a],
    initialS_LNFA :: States_LNFA a,
    acceptS_LNFA :: States_LNFA a
}
-----------------------------------------------------------------------------------------------

instance (Show a) => Show (StateID a) where
    show (StateID a) = "S" ++ show a

instance (Eq a, Show a) => Show (Automaton_DFA a) where
    show automaton@(Automaton_DFA states alphabet transFunc initialS acceptS) = 
        let strStates = getDFAStatesStrRep states
            strAlphabet = "Alphabet: {\n" ++ intersperse ',' alphabet ++ "\n}\n"
            strTransFunc = getDFATransFuncStrRep automaton
        in strStates ++ strAlphabet ++ strTransFunc

-- returns string reprezentation of DFA states
getDFAStatesStrRep :: (Show a) => [State_DFA a] -> String
getDFAStatesStrRep states = let
    strAliases = [show (i*5) ++ " - " ++ show (i*5 + 4) ++ ": "|i <- [0..length states `div` 5]]
    getStrStates prevStates (alias, state:states) = foldl (\acc s -> acc ++ ", " ++ show s) (prevStates ++ alias ++ show state) states ++"\n"
    in "States: {\n" ++
                foldl getStrStates "" (zip strAliases (chunksOf 5 states)) ++
                "}\n"
-- returns string representattion of transition function of DFA in shape of table
getDFATransFuncStrRep :: (Eq a) => Automaton_DFA a -> String
getDFATransFuncStrRep automaton@(Automaton_DFA states alphabet transFunc initialS acceptS) = let
    maxAliasLength = length $ show (length states - 1)
    in "Transition function:\n" ++ 
        [' '| _ <- [1..maxAliasLength+2]] ++ "|" ++ 
        foldl (\acc symb -> acc ++ 
            [' '| _ <- [1..(maxAliasLength-1) `div` 2 + 1]] ++ [symb] ++ 
            [' '| _ <- [1..(maxAliasLength-1) `div` 2 + (maxAliasLength-1) `mod` 2 + 1]] ++ "|") "" alphabet ++ 
        "\n    " ++
        ['-'| _ <- [1..maxAliasLength+1+(maxAliasLength+3)*length alphabet-2]] ++ "\n" ++
        foldl (getDFATransitionStrRep automaton) "" states

-- this funciton is used as 'foldl' argument, adds new line of table of transition function to accumulator
getDFATransitionStrRep :: (Eq a) => Automaton_DFA a -> String -> State_DFA a -> String
getDFATransitionStrRep (Automaton_DFA states alphabet transFunc initialS acceptS) acc state
    | state == initialS && state `elem` acceptS = acc ++ ">*" ++ transitionStr
    | state == initialS = acc ++ "> " ++ transitionStr
    | state `elem` acceptS = acc ++ " *" ++ transitionStr
    | otherwise = acc ++ "  " ++ transitionStr
    where
        maxAliasLength = length $ show $ length states - 1
        Just stateAlias = show <$> elemIndex state states
        transFromState = [ destState | symbol <- alphabet, let Just destState = getDestStateAlias symbol state states transFunc]
        transitionStr = stateAlias ++ 
             [' '| _ <- [1..maxAliasLength - length stateAlias]] ++ "|" ++
             foldl (\acc alias -> acc ++ 
                [' '| _ <- [1..(maxAliasLength-length alias) `div` 2 + 1]] ++ alias ++ 
                [' '| _ <- [1..(maxAliasLength-length alias) `div` 2 + (maxAliasLength-length alias) `mod` 2 + 1]] ++ "|") "" transFromState ++
             "\n"

-- returns alias of DFA state, alias is equal to index of state in state list
getDestStateAlias ::(Eq a) => InSymbID -> State_DFA a -> [State_DFA a] -> [Trans_DFA a] -> Maybe String
getDestStateAlias _ _ _ [] = Just ""
getDestStateAlias symbol state states (Trans_DFA symbT outGoingT inGoingT:restTransFunc)
    | symbT == symbol && state==outGoingT = show <$> elemIndex inGoingT states
    | otherwise = getDestStateAlias symbol state states restTransFunc

-----------------------------------------------------------------------------------------------

-- main function for executing
main :: IO()
main = do
    putStrLn "Hello, welkome to lambda-NFA to DFA convertor."
    putStrLn "This convertor is using subset constraction method for converting."
    automaton_LNFA <- getLNFA
    let automaton_DFA = convertLNFA_DFA automaton_LNFA
    _ <- showDFA automaton_DFA
    putStrLn "Thanks for using our convertor. Hope it served well."

-- gets and returns LNFA from user, it gives instructions to user and then reads input
getLNFA :: IO (Automaton_LNFA (StateID String))
getLNFA = do
    putStrLn "Please, insert your lambda-NFA according to the instructions:"
    putStrLn ""
    putStrLn "1. Insert set of automaton state names in one line,"
    putStrLn "delimetered by spaceces like: 454 A2 hello +-/* World ..."
    states <- getStates
    putStrLn ""
    putStrLn "2. Insert alphabet symbols similarly as in the previous case."
    putStrLn "Alphabet symbols are allowed to be only single characters."
    putStrLn "You can but dont have to place spaces between characters."
    putStrLn "Input can look like: 1 d - 5 s p * ..."
    alphabet <- getAlphabet
    putStrLn ""
    putStrLn "3. Insert transition function of automaton. Each transition"
    putStrLn "write on one line. Shape of input should be three/two words"
    putStrLn "delimetered by spaces as follows:"
    putStrLn "outGoingState inGoingState alphabetSymbol"
    putStrLn "For lambda transitions as: outGoingState inGoingState"
    putStrLn "After last transition write: end, to end input"
    putStrLn "Each line will be validated outright. Each not valid transition"
    putStrLn "won't be used in final automaton. Warning will be shown."
    transitionFunction <- getTransFunc states alphabet
    putStrLn ""
    putStrLn "4. Insert set of initial states of automaton similarly to"
    putStrLn "first case. This set must be subset of all states declared before."
    initialS <- getInitialS states
    putStrLn ""
    putStrLn "5. In the way of previous point insert set of accept states."
    acceptS <- getAcceptS states
    putStrLn ""
    putStrLn ""
    return $ Automaton_LNFA states alphabet transitionFunction initialS acceptS
    where
        getStates = do
            inStates <- getLine
            case apply parseStates inStates of
                Nothing -> do
                    putStrLn "Not valid input. Try again."
                    getStates
                Just (states, _) -> return states
        getAlphabet = do
            inAlphabet <- getLine
            case apply parseAlphabet inAlphabet of
                Nothing -> do
                    putStrLn "Not valid input. Try again."
                    getAlphabet
                Just (alphabet, _) -> return alphabet

-- gets transition function from user and at the same time it checks input and warns user before invalid transitions
getTransFunc :: States_LNFA (StateID String)-> [InSymbID] -> IO [Trans_LNFA (StateID String)]
getTransFunc states alphabet = do
    line <- getLine
    case apply parseTransFuncLine line of
        Nothing -> case apply (token parseWord) line of
            Nothing -> do
                putStrLn "Not valid input. Try again."
                getTransFunc states alphabet
            Just("end", _) -> return []
            Just(_,_) -> do
                putStrLn "Not valid input. Try again."
                getTransFunc states alphabet
        Just (transition@(Trans_LNFA symb outGoingState inGoingState), _) -> testLNFATransition transition states alphabet
        Just (transition@(Lambda_LNFA outGoingState inGoingState), _) -> testLNFALambdaTransition transition states alphabet

-- testing of transition, if it includes states from set of states and symbols of alphabet
testLNFALambdaTransition :: Trans_LNFA (StateID String) -> States_LNFA (StateID String) -> Alphabet -> IO [Trans_LNFA (StateID String)]
testLNFALambdaTransition transition@(Lambda_LNFA outGoingState inGoingState) states alphabet
    | outGoingState `notElem` states = do
        putStrLn "Outgoing state is not included in given states. Try to insert transition again."
        getTransFunc states alphabet
    | inGoingState `notElem` states = do
        putStrLn "Ingoing state is not included in given states. Try to insert transition again."
        getTransFunc states alphabet
    | otherwise = (transition:) <$> getTransFunc states alphabet
    
testLNFATransition :: Trans_LNFA (StateID String) -> States_LNFA (StateID String) -> Alphabet -> IO [Trans_LNFA (StateID String)]
testLNFATransition transition@(Trans_LNFA symb outGoingState inGoingState) states alphabet
    | symb `notElem` alphabet = do
        putStrLn "Given symbol is not included in alphabet. Try to insert transition again."
        getTransFunc states alphabet
    | outGoingState `notElem` states = do
        putStrLn "Outgoing state is not included in given states. Try insert transition again."
        getTransFunc states alphabet
    | inGoingState `notElem` states = do
        putStrLn "Ingoing state is not included in given states. Try insert transition again."
        getTransFunc states alphabet
    | otherwise = (transition:) <$> getTransFunc states alphabet

-- gets initial states of LNFA, and at the same time checks if it is valid
getInitialS :: States_LNFA (StateID String) -> IO (States_LNFA (StateID String))
getInitialS states = do
    line <- getLine
    case apply parseStates line of
        Nothing -> do
            putStrLn "Not valid input. Try again."
            getInitialS states
        Just (initialS, _) -> testInitialStates initialS states

-- funciton for testing validity of initial state
testInitialStates :: States_LNFA (StateID String) -> States_LNFA (StateID String) -> IO (States_LNFA (StateID String))
testInitialStates [] _ = return []
testInitialStates (initialS@(StateID id):restInitialS) allStates
    | initialS `notElem` allStates = do
        putStrLn ("Unknown state: " ++ show id ++ ". Try insert states again.")
        getInitialS allStates
    | otherwise = (initialS:) <$> testInitialStates restInitialS allStates

-- gets and returns accepting states of LNFA, same inplementation as for initial states
getAcceptS :: States_LNFA (StateID String) -> IO (States_LNFA (StateID String))
getAcceptS = getInitialS

-- shows final DFA to user using Show instance for Automaton_DFA
showDFA :: Automaton_DFA [StateID String] -> IO ()
showDFA automaton = do
    putStrLn "Here is yours converted automaton:"
    putStrLn ""
    print automaton
    putStrLn ""
    putStrLn ""

-----------------------------------------------------------------------------------------------

-- type for creating parsers
newtype Parser a = Parser (String -> Maybe (a, String))

-- a Parser that always fails
fail :: Parser a
fail = Parser (const Nothing)

-- reads any char from string
anyChar :: Parser Char
anyChar = Parser (\s ->
    case s of
        "" -> Nothing
        c : cs -> Just (c, cs)
    )

apply :: Parser a -> String -> Maybe (a, String)
apply (Parser p) = p

-- a parser combinator
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser (\s ->
    case apply p s of
        Just (x, s') -> Just (x, s')
        Nothing -> apply q s
    )

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure x =  Parser (\s -> Just (x, s))
    (<*>) = ap

instance Monad Parser where
    return = pure
    p >>= q = Parser (\s -> do
        (x, s') <- apply p s
        (y, s'') <- apply (q x) s'
        return (y, s'')
      )

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    c <- anyChar
    if f c then return c else fail

parseSpace :: Parser Char
parseSpace = satisfy isSpace

parseLetter :: Parser Char
parseLetter = satisfy isAlpha

parseWord :: Parser String
parseWord = parseMany1 parseLetter

-- parse nothing, return an empty list
parseNone :: Parser [a]
parseNone = return []

-- (many p) parses 0 or more instances of p, i.e. p*  (never fails)
parseMany :: Parser a -> Parser [a]
parseMany p = parseMany1 p <|> parseNone

-- (many p1) parses 1 or more instances of p, i.e. p+  (could fail)
parseMany1 :: Parser a -> Parser [a]
parseMany1 p = do
    x <- p
    xs <- parseMany p
    return (x : xs)

-- (token p) parses a p, optionally preceded by whitespace
token :: Parser a -> Parser a
token p = parseMany parseSpace >> p

-- (sym c) parses the character c, optionally preceded by whitespace, or fails
parseSym :: Char -> Parser Char
parseSym c = token (satisfy (== c))

parseStates :: Parser [StateID String]
parseStates = parseMany1 parseState

parseState :: Parser (StateID String)
parseState = StateID <$> (token . parseMany1 . satisfy) (/= ' ')

parseAlphabet :: Parser Alphabet
parseAlphabet = parseMany1 parseAlphSymb

parseAlphSymb :: Parser InSymbID
parseAlphSymb = token anyChar

parseTrans_LNFA :: Parser (Trans_LNFA (StateID String))
parseTrans_LNFA = Trans_LNFA <$> parseAlphSymb <*> parseState <*> parseState

parseLambdaTrans_LNFA :: Parser (Trans_LNFA (StateID String))
parseLambdaTrans_LNFA = Lambda_LNFA <$> parseState <*> parseState

parseTransFuncLine :: Parser (Trans_LNFA (StateID String))
parseTransFuncLine = parseTrans_LNFA <|> parseLambdaTrans_LNFA

-----------------------------------------------------------------------------------------------

-- main fucntion for converting LNFA to DNFA, it cheks inserted LNFA, if it is valid and then calls function for making DFA
convertLNFA_DFA ::(Eq a, Ord a, Show a) => Automaton_LNFA a -> Automaton_DFA [a]
convertLNFA_DFA automaton_LNFA = let
    preprocesed_LNFA = preprocessAndcheckLNFA automaton_LNFA
    automaton_DFA = makeDFAFromLNFA preprocesed_LNFA
    in automaton_DFA

-- function for preprocessing and checking LNFA, removes duplicates from automaton and chekcs its validity
preprocessAndcheckLNFA :: (Ord a, Show a) => Automaton_LNFA a -> Automaton_LNFA a
preprocessAndcheckLNFA automaton = let
    deDupStates = (remDupAndSort . states_LNFA) automaton
    deDupAlphabet = (remDupAndSort . alphabet_LNFA) automaton
    _ = checkTransFuncLNFA automaton
    deDupTransFunc = (remDupAndSort. transFunc_LNFA) automaton
    _ = foldl (\_ initS -> initS `elem` states_LNFA automaton || error ("Initial state" ++ show initS ++ "is not state of automaton!")) True (initialS_LNFA automaton)
    deDupInitialS = (remDupAndSort . initialS_LNFA) automaton
    _ = foldl (\_ acceptS -> acceptS `elem` states_LNFA automaton || error ("Accepting state" ++ show acceptS ++ "is not state of automaton!")) True (acceptS_LNFA automaton)
    deDupAcceptS = (remDupAndSort . acceptS_LNFA) automaton
    in Automaton_LNFA deDupStates deDupAlphabet deDupTransFunc deDupInitialS deDupAcceptS

-- checking transition funciton of LNFA for valid transitions
checkTransFuncLNFA :: (Eq a, Show a) => Automaton_LNFA a -> Bool
checkTransFuncLNFA automaton = foldl checkTransition True (transFunc_LNFA automaton)
    where
        checkTransition acc transition = case transition of
            Lambda_LNFA outGoing inGoing ->
                if outGoing `notElem` states_LNFA automaton
                    then error ("State " ++ show outGoing ++ " from transition function is not state of automaton!")
                    else inGoing `elem` states_LNFA automaton || error ("State " ++ show inGoing ++ " from transition function is not state of automaton!")
            Trans_LNFA symb outGoing inGoing ->
                if symb `notElem` alphabet_LNFA automaton
                    then error ("Symbol " ++ show symb ++ " from transition function is not alphabet of automaton!")
                    else if outGoing `notElem` states_LNFA automaton
                        then error ("State " ++ show outGoing ++ " from transition function is not state of automaton!")
                        else inGoing `elem` states_LNFA automaton || error ("State " ++ show inGoing ++ " from transition function is not state of automaton!")

-- makes DFA from LNFA by using sub-set construction method
makeDFAFromLNFA :: (Ord a) => Automaton_LNFA a -> Automaton_DFA [a]
makeDFAFromLNFA automaton = let
    initialS = lambdaClosure (initialS_LNFA automaton) (transFunc_LNFA automaton)
    alphabet = alphabet_LNFA automaton
    (states, transFunc) = processLNFA_DFA (transFunc_LNFA automaton) alphabet [initialS] [] [initialS] 
    acceptS = [s|s <-states, not $ null $ s `intersect` acceptS_LNFA automaton]
    in Automaton_DFA states alphabet transFunc initialS acceptS

-- returns states and transition funciton of DFA, states are searched iteratively
processLNFA_DFA :: (Ord a) => [Trans_LNFA a] -> Alphabet -> States_DFA [a] -> [Trans_DFA [a]] -> States_DFA [a] -> (States_DFA [a], [Trans_DFA [a]])
processLNFA_DFA _ _ states_DFA transFunc_DFA [] = (states_DFA, transFunc_DFA)
processLNFA_DFA transFuncLNFA alphabet states_DFA transFunc_DFA (toProcess:restToProcess) = let
    statesForAlphabetTransitions= [newState_DFA | symb <- alphabet, let newState_DFA = lambdaClosure (transitFromStates toProcess symb transFuncLNFA) transFuncLNFA]
    (newStates, newTransitions) = processNewStates toProcess statesForAlphabetTransitions alphabet states_DFA
    in processLNFA_DFA transFuncLNFA alphabet (states_DFA ++ remDupAndSort newStates) (transFunc_DFA ++ newTransitions) (restToProcess ++ remDupAndSort newStates)

-- gets new states for every symbol of alphabet and returns transitions for them and returns such states, that wasnt made in previous iterations yet
processNewStates :: (Eq a) => State_DFA [a] -> States_DFA [a] -> Alphabet -> States_DFA [a] -> (States_DFA [a], [Trans_DFA [a]])
processNewStates _ [] [] oldStates = ([], [])
processNewStates outGoingState ([]:restStates) (_:restAlphabet) oldStates = processNewStates outGoingState restStates restAlphabet oldStates
processNewStates outGoingState (state:restStates) (sym:restAlphabet) oldStates
    | state `elem` oldStates = (restNewStates, Trans_DFA sym outGoingState state:restNewTransitions)
    | otherwise = (state:restNewStates, Trans_DFA sym outGoingState state:restNewTransitions)
    where
        (restNewStates, restNewTransitions) = processNewStates outGoingState restStates restAlphabet oldStates

-- makes lambda closure of inserted states, respectively to LNFA transition function, returns DFA state
lambdaClosure ::(Ord a) => States_LNFA a -> [Trans_LNFA a] -> States_LNFA a
lambdaClosure states transFunc  = remDupAndSort (states ++ [inGoing|s <- states, Lambda_LNFA outGoing inGoing <- transFunc, outGoing == s])

-- returns set of LNFA states, which we will obtain from transition by symbol 'transSymn' from inserted LNFA states
transitFromStates ::(Ord a) => States_LNFA a -> InSymbID -> [Trans_LNFA a] -> States_LNFA a
transitFromStates states transSymb transFunc = remDupAndSort [inGoing|s <- states, Trans_LNFA symb outGoing inGoing <- transFunc, symb == transSymb, outGoing == s]

-- removes duplicate values in list by sorting them, returns sorted list without duplicates
remDupAndSort ::(Ord a) => [a] -> [a]
remDupAndSort = map head . group . sort


