import Text.ParserCombinators.Parsec;
import Text.Read
import Control.Exception
import Data.Data
import Debug.Trace


data MyException = ScannerError | SyntaxError | OtherError
    deriving(Show, Typeable);

instance Exception MyException;

data Symbol = Error | Num | Add | Sub | Mul | Div | LPar | RPar
    deriving(Eq, Show);

data SyntaxTree = Branch (SyntaxTree, SyntaxTree, SyntaxTree) | Leaf Symbol
    deriving (Eq, Show);

indexOf :: [Char] -> Char -> Int -> Int;
indexOf input c index = do
    if null input
        then -1;
        else if head input == c
            then index;
            else indexOf (drop 1 input) c (index + 1);

-- Gets substring until char or end of string
getSubString :: String -> Char -> String;
getSubString input c = do
    let charIndex = indexOf input c 0;
    if charIndex == -1
        then input
        else take charIndex input

-- Gets end of scope
-- Returns -1 if there is no end
getEndOfScope :: [Symbol] -> Int;
getEndOfScope input = getEndOfScopeI input 0 0;

getEndOfScopeI :: [Symbol] -> Int -> Int -> Int;
getEndOfScopeI input nestLevel currentIndex = do
    if null input
        then -1
    else if head input == RPar
        then if nestLevel == 1
            then currentIndex
            else getEndOfScopeI (drop 1 input) (nestLevel - 1) (currentIndex + 1)
    else if head input == LPar
        then getEndOfScopeI (drop 1 input) (nestLevel + 1) (currentIndex + 1)
    else getEndOfScopeI (drop 1 input) nestLevel (currentIndex + 1)

isSymbol :: String -> Bool;
isSymbol str = do
    case readMaybe str :: Maybe Int of
        Just x -> True
        Nothing ->
            (str == "+" ||
            str == "-" ||
            str == "*" ||
            str == "/" ||
            str == "(" ||
            str == ")")

toSymbol :: String -> Symbol;
toSymbol str = do
    case readMaybe str :: Maybe Int of
        Just x -> Num
        Nothing ->
            if str == "+"
                then Add
            else if str == "-"
                then Sub
            else if str == "*"
                then Mul
            else if str == "/"
                then Div
            else if str == "("
                then LPar
            else if str == ")"
                then RPar
            else Error

lexAnal :: String -> [Symbol] -> [Symbol]
lexAnal input symbols = do
    let substr = getSubString input ' ';
    let validSymbol = isSymbol substr;
    if null input
        then reverse symbols
        else if validSymbol
            then lexAnal (drop (length substr + 1) input) (toSymbol substr: symbols)
            else throw ScannerError;

generateSyntaxTree :: [Symbol] -> SyntaxTree;
generateSyntaxTree input = do
    let currentInput = head input;
    if length input == 1
        then Leaf (head input)
        else do
    if length input == 2
        then throw SyntaxError
        else do
    let nextInput = input !! 1;
    if currentInput == LPar
        then do
            let endScopeIndex = getEndOfScope input;
            if endScopeIndex == -1
                then throw SyntaxError;
                else do
            if endScopeIndex == length input - 1
                then trace "A1" (Branch (
                    Leaf LPar,
                    generateSyntaxTree (reverse (drop 1 (reverse (drop 1  input)))),
                    Leaf RPar
                ))
                else trace "A2" (Branch (
                    Branch(Leaf LPar, generateSyntaxTree (drop 1 (take endScopeIndex input)), Leaf RPar),
                    Leaf (input !! (endScopeIndex + 1)),
                    generateSyntaxTree (drop (endScopeIndex + 2) input)))
        else if nextInput == Add || nextInput == Sub || nextInput == Mul || nextInput == Div
            then trace "B" (Branch (Leaf currentInput, Leaf nextInput, generateSyntaxTree (drop 2 input)));
            else if head input == Num
                then trace "C" (Leaf Num)
                else throw SyntaxError;

syntaxAnal :: [Symbol] -> SyntaxTree;
syntaxAnal input = do
      generateSyntaxTree input;

main = do
    print (syntaxAnal [LPar, LPar, Num, Add, Num, RPar, Mul, Num, RPar, Add, LPar, Num, RPar]);
    --print (lexAnal "12 + 47 - 9" []);

--(2 * 2) + 2 + 2
--2 + (2 * 2)
--2 + 2 + (2 * 2)