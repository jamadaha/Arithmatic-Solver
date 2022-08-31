{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}
import Text.ParserCombinators.Parsec;
import Text.Read
import Control.Exception
import Data.Data
import Debug.Trace
import qualified Data.Text as T

data MyException = ScannerError | SyntaxError | OtherError
    deriving(Show, Typeable);

instance Exception MyException;

data Symbol = Error | Num | Add | Sub | Mul | Div | LPar | RPar
    deriving(Eq, Show);

data SyntaxTree = Branch (SyntaxTree, SyntaxTree, SyntaxTree) | Leaf Symbol
    deriving (Eq, Show);

t1 :: (a, b, c) -> a;
t1 (a, _, _) = a;

t2 :: (a, b, c) -> b;
t2 (_, b, _) = b;

t3 :: (a, b, c) -> c;
t3 (_, _, c) = c;

isLeaf :: SyntaxTree -> Bool;
isLeaf (Leaf _) = True;
isLeaf _ = False;

isBranch :: SyntaxTree -> Bool;
isBranch (Branch _) = True;
isBranch _ = False;

-- Assumes that it is symbol
asSymbol :: SyntaxTree -> Symbol;
asSymbol (Leaf leaf) = leaf;
asSymbol (Branch branch) = throw OtherError

asTuple :: SyntaxTree -> (SyntaxTree, SyntaxTree, SyntaxTree)
asTuple (Branch branch) = branch;
asTuple (Leaf leaf) = throw OtherError

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

splitString :: String -> Char -> [String];
splitString input delim = iSplitString input delim [];

iSplitString :: String -> Char -> [String] -> [String];
iSplitString input delim tempOutput = do
    if null input
        then reverse tempOutput;
        else do
    let subString = getSubString input delim;
    iSplitString (drop (length subString + 1) input) delim (subString:tempOutput);

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

isParenthesis :: SyntaxTree -> Bool;
isParenthesis input = do
    if (isLeaf input)
        then False;
        else do
    let tuple = asTuple input;
    if (isBranch (t1 tuple) || isBranch (t3 tuple))
        then False;
        else (((asSymbol (t1 tuple)) == LPar) && ((asSymbol (t3 tuple)) == RPar));

isBinaryOperator :: SyntaxTree -> Bool;
isBinaryOperator input = do
    if (isLeaf input)
        then False;
        else do
    let tuple = asTuple input;
    if (isLeaf(t2 tuple))
        then ((asSymbol(t2 tuple)) `elem` [Add, Sub, Mul, Div])
        else False;

isNumber :: SyntaxTree -> Bool;
isNumber input = do
    if (isBranch input)
        then False;
        else ((asSymbol(input)) == Num);

generateSyntaxTree :: [Symbol] -> SyntaxTree;
generateSyntaxTree input = do
    let currentInput = head input;
    if length input == 1
        then Leaf (head input)
        else do
    if length input == 2
        then trace "Error 2" throw SyntaxError
        else do
    let nextInput = input !! 1;
    if currentInput == LPar
        then do
            let endScopeIndex = getEndOfScope input;
            if endScopeIndex == -1
                then traceShow ("No matching parenthesis " ++ show input) throw SyntaxError;
                else do
            if endScopeIndex == length input - 1
                then (Branch (
                    Leaf LPar,
                    generateSyntaxTree (reverse (drop 1 (reverse (drop 1  input)))),
                    Leaf RPar
                ))
                else (Branch (
                    Branch(Leaf LPar, generateSyntaxTree (drop 1 (take endScopeIndex input)), Leaf RPar),
                    Leaf (input !! (endScopeIndex + 1)),
                    generateSyntaxTree (drop (endScopeIndex + 2) input)))
        else if nextInput == Add || nextInput == Sub || nextInput == Mul || nextInput == Div
            then (Branch (Leaf currentInput, Leaf nextInput, generateSyntaxTree (drop 2 input)));
            else trace "Error 3" throw SyntaxError;

syntaxAnal :: SyntaxTree -> Bool;
syntaxAnal input = do
    if isParenthesis input
        then syntaxAnal (t2 (asTuple input))
        else do
    if isBinaryOperator input
        then syntaxAnal(t1 (asTuple input)) && syntaxAnal(t3 (asTuple input))
        else if isNumber input
            then True
            else throw SyntaxError

processInput :: String -> IO();
processInput input = do
    print ("---- " ++ input ++ " ----");
    let symbols = (lexAnal input []);
    let syntaxTree = generateSyntaxTree symbols;
    let isValidTree = syntaxAnal syntaxTree;
    
    print ("Symbols: " ++ show symbols);
    print ("Syntax Tree: " ++ show syntaxTree);
    print ("Valid: " ++ show isValidTree);

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let exp = splitString contents '\n'
    let unpackedExp = exp;
    mapM_ processInput exp
    