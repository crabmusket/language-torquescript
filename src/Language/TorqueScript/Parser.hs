module Language.TorqueScript.Parser where

import Text.Parsec
import Language.TorqueScript.AST
import Control.Monad (liftM, void)
import Control.Applicative ((<$>), (<*>))

-- Trick to allow >> and >>= to be used inside clauses of an <|>. Need to change
-- their fixity to make the <|> operator lower-precedence.
import Prelude hiding ((>>), (>>=))
import qualified Prelude as P ((>>), (>>=))
(>>) = (P.>>)
--(>>=) = (P.>>=)
infixr 2 >> --, >>=

-- Convenience function for a commonish pattern. Same as (<*) from
-- Control.Applicative, I think.
p `thenIgnore` q = do
    stuff <- p
    q
    return stuff

type P = Parsec [Char] ()

file :: P File
file = File <$> (topLevel `endBy` spaces) `thenIgnore` eof

topLevel :: P TopLevel
topLevel = try (TLD <$> definition)
           <|> (TLS <$> statement)

definition :: P Definition
definition = package
          <|> FunctionDef <$> function

package :: P Definition
package = do
    string "package"
    spaces1
    name <- ident
    spaces
    defs <- blockOf function
    return $ PackageDef name defs

blockOf :: P a -> P [a]
blockOf f = do
    open >> spaces
    tillClose $ f `thenIgnore` spaces

    where open = char '{'
          close = char '}'
          tillClose = flip manyTill close

parens :: P a -> P a
parens = between (open >> spaces) (spaces >> close)
    where open = char '('
          close = char ')'

parenList :: P a -> P [a]
parenList f = parens $ f `sepBy` (spaces >> char ',' >> spaces)

spaced :: P a -> P a
spaced = between spaces spaces

function = do
    string "function"
    spaces1
    (ns, name) <- fname
    spaces
    params <- parenList local
    spaces
    stmts <- blockOf statement
    return $ Function ns name params stmts

fname :: P (Maybe Namespace, FunctionName)
fname = do
    first <- fident
    second <- optionMaybe $ do
        string "::"
        fident
    case second of
        Nothing -> return (Nothing, first)
        Just n -> return (Just first, n)

    where fident = do
            first <- under <|> letter
            rest <- many (alphaNum <|> under)
            return $ first : rest

statement :: P Statement
statement  =  try ifStmt
          <|> try whileStmt
          <|> try doWhileStmt
          <|> try forStmt
          <|> try forEach
          <|> try forEachString
          <|> try switch
          <|> try switchString
          <|> try (loopCntrl `thenIgnore` semi)
          <|> try (returnStmt `thenIgnore` semi)
          <|> ExpStmt <$> expression `thenIgnore` semi

ifStmt :: P Statement
ifStmt = do
    string "if"
    cond <- spaced $ parens expression
    body <- spaced $ blockOf statement
    els <- optionMaybe $ do
        try $ string "else" >> spaces >> lookAhead (char '{')
        spaces
        blockOf statement
    return $ case els of
        Nothing -> If cond body
        Just e  -> IfElse cond body e

whileStmt :: P Statement
whileStmt = do
    string "while"
    cond <- spaced $ parens expression
    body <- blockOf statement
    return $ While cond body

doWhileStmt :: P Statement
doWhileStmt = do
    string "do"
    body <- spaced $ blockOf statement
    string "while"
    cond <- spaced $ parens expression
    semi
    return $ DoWhile body cond

forStmt :: P Statement
forStmt = do
    string "for"
    (init, cond, term) <- spaced . parens $ do
        i <- expression `thenIgnore` (semi >> spaces)
        c <- expression `thenIgnore` (semi >> spaces)
        t <- expression
        return (i, c, t)
    body <- blockOf statement
    return $ For init cond term body

forEach       = forEach' "foreach"  ForEach
forEachString = forEach' "foreach$" ForEachString
forEach' s c = do
    string s
    (name, iter) <- spaced . parens $ do
        n <- local
        spaces
        string "in"
        spaces
        i <- expression
        return (n, i)
    body <- blockOf statement
    return $ c name iter body

switch       = switch' "switch"  Switch
switchString = switch' "switch$" SwitchString
switch' s c = do
    string s
    target <- spaced . parens $ expression
    body <- blockOf cas
    return $ c target body

cas :: P Case
cas = aCase <|> aDefault
    where aCase = do
            string "case"
            cond <- spaced expression
            spaced $ char ':'
            body <- manyTill statement endCase
            return $ Case cond body
          aDefault = do
            string "default"
            spaced $ char ':'
            body <- manyTill statement endCase
            return $ Default body
          endCase = try $ lookAhead $ spaces >> (void (char '}') <|> void cas)

loopCntrl :: P Statement
loopCntrl = try (string "break" >> return Break)
            <|> (string "continue" >> return Continue)

returnStmt :: P Statement
returnStmt = do
    string "return"
    exp <- optionMaybe (spaced expression)
    return $ Return exp

expression :: P Expression
expression = L . Variable <$> (local <|> global)

ident :: P String
ident = do
    first <- under <|> letter
    rest <- many (alphaNum <|> under)
    return $ first : rest

local :: P Name
local = Local <$> (char '%' >> ident)

global :: P Name
global = Global <$> (char '$' >> ident)

semi = spaces >> char ';'
under = char '_'
spaces1 = skipMany1 space
