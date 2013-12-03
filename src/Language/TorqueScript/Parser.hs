module Language.TorqueScript.Parser where

import Text.Parsec
import Language.TorqueScript.AST
import Control.Monad (liftM)

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
file = liftM File $ (topLevel `endBy` spaces) `thenIgnore` eof

topLevel :: P TopLevel
topLevel = try (liftM TLD definition)
           <|>  liftM TLS statement

definition :: P Definition
definition = package
          <|> liftM FunctionDef function

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
          tillClose = flip manyTill $ close

parens :: P a -> P a
parens f = do
    open >> spaces
    contents <- f
    spaces >> close
    return contents

    where open = char '('
          close = char ')'

parenList :: P a -> P [a]
parenList f = parens $ f `sepBy` (spaces >> char ',' >> spaces)

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
          <|> liftM Exp (expression `thenIgnore` semi)

ifStmt :: P Statement
ifStmt = do
    string "if"
    spaces
    cond <- parens $ expression
    spaces
    body <- blockOf statement
    spaces
    els <- optionMaybe $ do
        try $ string "else" >> spaces >> lookAhead (char '{')
        spaces
        blockOf $ statement
    return $ case els of
        Nothing -> If cond body
        Just e  -> IfElse cond body e

whileStmt :: P Statement
whileStmt = do
    string "while"
    spaces
    cond <- parens $ expression
    spaces
    body <- blockOf statement
    return $ While cond body

expression :: P Expression
expression = liftM Variable (local <|> global)

ident :: P String
ident = do
    first <- under <|> letter
    rest <- many (alphaNum <|> under)
    return $ first : rest

local :: P Name
local = liftM Local $ char '%' >> ident

global :: P Name
global = liftM Global $ char '$' >> ident

semi = char ';'
under = char '_'
spaces1 = skipMany1 space
