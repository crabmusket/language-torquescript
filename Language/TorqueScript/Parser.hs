module Language.TorqueScript.Parser where

import Text.Parsec
import Language.TorqueScript.AST

-- Trick to allow >> and >>= to be used inside clauses of an <|>. Need to change
-- their fixity to make the <|> operator lower-precedence.
import Prelude hiding ((>>), (>>=))
import qualified Prelude as P ((>>), (>>=))
(>>) = (P.>>)
(>>=) = (P.>>=)
infixr 2 >>, >>=

test = parse file "" "package p { f f} ;;"

type P = Parsec [Char] ()

file :: P File
file = do
    tls <- topLevel `sepBy` spaces
    eof
    return $ File tls

topLevel :: P TopLevel
topLevel = try (definition >>= return . TLD)
           <|>  statement  >>= return . TLS

definition :: P Definition
definition = try package
             <|> function >>= return . FunctionDef

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
    char '{' >> spaces
    manyTill' (char '}') $ do
        res <- f
        spaces
        return res

function = do
    string "f"
    return $ Function Nothing "" [] []

statement :: P Statement
statement = do
    semi
    return $ Return Nothing

ident :: P String
ident = do
    first <- under <|> letter
    rest <- many (alphaNum <|> under)
    return $ first : rest

local :: P Name
local = do
    char '%'
    n <- ident
    return $ Local n

global :: P Name
global = do
    char '$'
    n <- ident
    return $ Global n

semi = char ';'
under = char '_'

spaces1 = skipMany1 space
manyTill' = flip manyTill
