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

type P = Parsec [Char] ()

file :: P File
file = do
    tls <- topLevel `endBy` spaces
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
    open >> spaces
    tillClose $ do
        res <- f
        spaces
        return res

    where open = char '{'
          close = char '}'
          tillClose = flip manyTill $ close

parenList :: P a -> P [a]
parenList f = do
    char '(' >> spaces
    contents <- f `sepBy` (spaces >> char ',' >> spaces)
    spaces >> char ')'
    return contents

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
