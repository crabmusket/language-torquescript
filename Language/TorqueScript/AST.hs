{-# LANGUAGE DeriveDataTypeable #-}
module Language.TorqueScript.AST where

import Data.Data

{- A TS file can contain, at the top level, both regular statements (if, while)
 - as well as definitions (functions and packages). To preserve the structure of
 - the file, these statements are in a single list, otherwise I'd pop them in
 - separate lists to save using an Either type. -}
data File = File [Either Statement Definition]
    deriving (Eq, Ord, Show, Typeable, Data)

{- Definitions include functions and packages. I'll stray from the actual TS
 - implementation here for the sake of expediency and say a Package contains a
 - list of Definitions. In actuality, Packages may only contain function
 - definitions. -}
data Definition
    = FunctionDef FunctionName Params Statements
    | PackageDef PackageName Definitions
    deriving (Eq, Ord, Show, Typeable, Data)

{- Most things are a Statement. Except, for example, some short expressions like
 - 5;. They are allowed here, again, for simplicity of definition and parsing
 - and would not be accepted by the TS compiler. -}
data Statement
    = IfElse Expression Block (Maybe Block)
    | While Expression Block
    | For Expression Expression Expression Block
    | ForEach Name Expression Block
    | ForEachString Name Expression Block
    | ExpStmt Expression
    deriving (Eq, Ord, Show, Typeable, Data)

data Expression
    = Binary Expression Op Expression
    | SlotAccess
    | ArrayAccess 
    | LiteralExp Literal
    | VariableExp Name

data Literal
    = IntLit Int
    | FloatLit Float
    | StringLit String

data Name = Local Ident | Global Ident
    deriving (Eq, Ord, Show, Typeable, Data)

data Ident = Ident String
    deriving (Eq, Ord, Show, Typeable, Data)

type FunctionName = Ident
type Statements = [Statement]
type Expressions = [Expression]
type Definitions = [Definition]
type Params = [Name]
