TorqueScript abstract syntax tree
=================================

This file defines the types and abtract data structures that represent the
syntax tree of a TorqueScript program.

> {-# LANGUAGE DeriveDataTypeable #-}
> module Language.TorqueScript.AST where
>
> import Data.Data

The top level
-------------

A TS file can contain, at the top level, both regular statements (if, while)
as well as definitions (functions and packages). To preserve the order in which
they apear in the file, these statements are in a single list, otherwise I'd pop
them in separate lists to save using another discriminative type.

> data File = File [TopLevel]
>     deriving (Eq, Show, Typeable, Data)
>
> data TopLevel = TLD Definition | TLS Statement
>     deriving (Eq, Show, Typeable, Data)

These `deriving (Eq, Show, Typeable, Data)` lines that follow every data
type declaration simply declare common (automatic) interfaces that these types
obey. `Typeable` and `Data` are required for analysis with `uniplate`.

Definitions include functions and packages. Packages contain only function
definitions; no other code should be possible inside them.

> data Definition
>     = FunctionDef Function
>     | PackageDef PackageName [Function]
>     deriving (Eq, Show, Typeable, Data)
>
> type PackageName = Ident

Function definitions have an optional Namespace they are defined in. While
Names have namespace syntax (::), it doesn't affect how they work.
Functions actually care about this property, so it's made explicit here.

> data Function = Function (Maybe Namespace) FunctionName Params Block
>     deriving (Eq, Show, Typeable, Data)

> type FunctionName = Ident
> type Params = [Name]
> type Block = [Statement]

Statements
----------

A Statement is an arrangement of Expressions that defines program structure.
The TS interpreter abhors statements that are not assignments or calls, such
as 5;. They are allowed here, again, for simplicity of definition and parsing
and would not be accepted by the TS compiler.
Curiously, break and continue are allowed anywhere, but seem to be dynamically
ignored when outside a loop.

> data Statement
>     = If Expression Block
>     | IfElse Expression Block Block
>     | While Expression Block
>     | DoWhile Block Expression
>     | For Expression Expression Expression Block
>     | ForEach Name Expression Block
>     | ForEachString Name Expression Block
>     | Switch Expression CaseBlock
>     | SwitchString Expression CaseBlock
>     | Break | Continue
>     | Return (Maybe Expression)
>     | Exp Expression 
>     deriving (Eq, Show, Typeable, Data)

Yes, case labels can contain an arbitrary expression! Isn't that exciting?

> data Case
>     = Case Expression Block
>     | Default
>     deriving (Eq, Show, Typeable, Data)

> type CaseBlock = [Case]

Expressions
-----------

An Expression is, simply, something that has a value. We don't need to deal
with parenthetical expressions or order of operations here because this is
just an AST, not a grammar. Those are both implicit in the finally constructed
tree. The parser, on the other hand, needs to worry about them.

> data Expression
>     = Assign Expression Assignment Expression
>     | Call (Maybe Namespace) FunctionName Arguments
>     | Binary Expression Op Expression
>     | Post Unary Expression -- No prefix :(
>     | Negate Expression
>     | SlotAccess Expression Expression
>     | ArrayAccess Expression Expression
>     | IntLit Int
>     | FloatLit Float
>     | StringLit String
>     | TaggedStringLit String
>     | Variable Name
>     | ObjectExp Object
>     deriving (Eq, Show, Typeable, Data)

> type Arguments = [Expression]

This is annoying because we need to account for object creation in general
expressions as well as inside an object creation.

> data Object = Create ObjectConstructor ObjectClass (Maybe ObjectName) ObjectContents
>     deriving (Eq, Show, Typeable, Data)

> type ObjectContents = [Either Member Object]
> type ObjectName = Ident
> type ObjectClass = Either Ident Expression

Different ways of constructing new objects.

> data ObjectConstructor
>     = New
>     | Datablock
>     | Singleton
>     deriving (Eq, Show, Typeable, Data)

Syntax inside object creation is fairly limited. We can assign to members
(members have no % prefix like variables), or construct new objects.

> data Member = Member MemberName Expression
>     deriving (Eq, Show, Typeable, Data)

> type MemberName = Ident

Boring operators.

> data Op
>     = Plus | Minus | Times | Div
>     | Complement | And | Or | XOr
>     | LeftShift | RightShift
>     | Cat | Space | Line | Tab
>     | IsEqual | IsStringEqual | IsNotEqual | IsNotStringEqual
>     | LessThan | GreaterThan | LessThanEqual | GreaterThanEqual
>     deriving (Eq, Show, Typeable, Data)

And types of assignment. I originally planned to implement these all as Ops
and allow Binary to express assignment as well. However this just feels a bit
more right, and truer to how the language is interpreted.

> data Assignment
>     = Equals
>     | PlusEquals | MinusEquals | TimesEquals | DivEquals
>     | AndEquals | OrEquals | XOrEquals
>     | LeftShiftEquals | RightShiftEquals
>     deriving (Eq, Show, Typeable, Data)

There are only two unary operations, so this is a bit of overkill, but I like
the style it confers for declaring/matching ASTs: Post Increment (exp)

> data Unary = Increment | Decrement
>     deriving (Eq, Show, Typeable, Data)

A Name represents an actual variable, as opposed to an Ident which can be
used for function names and everything else. Note that therefore the Ident
will not include the % or $ sign, since these are determined by whether the
Local or Global conctructor is used. Still deciding whether that is helpful,
since it will require more pattern matching on things that should probably
be treated the same way.

> data Name = Local Ident | Global Ident
>     deriving (Eq, Show, Typeable, Data)

Nothing to see here.

> type Namespace = Ident

After all that, the mystical Ident is just a String.

> type Ident = String
