language-torquescript basics
============================

_This is a Literate Haskell file.
Compile it with `ghc --make Tutorials/Basics.lhs`
or run it directly with `runhaskell Tutorials/Basics.lhs`._

This module allows you to parse, manipulate and render TorqueScript language programs.
The centre of the module is the TorqueScript Abstract Syntax Tree (AST) data type.
It's defined in [`Language.TorqueScript.AST`][AST].
This tutorial will go over the basics of creating and accessing a simple function AST.

 [AST]: ../Language/TorqueScript/AST.hs

> import Language.TorqueScript.AST
> import Data.Generics.Uniplate.Data (universeBi)
> import Data.List (nub)

The AST
-------

The TorqueScript syntax tree revolves around two data types: `Statement` and `Expression`.
An `Expression` is any language construct that has a _value_.
For example, the literal `5` has a value of 5.
The method call `%obj.getPosition()` has a value equal to the object's position.
However, a `while` statement has no value - you can't write `%var = while(...) {...}`.
So control structures like `if`s and loops are value-less `Statements` to differentiate them from `Expressions`.

A valid TorqueScript function contains a list of `Statements`.
Note that many common expressions are also valid TorqueScript statements - for example,
a function call like `exec("file.ts");` is a valid statement in its own right, but also returns a value, so it can be used as an expression.
We'll see how the AST handles this below.

Building a simple function
--------------------------

Let's translate a simple TorqueScript function into its AST representation.
(Note: usually this is what the parser does, but we'll do it by hand here just for fun.)
This function adds two numbers, and prints and returns the result:

    function addAndPrint(%a, %b) {
       %c = %a + %b;
       echo(%c);
       return %c;
    }

Now, I'll introduce the AST completely then go over its meaning.

> addAndPrint = Function Nothing "addAndPrint" [Local "a", Local "b"]
>   [ Exp (Assign (Variable (Local "c"))
>                 Equals
>                 (Binary (Variable (Local "a"))
>                         Plus
>                         (Variable (Local "b"))))
>   , Exp (Call Nothing "echo" [Variable (Local "c")])
>   , Return (Just (Variable (Local "c")))
>   ]

This may look like a lot to digest unless you're used to ASTs and Haskell syntax already.
Let's start with the first line, where we declare a new `Function` AST node:

    addAndPrint = Function Nothing "addAndPrint" [Local "a", Local "b"]

`Function` is both a type and a constructor.
(Think of how, in C++, you might have a `class Something` with a `Something()` constructor.
`Something` is at once a type name and a callable function that constructs an object of that type.
Same deal here.)
It is defined in [the AST module][AST] as a data type:

    data Function = Function (Maybe Namespace) FunctionName Params Block

So to construct a `Function`, we use the `Function` constructor and pass it a namespace (maybe),
a name, a list of parameters, and a block of code to execute.
In this case, `Nothing` is the namespace (there isn't one), `"addAndPrint"` is the name,
and the two local variables are the parameters.
Note that `Local` is a constructor of the type `Name`, and `Params` is actually just a synonym for `[Name]` - a list of `Name`s,
which is what we pass to the constructor.

Now, the final part of the function definition - the `Block`.
This is just another type synonym for `[Statement]` - a list of `Statement`s.
So we give it a list, where each element in the list is a `Statement`.
Let's start from the last element in the list, since it's the simplest.

    , Return (Just (Variable (Local "c")))

`Return` is one of the constructors for the type `Statement`.
It is defined like so:

    data Statement
        = stuff...
        | Return (Maybe Expression)

There's that `Maybe` keyword showing up again.
Basicaly, it means the value you return can be either `Nothing` or a `Just Expression`.
These map to the two cases where you might just write `return;`,
or some complex expression like `return %c[%a.name];`.

So in this case, we _do_ have an expression to return, which is why we use `Just`.
Then we make an `Expression` using the `Variable` constructor,
and give it the value `Local "c"` because we want to `return %c;`.

Following these principles, let's look at the second line of the function body.

    , Exp (Call Nothing "echo" [Variable (Local "c")])

This line starts with `Exp`.
This is one of `Statement`'s constructors, defined like:

    data Statement
        = Exp Expression
        | stuff...

So if we want a statement that is actually just a simple expression (not a control structure or anything else)
then we have to wrap it in `Exp`.
This is because Haskell requires all elements of a list to be the same type.
If you're making a `[Statement]`, a list of `Statement`s, you can't just stick an `Expression` in there.
You have to wrap it in a `Statement`.

Anyway, the contents of this expression are fairly simple - we call the function in much the same way as we defined it,
with a `Nothing` namespace, name `"echo"`, and one parameter.
Note that parameters are expressions, so we can't pass `Local "c"` as a parameter itself.
It is of type `Name`, so, like `Exp` above, we have to wrap it in an `Expression`.

Now, you should have enough information to decode that first line, keeping in mind a few more constructors:

    data Expression
        = stuff...
        | Assign Expresion Assignment Expression
        | Binary Expression Op Expression

The `Assign` constructor creates an assignment like `%x = %y`.
The `Assignment` type represents valid assignment operations like `=` and `+=`.
The `Binary` constructor is similar, but is used for other binary operators like additon and comparison.

And that's your first TorqueScript function represented as an AST!

A word of caution
-----------------

This library's AST does not match the tree used by the Torquecript compiler exactly.
This AST type is more permissive - for example, a literal like `5;` is allowed to be a statement.
Not all valid AST objects represent valid TS programs, though the correlation is very close.

Playing in the tree
-------------------

Let's implement some simple functions to get a list of all variables used in a function.
We'll list them as `Name` objects so we can tell `Local`s from `Global`s
(even though we don't use any global variables in our simple function).
First, we obviously want a function that takes a `Function` and returns a list of `Name`s, a `[Name]`.

> variablesInFunction :: Function -> [Name]

What should it do?
Well, it needs to

 * Get the list of variables defined in each expression
 * Stick all the sublists together into one flat list
 * Remove duplicate entries

Haskell's functional style and standard library make this a quick definition:

> variablesInFunction (Function _ _ _ stmts) = (nub . concat . map variablesInStatement) stmts

Briefly, `nub` gets unique elements of a list,
`concat` flattens a list of lists, and `map function` applies `function` to every element of a list.
The `.` function is composition, which you may have seen in high school maths as a large empty dot: `(f1 . f2 . f3) x == f1 (f2 (f3 x))`.
In this function, we use pattern-matching to extract the list of statements from the function definition, then do our magic.

Now all we need to do is define the `variablesInStatement` function, which gets the variables out of, you guessed it, an `Statement` type.
The usual way to do this would be to write the function with lots of pattern matches, one for each constructor of `Statement`,
which call some function `varialbesInExpression` on each `Expression` term in the statement.
However, we can use library functions to do away with all that boilerplate code.

Biplates to the rescue!
Check this out:

> variablesInStatement :: Statement -> [Name]
> variablesInStatement stmt = [n | Variable n <- universeBi stmt]

That list comprehension will only match against `Variable`-constructed values in the list `universeBi stmt`,
which is like a recursively flattened representation of that statement's AST.
Note that although the type of `stmt` is `Statement`, we match againat the `Variable` constructor,
which creates values of type `Expression`.
Wicked, right?

Now we can put our new functions to use:

> main = print (variablesInFunction addAndPrint)

Run this file as described above to view the result.
It should apper something like this:

    [Local "c", Local "a", Local "b"]

And congratulations!
That's a simple AST analysis... from here, the sky's the limit!
