Complicated AST traversal
=========================

This tutorial will cover a more complex AST analysis: determining whether a variable has been used before it has been defined.
Because TorqueScript does not require variables to be declared before they are used,
typos can introduce errors where an empty string is read from an incorrect variable name.
We'd like to be able to catch these sorts of errors by assuming that if a name has not been assigned to,
then it is probably not defined.

We'll restrict our analysis to local variables inside a function, and assume that all arguments of the function are defined when it is called.

> import Language.TorqueScript.AST
> import Data.Generics.Uniplate.Data (universeBi)
> import Data.List (nub, foldl')

 [basics]: ./Basics.lhs

Here's the function we'll analyse to test our algorithm:

    function addAndPrint(%a, %b) {
       %c = %a + %n;
       echo(%c);
       return %c;
    }

It's a variant of the function used in the [basics tutorial][basics], with a typo introduced here:

    %a + %n

The local variable `%n` is used before it has been assigned to in any way.
Here's that function again in AST form:

> addAndPrint = Function Nothing "addAndPrint" [Local "a", Local "b"]
>   [ Exp (Assign (Variable (Local "c"))
>          Equals
>         (Binary (Variable (Local "a"))
>                  Plus
>                 (Variable (Local "n"))))
>   , Exp (Call Nothing "echo" [Variable (Local "c")])
>   , Return (Just (Variable (Local "n")))
>   ]

How do we go about detecting variables that are read before being written?
Let's think about what we have to work with.
We know we can extract all variables from a `Statement` using the method we covered in the basics tutorial:

    names = nub [n | Variable n <- universeBi stmt]

Taking it a step further, we can use deeper pattern-matching to pull out names that are directly written to:

    assigned = nub [n | Assign (Variable n) Equals _ <- universeBi stmt]

This looks like magic, but it's actually the same syntax as the line above.
We just specify a more detailed pattern to match on and pull `n` out of.
Note that the only way to _define_ a variable in TorqueScript is to write to it with the `=` operator.
Even using `+=` or other assignments imply reading the value of the variable first,
so they can't guarantee that the variable has any initial value.

In addition, function arguments are passed by value, never reference.
So while in C, calling `some_fn(&a);` may cause `a` to become defined, there is no equivalent in TorqueScript.
This makes our code nice and easy!

Before we continue, I'm going to introduce a type that will simplify our remaining code.

> type NameWithPosition = Either Name Name
> type NamesWithPosition = [NameWithPosition]

`Either` is a Haskell type that has two constructors: `Left` and `Right`.
Usually they take two different types - for example `Either Bool Int` can construct `Left Bool`s and `Right Int`s,
but in this case we define them both as `Name`s.
We'll use this to denote whether a variable is defined in a statement (it's on the `Left`) or read from (it's on the `Right`).
The type `NamesWithPosition` is simply defined as a list of its singular type.
I find this a useful idiom!

Now, an easy function to define is the names (with positions) used in a statement.
The type, of course, is:

> namesWithPosition :: Statement -> NamesWithPosition

And nearly as obvious is the definition of the function itself:

> namesWithPosition stmt = map (position assigned) names

Well, okay, maybe that wasn't totally obvious.
Let's break it down.

`names` and `assigned` are going to be the lists we came up with above, representing all names in the statement,
and just the names assigned to in the expression, respectively.
`position` is going to be a function that takes a list of assigned-to names, and a single name,
and decides whether the name should be `Left` (defined) or `Right` (used).
The two lists I won't explain any further - here they are:

>   where assigned = nub [n | Assign (Variable n) Equals _ <- universeBi stmt]
>         names    = nub [n | Variable n <- universeBi stmt]

Now the `position` function.
We see that it's a fairly simple `if` expression that tests whether `n` (the new name) is an element of `as` (the assigned names in the expression).
If so, it can be a `Left` - otherwise it must be a `Right`.

>         position as n = if n `elem` as then Left n else Right n

Well, that takes care of the simplest case - the names in a single expression.
COnsidering _order_ is where this gets complicated.
We can't simply ask for all variables that are ever assigned to in a function and assume they are defined -
a name may be read, and then written to in a later line.
This requires analysing the entire contents of a `Block` as a whole.

The first step is to pull the `Block` out of the `Function` and transform it into a list of `NamesWithPosition`.
We'll do this with the aptly-named `namesWithPositionF`:

> namesWithPositionF :: Function -> [NamesWithPosition]
> namesWithPositionF (Function _ _ ps stmts) = params : contents
>   where params   = map Left ps
>         contents = map namesWithPosition stmts

Remember that `NamesWithPosition` is a list type itself, so the final return type is a list of lists of `Either Name Name`.
The function is not complex; as per our reasoning above, we convert all the function arguments to `Left` values,
then put them at the top of the list comprised of all the other name lists.

Next, we need to accumulate these values into a single result of names that were used before they were defined.
We will do this using a functional technique known as `fold`ing.
Other languages call it `reduce` or even `accumulate`.
Briefly, a left fold uses a function to accumulate the values of a list into another structure.
The folding function should take the accumulated value and a new value from the list, and produce a new acculumated value.
With that in mind, let's define another type:

> type Accumulator = ([Name], [Name])

The type of our accumulating function is a tuple of two lists of `Name`s.
The first list will store all the names that have been _defined so far_ in a function.
The second will be our result value, the list of names that were used before they were defined.
We'll fold through the list of `Statement`s that make up the `Block`, gradually accumulating these lists.

> accumulate :: Accumulator -> NamesWithPosition -> Accumulator
> accumulate (defined, ubd) ns = (defined ++ newDefs, ubd ++ newUbd)
>   where newDefs = [n | Left n <- ns]
>         newUbd  = [n | Right n <- ns,
>                        not (n `elem` defined),
>                        not (n `elem` ubd)]

`accumulate` has the type we determined above, taking first the currently-accumulated value,
then the next element of the list, and returning a new accumulated value.
In this case, `ubd` refers to a list of `Name`s that were **u**sed **b**efore they were **d**efined.
And of course `defined` is a list of names that have previously been defined in the function.
See that as we step through each element of the input list, we gradually increase `defined` with new variable names.
We also add to `ubd` when we find variables that don't already exist in either `defined` or `ubd`.

Finally, we can define the function that puts all these pieces together and performs the fold!
Check this out:

> namesUsedBeforeDefined :: Function -> [Name]
> namesUsedBeforeDefined = snd . foldl' accumulate ([], []) . namesWithPositionF

Remember that `.` represents function composition.
So `namesUsedBeforeDefined` first calls `namesWithPositionF` on its input, then uses `foldl'` with `accumulate` and an empty initial value,
and finally calls `snd` to get the second value out of the tuple returned by the accumulator - the list of undefined names.

And to top it all off, let's write a simple main function that tests this for us:

> main = print (namesUsedBeforeDefined addAndPrint)

If all went according to plan, you should see `[Local "n"]` printed.
