Beyond basics: tree pruning
===========================

Let's have a look at the basic method of modifying the AST: Uniplate's `transform` function.
In this instalment we'll write a function that removes any lines of code after a `return` statement in a function.

 [basics]: ./Basics.lhs
 [AST]: ../Language/TorqueScript/AST.hs

> import Language.TorqueScript.AST
> import Data.Generics.Uniplate.Data (transform)
> import Data.List (nub)

The function we'll be working with is a modified version of the function introduced in the [last tutorial][basics],
but with a bit more complexity.
I've added an `if` statement to show that our solution will also remove code from nested blocks,
and I've added some code after the return statements so we can actually see stuff happen.

    function deadCode(%a, %b) {
       %c = %a + %b;
       if(%c > 10) {
          return %a;
          echo(%a);
       }
       return %c;
       echo(%c);
    }

I'm not going to go over the details of constructing the AST again,
but here's the function constructed as such:

> deadCode = Function Nothing "deadCode" [Local "a", Local "b"]
>   [ Exp (Assign (Variable (Local "c"))
>                 Equals
>                 (Binary (Variable (Local "a"))
>                         Plus
>                         (Variable (Local "b"))))
>   , If (Binary (Variable (Local "c")) GreaterThan (IntLit 10))
>       [ Return (Just (Variable (Local "a")))
>       , Exp (Call Nothing "echo" [Variable (Local "a")])
>       ]
>   , Return (Just (Variable (Local "c")))
>   , Exp (Call Nothing "echo" [Variable (Local "c")])
>   ]

Notice that after we remove impossible-to-reach code (i.e. code after a return statement),
there should be no `Call` expressions, because they occur only in unreachable locations.
WHat we want is some function, let's call it `removeCodeAfterReturn`, that transforms a function.
Hence we say it maps from a `Function` to a `Function`, and will have the type

> removeCodeAfterReturn :: Function -> Function

Remember the definition of a `Function` from the [AST][]:

    data Function = Function (Maybe Namespace) FunctionName Params Block

Now, the important part of the function to modify is the `Block` at the end.
To extract that from the rest of the function, we'll use pattern-matching to consruct a new `Function` with the modified block:

> removeCodeAfterReturn (Function a b c stmts) =
>                        Function a b c stmts'

Sorry for the wacky indentation - I just like the way the constructors line up.
We take `stmts'` to be our transformed block, which we must now define in the `where` clause:

>   where stmts' = transform (takeUntil isReturn) stmts

There's that `transform` method I mentioned.
Basically, and this goes for many Uniplate features, `transform` takes some function and applies it recursively to our AST as appropriate.
In this case, we are transforming `stmts`, which is a `Block` or `[Statement]`,
so our transforming function must have type `Block -> Block`.

The `takeUntil` function has type `(a -> Bool) -> [a] -> [a]`.
As in, it takes a function from some type `a` to a `Bool`, then a list of `a`s, and returns a list of `a`s.
What it actually returns, of course, is all items of the list it is given,
up to and including the first element that causes the function to return `True`.
The idea is that in a list of statements, we only need to keep the ones before and including the first return.

If we let type `a` be `Statement`, we have: `(Statement -> Bool) -> Block -> Block`.
`isReturn` provides the `Statement -> Bool` part of this type,
returning `True` if it's ever given a value constructed with `Return`.

>         isReturn stmt = case stmt of
>           Return _ -> True
>           otherwise -> False

(Note that this code fragment is still inside the `where` clause opened above, hence the indentation.)
When applied to `takeWhile`, the resulting type is `Block -> Block`, exactly what we need to transform the AST.
`takeUntil` is not actually a standard function, so I define it now:

> takeUntil p xs = foldr (\x r -> if p x then [x] else x:r) [] xs

Don't worry, I hardly understand that either.
But, pressing swiftly onward!
Once more we'll define a main method that will print our transformed function:

> main = print (removeCodeAfterReturn deadCode)

That should print an ugly mess of an AST, but you can easily check that `Call` is nowhere to be found!
Both occurrences of the statements including the `echo` calls were pruned by our transformer.
