import Language.TorqueScript.AST
import Data.Generics.Uniplate.Data
import Data.List (nub)

vars = variables damage

damage =
 let
    db  = Local "db"
    obj = Local "obj"
    dam = Local "dam"
    ns  = Just $ Namespace "SceneObject"
 in Function ns "damage" [obj, dam]
    [ Exp (Assign (Variable db) Equals (SlotAccess (Variable obj) (Call Nothing "getDataBlock" [])))
    , Exp (SlotAccess (Variable db) (Call Nothing "onDamage" [Variable obj, Variable dam]))
    , Return Nothing
    ]

class HasVariables a where
    variables :: a -> [Ident]

instance HasVariables Expression where
    variables e = concat [variables v | Variable v <- universe e]

instance HasVariables Name where
    variables (Local  s) = [s]
    variables (Global s) = [s]

instance HasVariables Function where
    variables (Function _ _ _ stmts) = nub . concat . map variables $ stmts

instance HasVariables Statement where
    variables (Exp e) = variables e
    variables _       = []
