module Evaluator (runInterpreter) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map
import Data.String
import AbsGramma
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad      ( when )
import Control.Monad.Trans.Cont


type Name = Ident
type Loc = Int

type VEnv = Map Name Loc

type Continuation a = a -> ContT () (StateT Store (ExceptT String IO)) ()

data Env = Env {
    varEnv :: VEnv,
    breakC :: Continuation (),
    continueC :: Continuation (),
    returnC :: Continuation Value
}

data Value
    = IntVal Integer
    | StrVal String
    | BoolVal Bool
    | VoidVal
    | Undefined
    | FunVal {isRef :: [Bool] , cont :: [Loc] -> Continuation Value -> Continuation ()}
    | Array { elements :: [Loc] }

-- FunVal: isRef = lista, czy kolejne argumenty być wzięte przez referencję (wpp. przez wartość, tzn. jest kopiowany), Loc - lokacje w pamięci tych argumentów w momencie wywołania

instance Show Value where
    show v = case v of
      IntVal n -> show n
      StrVal s -> s
      BoolVal b -> if b then "true" else "false"
      Array arr -> show arr
      Undefined -> "undefined"
      VoidVal -> "void"
      FunVal _ _ -> "function"

type Store = Map Loc Value

type Eval a = ReaderT Env (ContT () (StateT Store (ExceptT String IO))) a


runInterpreter :: Program -> IO (Either String ())
runInterpreter p = runEval s0 env0 (evalProgram p) where
    s0 = Data.Map.empty
    env0 = Env Data.Map.empty (\_ -> return ()) (\_ -> return ()) (\_ -> lift $ lift $ throwError "")


runEval :: Store -> Env -> Eval () -> IO (Either String ())
runEval s0 env0 e = runExceptT (evalStateT (evalContT (runReaderT e env0)) s0)

evalProgram :: Program -> Eval ()
evalProgram (Program p) = evalStmts p

evalBlock :: Block -> Eval ()
evalBlock (Block stmts) = do
    env <- ask
    local (const env) (evalStmts stmts)

throwRuntimeError :: String -> Eval a
throwRuntimeError s = lift $ lift $ throwError $ "Runtime error: " ++ s ++ ".\n"

fun :: Block -> Env -> [ArgType] -> Value
fun s env args = FunVal (Prelude.map isRef_ args) (\locs k _ -> runReaderT (evalBlock s) $ Prelude.foldr envUpdate (env {returnC = k}) (zip (argNames args) locs))

declareVar :: VEnv -> Item -> Eval VEnv
declareVar env i = do
    case i of
        NoInit x -> do
            l <- gets newloc
            modify $ Data.Map.insert l Undefined
            return $ Data.Map.insert x l env
        Init x e -> do
            val <- evalExprSafe e
            l <- gets newloc
            modify $ Data.Map.insert l val
            return $ Data.Map.insert x l env
        InitArr x e -> do
            (IntVal n) <- evalExprSafe e
            l <- alloc Undefined
            locs <- mapM (alloc . const Undefined) [1..n]
            let arr = Array locs
            modify $ Data.Map.insert l arr
            return $ Data.Map.insert x l env

evalStmts :: [Stmt] -> Eval ()
evalStmts [] = return ()
evalStmts (s:stmts) = do
    case s of
        (FunDecl _ name args b) -> do
            env <- ask
            l <- gets newloc
            let newEnv = envUpdate (name, l) env -- środowisko z zadeklarowaną tą funkcją
            let localEnv = newEnv { breakC = \_ -> return (), continueC = \_ -> return (), returnC = \_ -> return ()}
            let fVal = fun b localEnv args
            modify $ Data.Map.insert l fVal
            local (const newEnv) (evalStmts stmts)
            return ()
        (VarDecl _ items) -> do
            env <- ask
            updatedEnv <- foldM declareVar (varEnv env) items
            local (const $ env { varEnv = updatedEnv }) (evalStmts stmts)
        _ -> do
            evalStmt s
            evalStmts stmts


evalStmt :: Stmt -> Eval ()

evalStmt (BStmt s) = evalBlock s

evalStmt (FunDecl _ _ _ _) = throwRuntimeError "Unexpected runtime error" -- obliczane w evalStmts
evalStmt (VarDecl _ _) = throwRuntimeError "Unexpected runtime error" -- obliczane w evalStmts

evalStmt (Ass var e) = do
    val <- evalExprSafe e
    case var of
     VarName name -> do
         loc <- getVarLoc name
         modify $ Data.Map.insert loc val
     VarArrEl name idx -> do
           (IntVal idx) <- evalExprSafe idx
           (Array arr) <- lookupValue name
           i <- checkInRange idx (length arr)
           modify $ Data.Map.insert (arr !! i) val


evalStmt (Ret r) = do
    k <- asks returnC
    case r of
        RetExpr e -> do
            n <- evalExprSafe e
            lift $ k n
        RetVoid -> do
            lift $ k VoidVal

evalStmt (Cond e s) = do
    (BoolVal b) <- evalExprSafe e
    if b then evalBlock s else return ()

evalStmt (CondElse e s1 s2) = do
    (BoolVal b) <- evalExprSafe e
    if b then evalBlock s1 else evalBlock s2

evalStmt (While e s) = do
    (BoolVal b) <- evalExprSafe e
    env2 <- ask
    lift $ callCC $ \k -> do
        if b then do
            callCC $ \k' -> do
                runReaderT (local (\env -> env {breakC = k, continueC = k'}) (evalBlock s)) env2
            runReaderT (evalStmt (While e s)) env2
        else
            return ()

evalStmt Break = do
    k <- asks breakC
    lift $ k ()

evalStmt Continue = do
    k <- asks continueC
    lift $ k ()

evalStmt (Incr x) = evalStmt (Ass x (EAdd (EVar x) Plus (ELitInt 1)))
evalStmt (Decr x) = evalStmt (Ass x (EAdd (EVar x) Minus (ELitInt 1)))

evalStmt (Print e) = do
    n <- evalExprSafe e
    s <- get
    case n of
      Array locs -> do
          let (Just vals) = mapM (\l -> Data.Map.lookup l s) locs
          liftIO $ print vals
      _ -> liftIO $ print n
    return ()

evalStmt (SExpr e) = do
    evalExpr e
    return ()

evalStmt Skip = return ()

evalExprSafe :: Expr -> Eval Value
evalExprSafe e = do
    val <- evalExpr e
    case val of
      Undefined -> throwRuntimeError "reference to uninitialized variable"
      VoidVal -> throwRuntimeError "reference to void value - function did not return a value (?)"
      _ -> return val

evalExpr :: Expr -> Eval Value
evalExpr ELitTrue = return (BoolVal True)
evalExpr ELitFalse = return (BoolVal False)
evalExpr (ELitInt n) = return (IntVal n)
evalExpr (ELitString s) = return (StrVal s)

evalExpr (ELitArr l) = do
    vals <- mapM evalExprSafe l
    locs <- mapM alloc vals
    return $ Array locs


evalExpr (EVar x) = case x of
    VarName x -> lookupValue x
    VarArrEl x e -> do
        (IntVal idx) <- evalExprSafe e
        (Array locs) <- lookupValue x
        i <- checkInRange idx (length locs)
        (Just el) <- gets $ Data.Map.lookup (locs !! i)
        return el

evalExpr (ECall f args) = do
    (FunVal isRef fun) <- lookupValue f
    locs <- mapM prepareArgs $ zip isRef args
    lift $ callCC $ \k -> do
        fun locs k ()
        return VoidVal

evalExpr (Neg e) = do
    (IntVal n) <- evalExprSafe e
    return $ IntVal (-n)

evalExpr (Not e) = do
    (BoolVal b) <- evalExprSafe e
    return $ BoolVal (not b)

evalExpr (EAnd e1 e2) = do
    (BoolVal b1) <- evalExprSafe e1
    (BoolVal b2) <- evalExprSafe e2
    return $ BoolVal (b1 && b2)

evalExpr (EOr e1 e2) = do
    (BoolVal b1) <- evalExprSafe e1
    (BoolVal b2) <- evalExprSafe e2
    return $ BoolVal (b1 || b2)

evalExpr (EMul e1 op e2) = do
    (IntVal n1) <- evalExprSafe e1
    (IntVal n2) <- evalExprSafe e2
    case op of
        Times -> return $ IntVal (n1 * n2)
        Div -> if n2 == 0 then throwRuntimeError "dividing by 0" else return $ IntVal (n1 `div` n2)
        Mod -> return $ IntVal (n1 `mod` n2)

evalExpr (EAdd e1 op e2) = do
    (IntVal n1) <- evalExprSafe e1
    (IntVal n2) <- evalExprSafe e2
    case op of
        Plus -> return $ IntVal (n1 + n2)
        Minus -> return $ IntVal (n1 - n2)

evalExpr (ERel e1 op e2) = do
    (IntVal n1) <- evalExprSafe e1
    (IntVal n2) <- evalExprSafe e2
    case op of
        LTH -> return $ BoolVal (n1 < n2)
        LE -> return $ BoolVal (n1 <= n2)
        GTH -> return $ BoolVal (n1 > n2)
        GE -> return $ BoolVal (n1 >= n2)
        EQU -> return $ BoolVal (n1 == n2)
        NE -> return $ BoolVal (n1 /= n2)


-- Funkcje pomocnicze

envUpdate :: (Name, Loc) -> Env -> Env
envUpdate (x, l) env = env { varEnv = Data.Map.insert x l (varEnv env)}

argNames :: [ArgType] -> [Name]
argNames = Prelude.map getName where
    getName a = case a of
      ArgCopy _ id -> id
      ArgRef _ id -> id

isRef_ :: ArgType -> Bool
isRef_ a = case a of
    ArgCopy _ id -> False
    ArgRef _ id -> True

prepareArgs :: (Bool, Expr) -> Eval Loc
prepareArgs (isRef, e) =
    if isRef then do
        let (EVar var) = e
        case var of
          VarName id -> getVarLoc $ getVarName var
          VarArrEl id ex -> do
              (IntVal l) <- evalExprSafe ex
              return (fromInteger l)
    else do
        v <- evalExprSafe e
        l <- gets newloc
        case v of
          Array locs -> do
            newLocs <- mapM (alloc . const Undefined) locs
            s <- get
            let (Just vals) = mapM (flip Data.Map.lookup s) locs
            mapM_ (\(l, v) -> modify $ Data.Map.insert l v) (zip locs vals)
            modify $ Data.Map.insert l (Array newLocs)
            return l
          _ -> do
            modify $ Data.Map.insert l v
            return l

newloc :: Store -> Loc
newloc s =
  case Data.Map.toDescList s of
    [] -> 0
    ((k,_):_) -> k + 1

alloc :: Value -> Eval Loc
alloc v = do
    l <- gets newloc
    modify $ Data.Map.insert l v
    return l

checkInRange :: Integer -> Int -> Eval Int
checkInRange idx size = do
    let i = fromInteger idx
    if i < 0 || i >= size then
        throwRuntimeError "index out of range"
    else
        return i

-- zwraca wartość w pamięci, która znajduje się w lokacji wskazującej przez zmienną (pierwszy argument)
lookupValue :: Ident -> Eval Value
lookupValue x = do
    env <- asks varEnv
    state <- get
    let (Just l) = Data.Map.lookup x env
    val <- gets $ Data.Map.lookup l
    case val of
        Just v -> return v
        Nothing -> throwRuntimeError $ "Value of variable " ++ show x ++ "is undefined"

getVarName :: Var -> Name
getVarName var = case var of
    VarName x -> x
    VarArrEl x _ -> x

getVarLoc :: Name -> Eval Loc
getVarLoc x = do
    (Just loc) <- asks $ Data.Map.lookup x . varEnv
    return loc
