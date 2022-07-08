module TypeChecker where
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Map
import AbsGramma

type TCM a = ExceptT String (ReaderT TCMEnv Identity) a
type Err = String

type Name = Ident
type Depth = Int -- na jakim poziomie została zadeklarowana zmienna, zaczynając od zera
type TEnv = Map (Name, Depth) Type

data TCMEnv = TCMEnv {
    tEnv :: TEnv, -- aktualne środowisko, zadeklarowane zmienne wraz z poziomem zagnieżdżenia na którym zostały zadeklarowane i ich typy
    depth :: Depth,
    returnType :: Type -- jeśli wykonanie w funkcji, to jaki jest oczekiwany typ zwracanej funkcji, domyślnie Void
}

runTypeChecker :: Program -> Either Err ()
runTypeChecker p = runIdentity (runReaderT (runExceptT (checkTypeProgram p)) env0) where
    env0 = TCMEnv Data.Map.empty 0 TVoid

checkTypeProgram :: Program -> TCM ()
checkTypeProgram (Program p) = checkTypeStmts p

checkTypeBlock :: Block -> TCM ()
checkTypeBlock (Block s) = do
    env <- ask
    let d = depth env
    local (const $ env {depth = d + 1}) (checkTypeStmts s)

-- Jeśli już jest zadeklarowana w lokalnym zasięgu, to zwraca błąd
declareVar :: Depth -> Name -> Type -> TEnv -> TCM TEnv
declareVar d x t env = do
    let t' = Data.Map.lookup (x, d) env
    case t' of
      Nothing -> return $ Data.Map.insert (x, d) t env
      Just t -> throwError $ "Redeclaration of variable " ++ strIdent x ++ "\n"

-- Sprawdza poprawność typów oraz updatuje środowisko
checkTypeItem :: Type -> TEnv -> Item -> TCM TEnv
checkTypeItem t env i = do
    d <- asks depth
    case i of
        NoInit x -> declareVar d x t env
        Init x e -> do
            t' <- checkTypeExpr e
            if t' == TVoid then throwError $ "Wrong type, trying to initialize variable of type " ++ strType t ++ " with value of type void\n"
            else
                checkEqualTypes t t'
            declareVar d x t env
        InitArr x e -> do
            t' <- checkTypeExpr e
            case t' of
                TInt -> declareVar d x (TArray t) env
                _ -> throwError "Size of array must be of type int\n"

checkTypeStmts :: [Stmt] -> TCM ()
checkTypeStmts [] = return ()
checkTypeStmts (s:stmts) = case s of
    FunDecl t name args s -> case t of
        TFun _ _ -> throwError "Unexpected type error, cant declare function returning function\n"
        _ -> do
            env <- ask
            let d = depth env
            updatedTEnv <- declareVar d name (TFun t args) (tEnv env) -- środowisko z zadeklarowaną aktualną funkcją
            localFunEnv <- foldM (updateFunTypeEnv (d+1)) updatedTEnv args
            local (const $ TCMEnv localFunEnv (d+1) t) (checkTypeBlock s) -- sprawdz poprawność typów instrukcji wewnątrz funkcji
            local (const $ env { tEnv = updatedTEnv }) (checkTypeStmts stmts)
    VarDecl t items -> case t of
        TVoid -> throwError "Cannot declare variable of type void\n"
        TFun _ _ -> throwError "Unexpected type error (internal type), cant declare type function\n"
        _ -> do
            env <- ask
            updatedEnv <- foldM (checkTypeItem t) (tEnv env) items
            local (const $ env { tEnv = updatedEnv }) (checkTypeStmts stmts)
    _ -> do
          checkTypeStmt s
          checkTypeStmts stmts


checkTypeStmt :: Stmt -> TCM ()
checkTypeStmt x = case x of
  BStmt b -> checkTypeBlock b
  Ass var e -> do
      t <- checkVarType var
      case t of
        TFun _ _ -> throwError "Can't assign value to a function\n"
        TArray t' -> checkExprType t' e
        _ -> checkExprType t e
      return ()
  Ret e -> do
      retType <- asks returnType
      case e of
        RetExpr e -> checkTypeExpr e >>= checkEqualTypes retType
        RetVoid -> checkEqualTypes retType TVoid
      return ()
  Skip -> return ()
  Cond e s -> do
      checkExprType TBool e
      checkTypeBlock s
  CondElse e s1 s2 -> do
      checkExprType TBool e
      checkTypeBlock s1
      checkTypeBlock s2
  While e s -> do
      checkExprType TBool e
      checkTypeBlock s
  Break -> return ()
  Continue -> return ()
  Incr var -> do
      t <- checkVarType var
      case t of
        TInt -> return ()
        _ -> throwError "variable is not of type int, can't increase it\n"
  Decr var -> do
      t <- checkVarType var
      case t of
        TInt -> return ()
        _ -> throwError "variable is not of type int, can't increase it\n"
  Print e -> do
      t <- checkTypeExpr e
      case t of
        TFun _ _ -> throwError "can't print type function\n"
        TVoid -> throwError "can't print type void\n"
        _ -> return ()
  SExpr e -> do
      checkTypeExpr e
      return ()
  FunDecl _ _ _ _ -> throwError "Unexpected typecheck error\n" -- ta instrukcja powinna być wyewaluowana w evalStmts
  VarDecl _ _ ->  throwError "Unexpected typecheck error\n" -- jw.


-- zawsze zwraca tylko któryś z typów: TBool, TInt, TString, TVoid (funkcja zwracająca void aka procedura), TArray T, gdzie T \in {TBool, TInt, TString, TVoid (pusty literał tablicowy)}
checkTypeExpr :: Expr -> TCM Type
checkTypeExpr ELitTrue = return TBool
checkTypeExpr ELitFalse = return TBool
checkTypeExpr (ELitInt n) = return TInt
checkTypeExpr (ELitString s) = return TString

-- tablice mogą przechowywać tylko elementy typów podstawowych : bool, int, string. Wyjątkowo checkTypeExpr może zwrócić typ void - dla pustego literału tablicowego []
checkTypeExpr (ELitArr l) =  deduceArrayType l
checkTypeExpr (Neg e) = checkExprType TInt e
checkTypeExpr (Not e) = checkExprType TBool e
checkTypeExpr (EAnd e1 e2) = checkExprsType TBool e1 e2
checkTypeExpr (EOr e1 e2) = checkExprsType TBool e1 e2
checkTypeExpr (EMul e1 op e2) = checkExprsType TInt e1 e2
checkTypeExpr (EAdd e1 op e2) = checkExprsType TInt e1 e2

checkTypeExpr (ERel e1 op e2) = do
    checkExprsType TInt e1 e2
    return TBool

checkTypeExpr (EVar e) = checkVarType e
checkTypeExpr (ECall x vals) = do
    t <- lookupType x
    case t of
        TFun ret args -> do
            checkNrOfArg vals args
            checkArgTypes vals args
            return ret
        _ -> throwError $ strIdent x ++ " is not a function\n"

--
-- Funkcje pomocnicze

unpackArg :: ArgType -> (Type, Ident)
unpackArg arg = case arg of
  ArgCopy t id -> (t, id)
  ArgRef t id -> (t, id)

updateFunTypeEnv :: Depth -> TEnv -> ArgType -> TCM TEnv
updateFunTypeEnv d env arg = let (t, x) = unpackArg arg in declareVar d x t env

checkVarType :: Var -> TCM Type
checkVarType var = do
    case var of
        VarName x -> lookupType x
        VarArrEl x idx -> do
            checkIsIndex idx
            t <- lookupType x
            case t of
                TArray t' -> return t'
                _ -> throwError $ "Variable " ++ strIdent x ++ " is not an array.\n"

-- wyszukuje do jakiego typu odwołuje się dana zmienna w aktualnym środowisku
searchMap :: TEnv -> Name -> Depth -> Maybe Type
searchMap _ _ (-1) = Nothing
searchMap env x d = let t = Data.Map.lookup (x,d) env in case t of
                      Nothing -> searchMap env x (d-1)
                      Just t -> Just t

lookupType :: Name -> TCM Type
lookupType x = do
    env <- asks tEnv
    d <- asks depth
    let maybeType = searchMap env x d
    case maybeType of
      Nothing -> throwError $ undeclaredVariable x
      Just t -> return t


checkEqualTypes :: Type -> Type -> TCM Type
checkEqualTypes t1 t2 = if t1 /= t2 then throwError $ wrongTypeError t1 t2 else return t1

-- czy expressions podane jako argumenty mają oczekiwany typ (pierwszy argument)
checkExprType :: Type -> Expr -> TCM Type
checkExprType t e = do
    t1 <- checkTypeExpr e
    checkEqualTypes t t1
    return t

checkExprsType :: Type -> Expr -> Expr -> TCM Type
checkExprsType t e1 e2 = do
    t1 <- checkTypeExpr e1
    t2 <- checkTypeExpr e2
    checkEqualTypes t t1
    checkEqualTypes t t2

checkIsIndex :: Expr -> TCM Type
checkIsIndex e = do
    i <- checkTypeExpr e
    case i of
      TInt -> return TInt
      _ -> throwError $ "Array index must be of type Int, type of passed arg: " ++ strType i ++ "\n"

-- Przy założeniu że listy są tej samej długości
checkArgTypes :: [Expr] -> [ArgType] -> TCM ()
checkArgTypes [] [] = return ()
checkArgTypes (v:vs) (a:as) = do
    case a of
        (ArgCopy t x) -> checkExprType t v
        (ArgRef t x) -> case v of
            EVar x -> checkExprType t v
            _ -> throwError $ "You can only pass rvalue by reference\n"
    return ()

checkNrOfArg :: [Expr] -> [ArgType] -> TCM ()
checkNrOfArg vals args =
    let n1 = length vals; n2 = length args in
        when (n1 /= n2) $ throwError $ "Wrong number of arguments. Expected: " ++ show n2 ++ ", got: " ++ show n1 ++ "\n"


checkArrayType :: [Expr] -> Type -> TCM Type
checkArrayType [] t = return $ TArray t
checkArrayType (x:xs) t = do
    t' <- checkTypeExpr x
    if t == t' then
        checkArrayType xs t
    else
        throwError "Array elements have to be of the same type\n"

deduceArrayType :: [Expr] -> TCM Type
deduceArrayType [] = return $ TArray TVoid
deduceArrayType (x:xs) = do
    t <- checkTypeExpr x
    checkArrayType xs t

wrongTypeError :: Type -> Type -> Err
wrongTypeError t t' = "Wrong type, expected: " ++ strType t ++ ", got: " ++ strType t' ++ "\n"

undeclaredVariable :: Ident -> Err
undeclaredVariable x = "Use of undeclared identifier " ++ strIdent x ++ "\n"

strType :: Type -> String
strType t = case t of
  TInt -> "int"
  TString -> "string"
  TBool -> "bool"
  TVoid -> "void"
  TArray t' -> "array of " ++ strType t'
  TFun ty ats -> "function"

strIdent :: Ident -> String
strIdent (Ident s) = s