import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (StringLit str) = return $ String str
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    x <- stateLookup env var;
	e <- evalExpr env expr;
    case x of
		UnderfinedVar -> createAutomaticGlobalVar var e 
		_ -> setVar var e

evalExpr env (ArrayLit []) = return $ Array []
evalExpr env (ArrayLit array) = return $ Array array

		
--function expression
evalExpr env (FuncExpr mId args stmts) =
	case mId of
		Just (Id name) -> createAutomaticGlobalVar name (Function (Id name) args stmts)
		Nothing -> return (Function (Id "___") args stmts)

evalExpr env (CallExpr expr params) = do
	f <- evalExpr env expr
	case f of
		Function _ args stmts -> do
			createScope
			storeParams env args params
			evalResult <- evalStmt env (BlockStmt stmts)
			removeScope
			case evalResult of
				EmptyFunctReturn -> return Nil
				FunctReturn x -> return x
				_ -> return Nil
		UnderfinedVar -> error $ "função inexistente: " ++ show expr

storeParams _ [] [] = return Nil		
storeParams env ((Id argName):args) (param:params) = do
	v <- evalExpr env param
	createLocalVar argName v
	storeParams env args params
storeParms _ _ _ = error $ "quantidade de parametros incompativel" 
-- end functions expressions
		
evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
--BlockStmt
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (stmt:stmts)) = do
	a <- evalStmt env stmt
	case a of
		Break -> return Break
		FunctReturn x -> return (FunctReturn x)
		EmptyFunctReturn -> return EmptyFunctReturn
		_ -> evalStmt env (BlockStmt stmts)
--end BlockStmt

--if 
evalStmt env (IfSingleStmt expr stmt) = do
	Bool b <- evalExpr env expr
	if b then do
		createScope
		a <- evalStmt env stmt
		removeScope
		return a
	else return Nil

--end if
--if/else
evalStmt env (IfStmt expr stmt stmt2) = do
	Bool b <- evalExpr env expr
	if b then do
		createScope
		a <- evalStmt env stmt
		removeScope
		return a
	else do
		createScope
		c <- evalStmt env stmt2
		removeScope
		return c
--end if/else
--while
evalStmt env (WhileStmt expr stmt) = do
	Bool b <- evalExpr env expr
	if b then do
		createScope
		a <- evalStmt env stmt
		removeScope
		case a of 
			Break -> return Nil
			FunctReturn x -> return (FunctReturn x)
			EmptyFunctReturn -> return EmptyFunctReturn
			_ -> evalStmt env (WhileStmt expr stmt)
	else return Nil
--end while
--break
evalStmt env (BreakStmt _) = return Break
--function
evalStmt env (FunctionStmt (Id name) args stmts) = createAutomaticGlobalVar name (Function (Id name) args stmts)

evalStmt env (ReturnStmt exp) = 
	case exp of
		Nothing -> return EmptyFunctReturn
		Just x -> do
			y <- evalExpr env x
			return (FunctReturn y)

-- end function
--for
evalStmt env (ForStmt init condition increment stmt) = do
	createScope
	evalForInit env init
	Bool conditionResult <- evalForCondition env condition
	if conditionResult then do
		createScope
		evalResult <- evalStmt env stmt
		removeScope
		case evalResult of
			FunctReturn x -> do
				removeScope
				return (FunctReturn x)
			EmptyFunctReturn -> do
				removeScope
				return EmptyFunctReturn
			Break -> do
				removeScope
				return Nil
			_ -> do
				forAux env (ForStmt init condition increment stmt)
				removeScope
				return Nil
	else return Nil
	
forAux env (ForStmt init condition increment stmt) = do
	evalForIncrement env increment
	Bool conditionResult <- evalForCondition env condition
	if conditionResult then do
		createScope
		evalResult <- evalStmt env stmt
		removeScope
		case evalResult of
			FunctReturn x -> return (FunctReturn x)
			EmptyFunctReturn -> return EmptyFunctReturn
			Break -> do
				return Nil
			_ -> do
				forAux env (ForStmt init condition increment stmt)
				return Nil
	else return Nil
	
evalForInit env init = 
	case init of
		VarInit varDecl -> evalStmt env (VarDeclStmt varDecl)
		ExprInit expr -> evalExpr env expr
		_ -> return Nil

evalForCondition env condition =
	case condition of
		Just expr -> evalExpr env expr 
		Nothing -> return (Bool True)

evalForIncrement env increment = 
	case increment of
		Just val -> evalExpr env val
--end for


-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2
infixOp env _  _ _ = return $ error $ "Operação invalida"

--
-- Environment and auxiliary functions
--
-- environment modificado, agora temos uma lista de maps
environment :: [Map String Value]
environment = [Map.empty]

--funções para criar e remover escopos
createScope = ST $ \s -> (Nil, Map.empty:s)
removeScope = ST $ \s -> (Nil, (tail s))

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case levelSearch s var of
        Nothing -> (UnderfinedVar, s)
        Just val -> (val, s)
		
-- levelSearch busca em todos níveis
levelSearch :: [Map String Value] -> String -> Maybe Value
levelSearch [] _ = Nothing
levelSearch (x:xs) var =
	case Map.lookup var x of
		Nothing -> levelSearch xs var
		Just val -> Just val

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> createLocalVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            createLocalVar id val
			
createAutomaticGlobalVar id val = ST $ \s -> (val, createAutomaticGlobalVarAux id val s)
createLocalVar id val = ST $ \s -> (val, (insert id val (head s)):(tail s))
			
createAutomaticGlobalVarAux id val [] = []
createAutomaticGlobalVarAux id val (x:[]) = (insert id val x):[] 
createAutomaticGlobalVarAux id val (x:xs) = x:(createAutomaticGlobalVarAux id val xs)

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, setVarAux var val s)
--auxilia a setVar
setVarAux _ _ [] = error ("Erro inesperado")
setVarAux id val (x:xs) =
	case Map.lookup id x of
		Nothing -> x:(setVarAux id val xs)
		Just _ -> (insert id val x):xs
--
-- Types and boilerplate
--
type StateT = [Map String Value]
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--
-- showResult modificado para mostrar vários níveis da memoria
showResult :: (Value, StateT) -> String
showResult (val, []) = ""
showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union (head defs) (head environment)) ++ "\n" ++ showResult (val, (tail defs))

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f environment

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
