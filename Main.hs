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
		_ -> evalStmt env (BlockStmt stmts)
--end BlockStmt

--if 
evalStmt env (IfSingleStmt expr stmt) = do
	Bool b <- evalExpr env expr
	if b then do
		a <- evalStmt env stmt
		return a
	else return Nil

--end if
--if/else
evalStmt env (IfStmt expr stmt stmt2) = do
	Bool b <- evalExpr env expr
	if b then do
		a <- evalStmt env stmt
		return a
	else do
		c <- evalStmt env stmt2			
		return c
--end if/else
--while
evalStmt env (WhileStmt expr stmt) = do
	Bool b <- evalExpr env expr
	if b then do
		a <- evalStmt env stmt
		case a of 
			Break -> return Nil
			_ -> evalStmt env (WhileStmt expr stmt)
	else return Nil
--end while
--break
evalStmt env (BreakStmt _) = return Break
--


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

--
-- Environment and auxiliary functions
--
-- environment modificado, agora temos uma lista de maps
environment :: [Map String Value]
environment = [Map.empty]

--funções para criar e remover escopos
createScope = \s -> (Nil, Map.empty:s)
removeScope = \s -> (Nil, (tail s))

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
			
createAutomaticGlobalVar id val = ST $ \s -> (val, insertGlobalVar id val s)
createLocalVar id val = ST $ \s -> (val, (insert id val (head s)):(tail s))
			
insertGlobalVar id val [] = []
insertGlobalVar id val (x:[]) = (insert id val x):[] 
insertGlobalVar id val (x:xs) = x:(insertGlobalVar id val xs)

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

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
showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union (head defs) (head environment)) ++ "\n" ++ showResult (val, (tail defs))

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f Map.empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
