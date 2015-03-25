module ErrorChecking where

import Prelude hiding (LT, GT, EQ, id)
import FirstClassFunctions hiding (evaluate)
import Operators
import Data.Char

data Checked a = Good a | Error String
  deriving Show

evaluate :: Exp -> Env -> Checked Value
evaluate (Literal v) env = Good v

evaluate (Unary op a) env =
  case evaluate a env of
    Error msg -> Error msg
    Good av ->   checked_unary op av

evaluate (Binary op a b) env =
  case evaluate a env of
    Error msg -> Error msg
    Good av ->
      case evaluate b env of
        Error msg -> Error msg
        Good bv ->
          checked_binary op av bv

evaluate (Variable x) env =
  case lookup x env of
    Nothing -> Error ("Variable " ++ x ++ " undefined")
    Just v  -> Good v

evaluate (If a b c) env = 
  case evaluate a env of
    Error msg -> Error msg
    Good v -> case v of
                IntV t -> Error ("Int " ++ show t ++ " isn't a Boolean Expression")
                ClosureV s exp env -> Error ("The 'if' statement evaluated to be a function. Function declarations aren't Boolean Expressions")
                BoolV t -> 
                  let BoolV test = BoolV t in
                    if t then case evaluate b env of
                                  Error msg -> Error msg
                                  Good bv -> Good bv
                            else case evaluate c env of
                                  Error msg -> Error msg
                                  Good cv -> Good cv

--check for cases where you declare x to a variable y that isn't defined
evaluate (Declare x exp body) env =
  case evaluate exp env of
    Error msg -> Error msg
    Good v -> let newEnv = (x, v) : env in
      case evaluate body newEnv of
        Error msg -> Error msg
        Good val -> Good val

evaluate (Function x body) env = Good (ClosureV x body env)

--check for misc cases
evaluate (Call fun arg) env =
  case evaluate fun env of
    Error msg -> Error msg
    Good val -> 
      case val of
        IntV v -> Error ("Incorrect Paramaters passed to function call")
        BoolV v -> Error ("Incorrect Paramaters passed to function call")
        _ -> let ClosureV x body closeEnv = val in
                case evaluate arg env of
                    Error msg -> Error msg
                    Good v -> let newEnv = (x, v) : closeEnv in
                        case evaluate body newEnv of
                          Error msg -> Error msg
                          Good result -> Good result


--evaluate (Call fun arg) env = evaluate body newEnv    -- changed
  --where ClosureV x body closeEnv = evaluate fun env
    --    newEnv = (x, evaluate arg env) : closeEnv

evaluate (TryCatch exp1 exp2) env = 
    case evaluate exp1 env of
      Good val -> Good val
      Error msg -> evaluate exp2 env 

execute exp = evaluate exp []

checked_unary :: UnaryOp -> Value -> Checked Value
checked_unary Not (BoolV b) = Good (BoolV (not b))
checked_unary Neg (IntV i)  = Good (IntV (-i))
checked_unary op   v         = 
    Error ("Unary " ++ show op ++ " called with invalid argument " ++ show v)

checked_binary :: BinaryOp -> Value -> Value -> Checked Value
checked_binary Add (IntV a)  (IntV b)  = Good (IntV (a + b))
checked_binary Sub (IntV a)  (IntV b)  = Good (IntV (a - b))
checked_binary Mul (IntV a)  (IntV b)  = Good (IntV (a * b))
checked_binary Div _         (IntV 0)  = Error "Divide by zero"
checked_binary Div (IntV a)  (IntV b)  = Good (IntV (a `div` b))
checked_binary And (BoolV a) (BoolV b) = Good (BoolV (a && b))
checked_binary Or  (BoolV a) (BoolV b) = Good (BoolV (a || b))
checked_binary LT  (IntV a)  (IntV b)  = Good (BoolV (a < b))
checked_binary LE  (IntV a)  (IntV b)  = Good (BoolV (a <= b))
checked_binary GE  (IntV a)  (IntV b)  = Good (BoolV (a >= b))
checked_binary GT  (IntV a)  (IntV b)  = Good (BoolV (a > b))
checked_binary EQ  a         b         = Good (BoolV (a == b))
checked_binary op  a         b         = 
    Error ("Binary " ++ show op ++ 
           " called with invalid arguments " ++ show a ++ ", " ++ show b)
