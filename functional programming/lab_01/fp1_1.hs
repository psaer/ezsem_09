data Operator = Plus | Minus | Mult deriving (Show, Eq)

data UnaryOperator = UnMinus deriving (Show, Eq)

data Term = IntConstant { intValue::Int }
          | Variable { varName::String }
          | BinaryTerm { lhv::Term, rhv::Term, op::Operator } 
          | UnaryTerm {unaryOp::UnaryOperator, value::Term} deriving (Show, Eq)

infixl 6 <+>
(IntConstant a) <+> (IntConstant b) = IntConstant (a + b)
a <+> b = BinaryTerm a b Plus

infixl 6 <->
(IntConstant a) <-> (IntConstant b) = IntConstant (a - b)
a <-> b = BinaryTerm a b Minus

infixl 7 <*>
(IntConstant a) <*> (IntConstant b) = IntConstant (a * b)
a <*> b = BinaryTerm a b Mult

infixl 8 <-->
(IntConstant a) = IntConstant (-a)
(<-->) a = UnaryTerm UnMinus a

replaceVar :: Term -> String -> Term -> Term
replaceVar (IntConstant intValue) _ _ = IntConstant intValue
replaceVar (Variable varName) var term = if varName == var then term else Variable varName
replaceVar (BinaryTerm lhv rhv op) var term = BinaryTerm (replaceVar lhv var term) (replaceVar rhv var term) op
replaceVar (UnaryTerm op trm) var term = UnaryTerm op (replaceVar trm var term)