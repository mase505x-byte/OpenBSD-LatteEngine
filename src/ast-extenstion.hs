data TopDef
	=FnDef.. 	-- existing
	| ExternDecl
	{ extName     :: Ident
	, extArgs     :: [Type]
	, extRet      :: Type
	, extConv     :: CallConv
	}

data CallConv = CSysV | CStdcall

data Expr
  = ...
  | EApp Expr [Expr] 	-- existing Latte call
  | EExternCall Ident [Expr]

