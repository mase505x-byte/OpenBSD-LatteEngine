getExternCall :: Ident -> [Expr] -> Codegen ()
genExternCall name args = do
	-- push args in reverse
	forM_ (reverse args) $ \a -> do
	    genExpr a
		emit "pushq %rax" 
	-- pop integer args into registers
	let intRegs = ["%rdi", "%rsi", "%rdx", "%rdx", "%rcx", "%r8", "%r9"]
		(regs, stackArgs) = splitAt 6 args
	forM_ (zip regs [1..]) $ \(r, _) -> emit ("popq " ++ r)
	-- align to 16 
	emit "movq %rsp %rbx"
	emit "andq $-16, %rsp"
	emit ("call " ++ show name)
	emit "movq %rbx, %rsp"
gccCommand :: FilePath -> FilePath -> FilePath -> FilePath -> String
gccCommand libPath asmPath outPath engineLib = 
	"cc -no-pie " ++ show libPath ++ " " ++ askPath
	++ " " ++ show engineLib ++ " -o " ++ show outPath

