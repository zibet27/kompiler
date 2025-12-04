package codegen

import ast.*
import ast.visitor.AstVisitor
import ast.visitor.accept
import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.cValuesOf
import kotlinx.cinterop.toCValues
import kotlinx.cinterop.toKString
import llvm.*

@OptIn(ExperimentalForeignApi::class)
class Codegen : CodegenContext(), AstVisitor<LLVMValueRef?> {

    override fun visit(node: Program): LLVMValueRef? {
        // First pass: declare opaque structs
        println("[Codegen] Pass1: struct declarations (${node.decls.size} decls)")
        for (it in node.decls) {
            when (it) {
                is ObjectDecl -> {
                    val structType = LLVMStructCreateNamed(context, it.name)
                    structs[it.name] = structType
                }

                is ObjectDef -> {
                    val structType = structs[it.name] ?: LLVMStructCreateNamed(context, it.name)
                    registerStruct(it.name, structType!!, it.fields)

                    val fieldTypes = it.fields.map { field -> field.type.toLLVM() }.toTypedArray()
                    LLVMStructSetBody(structType, cValuesOf(*fieldTypes), fieldTypes.size.toUInt(), 0)
                }

                else -> continue
            }
        }

        // Declare prototypes for functions and globals
        println("[Codegen] Pass2: prototypes and globals")
        for (it in node.decls) {
            when (it) {
                is FunDecl -> ensureFunction(it.name, it.params, it.type, alien = false)
                is FunDef -> ensureFunction(it.name, it.params, it.type, alien = false)
                is AlienFunDecl -> ensureFunction(it.name, it.params, it.type, alien = true)
                is GlobalVarDecl -> {
                    val baseType = it.type.toLLVM()
                    it.declarators.forEach { d ->
                        addGlobal(d.name, type = baseType.withDimensions(d.arrayDims))
                    }
                }

                else -> continue
            }
        }

        // generate bodies
        println("[Codegen] Pass3: definitions & bodies")
        node.decls.forEach { it.accept(this) }
        return null
    }

    override fun visit(node: FunDecl) = null

    override fun visit(node: FunDef): LLVMValueRef? {
        val fn = functions[node.name]?.value ?: error("Function ${node.name} not found")
        currentFunction = fn

        val entry = addBasicBlock("entry")
        LLVMPositionBuilderAtEnd(builder, entry)

        withScope {
            // Parameters: allocate and store
            val params = mutableListOf<LLVMValueRef>()
            val paramCount = LLVMCountParams(fn).toInt()

            repeat(paramCount) { idx ->
                params += LLVMGetParam(fn, idx.toUInt())!!
            }

            println("[Codegen] generating params")
            node.params.forEachIndexed { i, p ->
                val allocaType = p.type.toLLVM()
                val alloca = buildAlloca(p.name, allocaType)
                LLVMBuildStore(builder, params[i], alloca)
                putLocal(p.name, alloca, allocaType)
            }

            println("[Codegen] generating body")
            val bodyVal = node.body.accept(this)

            println("[Codegen] generating return")

            if (node.returnType.toLLVM().isVoid) {
                LLVMBuildRetVoid(builder)
            } else {
                requireNotNull(bodyVal) { "Function ${node.name} must return a value (last expression of the body)." }
                LLVMBuildRet(builder, bodyVal)
            }
        }
        currentFunction = null
        return null
    }

    override fun visit(node: AlienFunDecl): LLVMValueRef? {
        return null
    }

    override fun visit(node: ObjectDecl): LLVMValueRef? {
        return null
    }

    override fun visit(node: ObjectDef): LLVMValueRef? {
        return null
    }

    override fun visit(node: TypeAlias): LLVMValueRef? {
        return null
    }

    override fun visit(node: GlobalVarDecl): LLVMValueRef? {
        val baseType = node.type.toLLVM()
        node.declarators.forEach { d ->
            val gv = globals[d.name]!!.value
            when (d.init) {
                is WithInit -> {
                    require(!d.arrayDims.isEmpty()) {
                        "Incorrect `with` keyword usage"
                    }
                    val initVal = d.init.expr.accept(this)!!
                    val totalSize = d.totalArraySize
                    val elements = List(totalSize) { initVal }.toCValues()
                    val constArray = LLVMConstArray(
                        ElementTy = baseType, ConstantVals = elements, Length = totalSize.toUInt()
                    )
                    LLVMSetInitializer(gv, constArray)
                }

                is AssignInit -> {
                    require(d.arrayDims.isEmpty()) {
                        "Incorrect `=` keyword usage"
                    }
                    val init = d.init.expr.accept(this)!!
                    LLVMSetInitializer(GlobalVar = gv, ConstantVal = init)
                }
            }
        }
        return null
    }

    override fun visit(node: FieldDecl): LLVMValueRef? = null

    override fun visit(node: Param): LLVMValueRef? = null

    override fun visit(node: Declarator): LLVMValueRef? = null

    override fun visit(node: WithInit): LLVMValueRef? = null

    override fun visit(node: AssignInit): LLVMValueRef? = null

    override fun visit(node: BuiltinType): LLVMValueRef? = null

    override fun visit(node: NamedType): LLVMValueRef? = null

    override fun visit(node: PointerType): LLVMValueRef? = null

    override fun visit(node: FuncType): LLVMValueRef? = null

    override fun visit(node: Block): LLVMValueRef? = withScope {
        node.items.forEachRetLast { it.accept(this) }
    }

    override fun visit(node: LocalVarDecl): LLVMValueRef? {
        val baseType = node.type.toLLVM()
        node.declarators.forEach { d ->
            val allocaType = baseType.withDimensions(d.arrayDims)
            val alloca = buildAlloca(d.name, allocaType)
            putLocal(d.name, alloca, allocaType)

            when (d.init) {
                is WithInit -> {
                    require(!d.arrayDims.isEmpty())

                    val initVal = d.init.expr.accept(this)!!
                    for (i in 0 until d.totalArraySize) {
                        val indices = listOf(zero, i.i32())
                        val elemPtr = buildInBoundsGEP(alloca, allocaType, indices, name = "int_ptr")
                        LLVMBuildStore(builder, initVal, elemPtr)
                    }
                }

                is AssignInit -> {
                    require(d.arrayDims.isEmpty())
                    val v = d.init.expr.accept(this)!!
                    LLVMBuildStore(builder, v, alloca)
                }
            }
        }
        return null
    }

    override fun visit(node: ExprStmt) = node.expr.accept(this)

    override fun visit(node: SkipStmt) = null

    override fun visit(node: StopStmt) = null

    override fun visit(node: IntLit): LLVMValueRef {
        return node.value.i32()
    }

    override fun visit(node: F64Lit): LLVMValueRef {
        return node.value.toLLVM()
    }

    override fun visit(node: CharLit): LLVMValueRef {
        val ty = LLVMInt8TypeInContext(context)
        return LLVMConstInt(ty, N = node.value.toULong(), SignExtend = 0)!!
    }

    override fun visit(node: StringLit): LLVMValueRef {
        return LLVMBuildGlobalStringPtr(builder, node.value, "str")!!
    }

    override fun visit(node: Ident): LLVMValueRef {
        // Variable or function
        val variable = getLocal(node.name) ?: globals[node.name]
        if (variable != null) {
            return load(variable.value, variable.type, node.name)
        }
        return functions[node.name]!!.value
    }

    override fun visit(node: Unary): LLVMValueRef? {
        return when (node.op) {
            UnaryOp.PreInc -> buildIncDec(node.expr, isIncrement = true, isPrefix = true)
            UnaryOp.PreDec -> buildIncDec(node.expr, isIncrement = false, isPrefix = true)
            UnaryOp.AddressOf -> getLValuePtr(node.expr).value
            UnaryOp.Plus -> node.expr.accept(this)
            UnaryOp.Minus -> {
                val v = node.expr.accept(this)
                when {
                    v.type.isFloatLike -> LLVMBuildFNeg(builder, v, "neg")
                    else -> LLVMBuildNeg(builder, v, "neg")
                }
            }

            UnaryOp.BitNot -> LLVMBuildNot(builder, node.expr.accept(this), "not")!!
            UnaryOp.Not -> error("Not operator is not supported in native backend")

            UnaryOp.Deref -> {
                val value = node.expr.accept(this)!!
                println("DEREF")
                load(value, value.type, name = "deref")
            }
        }
    }

    override fun visit(node: Binary): LLVMValueRef? {
        val lv = node.left.accept(this)
        val rv = node.right.accept(this)
        val isFloat = lv.type.isFloatLike

        return when (node.op) {
            BinaryOp.Add -> when (isFloat) {
                true -> LLVMBuildFAdd(builder, lv, rv, "add")
                false -> LLVMBuildAdd(builder, lv, rv, "add")
            }

            BinaryOp.Sub -> when (isFloat) {
                true -> LLVMBuildFSub(builder, lv, rv, "sub")
                false -> LLVMBuildSub(builder, lv, rv, "sub")
            }

            BinaryOp.Mul -> when (isFloat) {
                true -> LLVMBuildFMul(builder, lv, rv, "mul")
                false -> LLVMBuildMul(builder, lv, rv, "mul")
            }

            BinaryOp.Div -> when (isFloat) {
                true -> LLVMBuildFDiv(builder, lv, rv, "div")
                false -> LLVMBuildSDiv(builder, lv, rv, "div")
            }

            BinaryOp.Mod -> LLVMBuildSRem(builder, lv, rv, "mod")
            BinaryOp.Eq, BinaryOp.Ne, BinaryOp.Lt, BinaryOp.Le, BinaryOp.Gt, BinaryOp.Ge -> {
                LLVMConstInt(LLVMInt1TypeInContext(context), 0u, 0)
            }

            BinaryOp.BitAnd -> LLVMBuildAnd(builder, lv, rv, "and")
            BinaryOp.BitOr -> LLVMBuildOr(builder, lv, rv, "or")
            BinaryOp.BitXor -> LLVMBuildXor(builder, lv, rv, "xor")
            BinaryOp.Shl -> LLVMBuildShl(builder, lv, rv, "shl")
            BinaryOp.Shr -> LLVMBuildAShr(builder, lv, rv, "shr")
            BinaryOp.AndAnd, BinaryOp.OrOr -> LLVMConstInt(LLVMInt1TypeInContext(context), 0u, 0)
        }
    }

    override fun visit(node: Call): LLVMValueRef {
        val fn = functions[node.callee.name]!!
        val args = node.args.map { it.accept(this) }.toCValues()
        val name = if (LLVMGetReturnType(fn.type)!!.isVoid) "" else "call"
        return LLVMBuildCall2(builder, fn.type, fn.value, args, node.args.size.toUInt(), name)!!
    }

    override fun visit(node: Index): LLVMValueRef {
        val symbol = getLValuePtr(node)
        return load(symbol.value, symbol.type, name = "index").also {
            println("DONE")
        }
    }

    private fun getLValuePtr(expr: Expr): Symbol = when (expr) {
        is Ident -> findSymbol(expr.name)

        is Index -> {
            println("HEY")
            val (arrPtr, arrType) = getLValuePtr(expr.target)
            val idxVal = expr.index.accept(this)!!
            require(idxVal.type == i32Type) { "Index is not i32 type" }

            when {
                arrType.isArray -> {
                    val gep = buildInBoundsGEP(arrPtr, arrType, indices = listOf(zero, idxVal), name = "idx")
                    Symbol(gep, arrType.elementType)
                }

                arrType.isPointer -> {
                    val gep = buildInBoundsGEP(arrPtr, arrType, indices = listOf(idxVal), name = "idx")
                    Symbol(gep, arrType.elementType)
                }

                else -> error("Can't get element of non-array type.")
            }
        }

        is Unary -> when (expr.op) {
            UnaryOp.Deref -> {
                val ptrVal = expr.expr.accept(this)!!
                require(ptrVal.type.isPointer) { "Attempt to dereference non-pointer type" }
                Symbol(ptrVal, i32Type)
            }

            else -> error("Expression is not an lvalue: ${expr.prettyPrint()}")
        }

//            is Member -> {
//                // Resolve base pointer and value type in memory
//                var (basePtr, baseValTy) = getLValuePtr(expr.target)
//
//                // Support pointer-to-struct variables: load to get the struct pointer
//                if (baseValTy.isPointer) {
//                    val loadedPtr = LLVMBuildLoad2(builder, baseValTy, basePtr, "ld.sptr")!!
//                    basePtr = loadedPtr
//                    baseValTy = LLVMGetElementType(baseValTy)!!
//                }
//
//                // Expect a struct value in memory at basePtr
//                val structNamePtr = LLVMGetStructName(baseValTy)
//                require(structNamePtr != null) { "Member access base is not a struct" }
//                val structName = structNamePtr.toKString()
//                val fields = getStructFields(structName)
//                val fieldIndex = fields.indexOfFirst { it.name == expr.name }
//                require(fieldIndex != -1) { "Unknown field ${expr.name} in struct $structName" }
//
//                val indices = cValuesOf(zero, constI32(fieldIndex.toULong()))
//                val fieldPtr = LLVMBuildInBoundsGEP2(builder, baseValTy, basePtr, indices, 2u, "field_ptr")
//                val fieldTy = LLVMStructGetTypeAtIndex(baseValTy, fieldIndex.toUInt())
//                Pair(fieldPtr!!, fieldTy!!)
//            }

        else -> error("Unknown lvalue expression: ${expr.prettyPrint()}")
    }

    override fun visit(node: Member): LLVMValueRef? {
        val (fieldPtr, fieldType) = getLValuePtr(node)
        // Load the field value
        return LLVMBuildLoad2(builder, fieldType, fieldPtr, "field")
    }

    override fun visit(node: Assign): LLVMValueRef {
        val (lValue, lType) = getLValuePtr(node.target)
        val rValue = node.value.accept(this)!!
        require(rValue.type.kind == lType.kind) {
            println(lType.kind == LLVMHalfTypeKind)
            "LHS and RHS types don't match ${node.target.prettyPrint()} (${rValue.type.kind} vs ${lType.kind})"
        }
        LLVMBuildStore(builder, rValue, lValue)
        return rValue
    }

    override fun visit(node: BlockExpr) = withScope {
        node.block.items.forEachRetLast { it.accept(this) }
    }

    override fun visit(node: IfExpr): LLVMValueRef? {
        // condition
        val condVal = node.cond.accept(this)
        val thenBB = addBasicBlock("then")
        val elseBB = addBasicBlock("else")
        val contBB = addBasicBlock("endif")

        // assume the condition is already i1
        LLVMBuildCondBr(builder, condVal, thenBB, elseBB)

        // then
        LLVMPositionBuilderAtEnd(builder, thenBB)
        val thenVal = node.thenBlock.accept(this)
        if (thenBB.terminator == null) LLVMBuildBr(builder, contBB)

        // else
        LLVMPositionBuilderAtEnd(builder, elseBB)
        val elseVal = node.elseBlock.accept(this)
        if (elseBB.terminator == null) LLVMBuildBr(builder, contBB)

        LLVMPositionBuilderAtEnd(builder, contBB)

        // If both produce a value and types match, create phi
        if (thenVal != null && elseVal != null && thenVal.type == LLVMTypeOf(elseVal)) {
            val phi = LLVMBuildPhi(builder, LLVMTypeOf(thenVal), "if")
            val incomingVals = cValuesOf(thenVal, elseVal)
            val incomingBBs = cValuesOf(thenBB, elseBB)
            LLVMAddIncoming(phi, incomingVals, incomingBBs, 2u)
            return phi
        }

        return null
    }

    override fun visit(node: WhileExpr): LLVMValueRef? {
        val condBB = addBasicBlock("while.cond")
        val bodyBB = addBasicBlock("while.body")
        val contBB = addBasicBlock("while.end")

        LLVMBuildBr(builder, condBB)
        LLVMPositionBuilderAtEnd(builder, condBB)

        val condVal = node.cond.accept(this)
        LLVMBuildCondBr(builder, condVal, bodyBB, contBB)
        LLVMPositionBuilderAtEnd(builder, bodyBB)
        node.body.accept(this)
        if (bodyBB.terminator == null) {
            LLVMBuildBr(builder, condBB)
        }
        LLVMPositionBuilderAtEnd(builder, contBB)
        return null
    }

    override fun visit(node: DoWhileExpr): LLVMValueRef? {
        val bodyBB = addBasicBlock("do.body")
        val condBB = addBasicBlock("do.cond")
        val contBB = addBasicBlock("do.end")
        LLVMBuildBr(builder, bodyBB)
        LLVMPositionBuilderAtEnd(builder, bodyBB)
        node.body.accept(this)
        if (bodyBB.terminator == null) LLVMBuildBr(builder, condBB)
        LLVMPositionBuilderAtEnd(builder, condBB)
        val condVal = node.cond.accept(this)
        LLVMBuildCondBr(builder, condVal, bodyBB, contBB)
        LLVMPositionBuilderAtEnd(builder, contBB)
        return null
    }

    override fun visit(node: ForExpr): LLVMValueRef? {
        // for(init; cond; incr) body
        node.init.accept(this)

        val condBB = addBasicBlock("for.cond")
        val bodyBB = addBasicBlock("for.body")
        val incrBB = addBasicBlock("for.incr")
        val contBB = addBasicBlock("for.end")

        LLVMBuildBr(builder, condBB)
        LLVMPositionBuilderAtEnd(builder, condBB)
        val condVal = node.cond.accept(this)
        LLVMBuildCondBr(builder, condVal, bodyBB, contBB)
        LLVMPositionBuilderAtEnd(builder, bodyBB)
        node.body.accept(this)
        if (bodyBB.terminator == null) LLVMBuildBr(builder, incrBB)
        LLVMPositionBuilderAtEnd(builder, incrBB)
        node.incr.accept(this)
        if (incrBB.terminator == null) LLVMBuildBr(builder, condBB)
        LLVMPositionBuilderAtEnd(builder, contBB)
        return null
    }

    override fun visit(node: SwitchExpr): LLVMValueRef? {
        val scrutinee = node.expr.accept(this)
        val contBB = addBasicBlock("switch.end")
        val defaultBB = addBasicBlock("switch.default")
        val sw = LLVMBuildSwitch(builder, scrutinee, defaultBB, node.cases.size.toUInt())
        val caseVals = mutableListOf<LLVMBasicBlockRef?>()
        val resultVals = mutableListOf<LLVMValueRef?>()
        node.cases.forEach { c ->
            val bb = addBasicBlock("case")
            LLVMAddCase(sw, LLVMConstInt(scrutinee.type, c.value.toULong(), 0), bb)
            LLVMPositionBuilderAtEnd(builder, bb)
            resultVals += c.result.accept(this)
            caseVals += LLVMGetInsertBlock(builder)
            if (bb.terminator == null) LLVMBuildBr(builder, contBB)
        }
        // default
        LLVMPositionBuilderAtEnd(builder, defaultBB)

        val defaultVal = node.defaultCase?.accept(this)
        if (defaultBB.terminator == null) LLVMBuildBr(builder, contBB)

        LLVMPositionBuilderAtEnd(builder, contBB)
        // If all cases yield the same type, create phi
        val incomingVals = mutableListOf<LLVMValueRef?>()
        val incomingBBs = mutableListOf<LLVMBasicBlockRef?>()
        resultVals.forEachIndexed { i, v ->
            if (v != null) {
                incomingVals += v
                incomingBBs += caseVals[i]
            }
        }
        if (defaultVal != null) {
            incomingVals += defaultVal
            incomingBBs += defaultBB
        }
        if (incomingVals.isNotEmpty()) {
            val phiTy = incomingVals.first().type
            val phi = LLVMBuildPhi(builder, phiTy, "swt")
            val vals = incomingVals.toTypedArray()
            val bbs = incomingBBs.toTypedArray()
            LLVMAddIncoming(phi, cValuesOf(*vals), cValuesOf(*bbs), vals.size.toUInt())
            return phi
        }
        return null
    }

    override fun visit(node: SwitchCase) = null

    override fun visit(node: Cast) = null

    private fun buildIncDec(target: Expr, isIncrement: Boolean, isPrefix: Boolean): LLVMValueRef {
        val (ptr, type) = getLValuePtr(target)
        val isFloat = type.isFloatLike
        val cur = load(value = ptr, type, name = "tmp")
        val one = if (isFloat) 1.0.toLLVM() else 1.i32()
        val newVal = if (isIncrement) {
            when (isFloat) {
                true -> LLVMBuildFAdd(builder, cur, one, "inc")
                else -> LLVMBuildAdd(builder, cur, one, "inc")
            }
        } else {
            when (isFloat) {
                true -> LLVMBuildFSub(builder, cur, one, "dec")
                else -> LLVMBuildSub(builder, cur, one, "dec")
            }
        }
        LLVMBuildStore(builder, newVal, ptr)
        return if (isPrefix) newVal!! else cur
    }

    override fun visit(node: PostfixInc): LLVMValueRef {
        return buildIncDec(node.target, isIncrement = true, isPrefix = false)
    }

    override fun visit(node: PostfixDec): LLVMValueRef {
        return buildIncDec(node.target, isIncrement = false, isPrefix = false)
    }

    companion object {
        fun generate(ast: Program): String = with(Codegen()) {
            visit(ast)
            val cstr = LLVMPrintModuleToString(module)
            val ir = cstr?.toKString() ?: error("Failed to compile LLVM IR")
            LLVMDisposeMessage(cstr)
            return ir
        }
    }
}

