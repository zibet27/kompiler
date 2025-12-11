package codegen

import ast.*
import ast.visitor.AstVisitor
import ast.visitor.accept
import kotlinx.cinterop.ExperimentalForeignApi
import llvm.*

@OptIn(ExperimentalForeignApi::class)
class Codegen : CodegenContext(), AstVisitor<LLVMValueRef?> {

    override fun visit(node: Program): LLVMValueRef? {
        // First pass: declare opaque structs
        println("[Codegen] Pass1: struct declarations (${node.decls.size} decls)")
        for (it in node.decls) {
            when (it) {
                is ObjectDecl -> {
                    structs[it.name] = context.createNamedStruct(it.name)
                }

                is ObjectDef -> {
                    val structType = structs[it.name] ?: context.createNamedStruct(it.name)
                    registerStruct(it.name, structType, it.fields)

                    val fieldTypes = it.fields.map { field -> field.type.toLLVM() }
                    structType.setBody(fieldTypes)
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
        builder.positionAtEnd(entry)

        withScope {
            // Parameters: allocate and store
            val params = mutableListOf<LLVMValueRef>()
            val paramCount = fn.paramCount.toInt()

            repeat(paramCount) { idx ->
                params += fn.getParam(idx.toUInt())
            }

            println("[Codegen] generating params")
            node.params.forEachIndexed { i, p ->
                val allocaType = p.type.toLLVM()
                val alloca = buildAlloca(p.name, allocaType)
                builder.buildStore(params[i], alloca)
                putLocal(p.name, alloca, allocaType)
            }

            println("[Codegen] generating body")
            val bodyVal = node.body.accept(this)

            println("[Codegen] generating return")

            if (node.returnType.toLLVM().isVoid) {
                builder.buildRetVoid()
            } else {
                requireNotNull(bodyVal) {
                    "Function ${node.name} must return a value (last expression of the body)."
                }
                builder.buildRet(bodyVal)
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
                    val elements = List(totalSize) { initVal }
                    val constArray = baseType.constArray(elements)
                    gv.initializer = constArray
                }

                is AssignInit -> {
                    require(d.arrayDims.isEmpty()) {
                        "Incorrect `=` keyword usage"
                    }
                    val init = d.init.expr.accept(this)!!
                    gv.initializer = init
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
                        val elemPtr = builder.buildInBoundsGEP(allocaType, alloca, indices, name = "int_ptr")
                        builder.buildStore(initVal, elemPtr)
                    }
                }

                is AssignInit -> {
                    require(d.arrayDims.isEmpty())
                    val v = d.init.expr.accept(this)!!
                    builder.buildStore(v, alloca)
                }
            }
        }
        return null
    }

    override fun visit(node: ExprStmt) = node.expr.accept(this)

    override fun visit(node: SkipStmt): LLVMValueRef? {
        loopIncrementBlock?.let { builder.buildBr(it) }
        return null
    }

    override fun visit(node: StopStmt): LLVMValueRef? {
        loopExitBlock?.let { builder.buildBr(it) }
        return null
    }

    override fun visit(node: IntLit): LLVMValueRef {
        return node.value.i32()
    }

    override fun visit(node: F64Lit): LLVMValueRef {
        return node.value.f64()
    }

    override fun visit(node: CharLit): LLVMValueRef {
        val ty = context.i8Type()
        return ty.constInt(node.value.toULong())
    }

    override fun visit(node: StringLit): LLVMValueRef {
        return builder.buildGlobalStringPtr(node.value, "str")
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
            UnaryOp.AddressOf -> node.expr.toLValue().value
            UnaryOp.Plus -> node.expr.accept(this)
            UnaryOp.Minus -> {
                val v = node.expr.accept(this)!!
                when {
                    v.type.isFloatLike -> builder.buildFNeg(v, name = "neg")
                    else -> builder.buildNeg(v, name = "neg")
                }
            }

            UnaryOp.BitNot -> builder.buildNot(node.expr.accept(this)!!, "not")
            UnaryOp.Not -> {
                val v = node.expr.accept(this)!!
                val zeroVal = getZero(v.type)
                val cond = if (v.type.isFloatLike) {
                    builder.buildFCmp(LLVMRealPredicate.LLVMRealOEQ, lhs = v, rhs = zeroVal, name = "not")
                } else {
                    builder.buildICmp(LLVMIntEQ, lhs = v, rhs = zeroVal, name = "not")
                }
                builder.buildZExt(cond, boolType)
            }

            UnaryOp.Deref -> {
                val value = node.expr.accept(this)!!
                load(value, value.type, name = "deref")
            }
        }
    }

    override fun visit(node: Binary): LLVMValueRef? {
        if (node.op == BinaryOp.AndAnd || node.op == BinaryOp.OrOr) {
            return visitLogical(node)
        }

        val lv = node.left.accept(this)!!
        val rv = node.right.accept(this)!!
        val isFloat = lv.type.isFloatLike

        return when (node.op) {
            BinaryOp.Add -> when (isFloat) {
                true -> builder.buildFAdd(lv, rv, "add")
                false -> builder.buildAdd(lv, rv, "add")
            }

            BinaryOp.Sub -> when (isFloat) {
                true -> builder.buildFSub(lv, rv, "sub")
                false -> builder.buildSub(lv, rv, "sub")
            }

            BinaryOp.Mul -> when (isFloat) {
                true -> builder.buildFMul(lv, rv, "mul")
                false -> builder.buildMul(lv, rv, "mul")
            }

            BinaryOp.Div -> when (isFloat) {
                true -> builder.buildFDiv(lv, rv, "div")
                false -> builder.buildSDiv(lv, rv, "div")
            }

            BinaryOp.Mod -> builder.buildSRem(lv, rv, "mod")
            BinaryOp.Eq, BinaryOp.Ne, BinaryOp.Lt, BinaryOp.Le, BinaryOp.Gt, BinaryOp.Ge -> {
                buildCmp(node, lv, rv, isFloat)
            }

            BinaryOp.BitAnd -> builder.buildAnd(lv, rv, "and")
            BinaryOp.BitOr -> builder.buildOr(lv, rv, "or")
            BinaryOp.BitXor -> builder.buildXor(lv, rv, "xor")
            BinaryOp.Shl -> builder.buildShl(lv, rv, "shl")
            BinaryOp.Shr -> builder.buildAShr(lv, rv, "shr")
            BinaryOp.AndAnd, BinaryOp.OrOr -> error("Unreachable")
        }
    }

    private fun getZero(type: LLVMTypeRef) =
        when {
            type.isFloatLike -> type.constReal(0.0)
            else -> type.constInt(0u)
        }

    private fun buildCmp(node: Binary, lv: LLVMValueRef, rv: LLVMValueRef, isFloat: Boolean): LLVMValueRef {
        val cmp = if (isFloat) {
            val pred = when (node.op) {
                BinaryOp.Eq -> LLVMRealPredicate.LLVMRealOEQ
                BinaryOp.Ne -> LLVMRealPredicate.LLVMRealONE
                BinaryOp.Lt -> LLVMRealPredicate.LLVMRealOLT
                BinaryOp.Le -> LLVMRealPredicate.LLVMRealOLE
                BinaryOp.Gt -> LLVMRealPredicate.LLVMRealOGT
                BinaryOp.Ge -> LLVMRealPredicate.LLVMRealOGE
                else -> error("Unreachable")
            }
            builder.buildFCmp(pred, lv, rv)
        } else {
            val pred = when (node.op) {
                BinaryOp.Eq -> LLVMIntEQ
                BinaryOp.Ne -> LLVMIntNE
                BinaryOp.Lt -> LLVMIntSLT
                BinaryOp.Le -> LLVMIntSLE
                BinaryOp.Gt -> LLVMIntSGT
                BinaryOp.Ge -> LLVMIntSGE
                else -> error("Unreachable")
            }
            builder.buildICmp(pred, lv, rv)
        }
        return builder.buildZExt(cmp, boolType)
    }

    private fun visitLogical(node: Binary): LLVMValueRef {
        val lv = node.left.accept(this)!!
        val lhsBool = if (lv.type.isFloatLike) {
            builder.buildFCmp(LLVMRealPredicate.LLVMRealONE, lhs = lv, rhs = getZero(lv.type), name = "to_bool")
        } else {
            builder.buildICmp(LLVMIntNE, lhs = lv, rhs = getZero(lv.type), name = "to_bool")
        }

        val lhsBlock = builder.insertBlock!!
        val rhsBlock = addBasicBlock("logic.rhs")
        val mergeBlock = addBasicBlock("logic.end")

        if (node.op == BinaryOp.AndAnd) {
            builder.buildCondBr(lhsBool, rhsBlock, mergeBlock)
        } else {
            builder.buildCondBr(lhsBool, mergeBlock, rhsBlock)
        }

        builder.positionAtEnd(rhsBlock)
        val rv = node.right.accept(this)!!
        val rhsBool = if (rv.type.isFloatLike) {
            builder.buildFCmp(LLVMRealPredicate.LLVMRealONE, rv, getZero(rv.type), "to_bool")
        } else {
            builder.buildICmp(LLVMIntNE, rv, getZero(rv.type), "to_bool")
        }
        val rhsRes = builder.buildZExt(rhsBool, boolType)
        val rhsEndBlock = builder.insertBlock!!
        builder.buildBr(mergeBlock)

        builder.positionAtEnd(mergeBlock)
        val phi = builder.buildPhi(i32Type, "logic.res")
        val shortCircuitVal = if (node.op == BinaryOp.AndAnd) 0 else 1
        phi.addIncoming(
            values = listOf(shortCircuitVal.i32(), rhsRes),
            blocks = listOf(lhsBlock, rhsEndBlock)
        )
        return phi
    }

    override fun visit(node: Call): LLVMValueRef {
        val fn = functions[node.callee.name]!!
        val args = node.args.map { it.accept(this)!! }
        val name = if (fn.type.returnType.isVoid) "" else "call"
        return builder.buildCall(fn.type, fn.value, args, name)
    }

    override fun visit(node: Index): LLVMValueRef {
        val symbol = node.toLValue()
        return load(symbol.value, symbol.type, name = "index")
    }

    private fun Expr.toLValue(): Symbol = when (this) {
        is Ident -> findSymbol(name)

        is Index -> {
            val (arrPtr, arrType) = target.toLValue()
            val idxVal = index.accept(this@Codegen)!!
            require(idxVal.type == i32Type) { "Index is not i32 type" }

            when {
                arrType.isArray -> {
                    val gep = builder.buildInBoundsGEP(arrType, arrPtr, indices = listOf(zero, idxVal), name = "idx")
                    Symbol(gep, arrType.elementType)
                }

                arrType.isPointer -> {
                    val gep = builder.buildInBoundsGEP(arrType, arrPtr, indices = listOf(idxVal), name = "idx")
                    Symbol(gep, nothingType)
                }

                else -> error("Can't get element of non-array type.")
            }
        }

        is Unary -> when (op) {
            UnaryOp.Deref -> {
                val (ptrVal, ptrType) = expr.toLValue()
                require(ptrType.isPointer) { "Attempt to dereference non-pointer type" }
                Symbol(ptrVal, nothingType)
            }

            else -> error("Expression is not an lvalue: ${expr.prettyPrint()}")
        }

        is Member -> {
            // Resolve base pointer and value type in memory
            val (basePtr, baseValTy) = target.toLValue()

            // Expect a struct value in memory at basePtr
            val structName = baseValTy.structName
            require(structName != null) {
                "Member access base is not a struct"
            }
            val fields = getStructFields(structName)
            val fieldIndex = fields.indexOfFirst { it.name == name }
            require(fieldIndex != -1) {
                "Unknown field $name in struct $structName"
            }

            val indices = listOf(zero, fieldIndex.i32())
            val fieldPtr = builder.buildInBoundsGEP(baseValTy, basePtr, indices, "field_ptr")
            val fieldTy = baseValTy.getFieldTypeAt(fieldIndex)
            Symbol(fieldPtr, fieldTy)
        }

        else -> error("Unknown lvalue expression: ${prettyPrint()}")
    }

    override fun visit(node: Member): LLVMValueRef {
        val (fieldPtr, fieldType) = node.toLValue()
        return builder.buildLoad(fieldType, fieldPtr, name = "field")
    }

    override fun visit(node: Assign): LLVMValueRef {
        val (lValue, lType) = node.target.toLValue()
        val rValue = node.value.accept(this)!!
        require(rValue.type == lType || lType == nothingType) {
            "LHS and RHS types don't match"
        }
        builder.buildStore(rValue, lValue)
        return rValue
    }

    override fun visit(node: BlockExpr) = withScope {
        node.block.items.forEachRetLast { it.accept(this) }
    }

    override fun visit(node: IfExpr): LLVMValueRef? {
        // condition
        val condVal = node.cond.accept(this)!!
        val thenBB = addBasicBlock("then")
        val elseBB = addBasicBlock("else")
        val contBB = addBasicBlock("endif")

        // convert to i1 for branch
        val condBool = if (condVal.type.isFloatLike) {
            builder.buildFCmp(LLVMRealPredicate.LLVMRealONE, lhs = condVal, rhs = getZero(condVal.type), name = "cond")
        } else {
            builder.buildICmp(LLVMIntNE, lhs = condVal, rhs = getZero(boolType), name = "cond")
        }
        builder.buildCondBr(condBool, thenBB, elseBB)

        // then
        builder.positionAtEnd(thenBB)
        val thenVal = node.thenBlock.accept(this)
        val thenEndBB = builder.insertBlock
        val thenReachesCont = if (thenEndBB != null && thenEndBB.terminator == null) {
            builder.buildBr(contBB)
            true
        } else false

        // else
        builder.positionAtEnd(elseBB)
        val elseVal = node.elseBlock.accept(this)
        val elseEndBB = builder.insertBlock
        val elseReachesCont = if (elseEndBB != null && elseEndBB.terminator == null) {
            builder.buildBr(contBB)
            true
        } else false

        builder.positionAtEnd(contBB)

        val incomingVals = mutableListOf<LLVMValueRef>()
        val incomingBlocks = mutableListOf<LLVMBasicBlockRef>()

        if (thenReachesCont && thenVal != null && thenEndBB != null) {
            incomingVals.add(thenVal)
            incomingBlocks.add(thenEndBB)
        }
        if (elseReachesCont && elseVal != null && elseEndBB != null) {
            incomingVals.add(elseVal)
            incomingBlocks.add(elseEndBB)
        }

        if (incomingVals.isNotEmpty()) {
            val firstType = incomingVals.first().type
            if (!firstType.isVoid && incomingVals.all { it.type == firstType }) {
                if (incomingVals.size == 1) {
                    return incomingVals.first()
                }
                val phi = builder.buildPhi(firstType, "if")
                phi.addIncoming(incomingVals, incomingBlocks)
                return phi
            }
        }

        return null
    }

    override fun visit(node: WhileExpr): LLVMValueRef? {
        val condBB = addBasicBlock("while.cond")
        val bodyBB = addBasicBlock("while.body")
        val contBB = addBasicBlock("while.end")

        builder.buildBr(condBB)
        builder.positionAtEnd(condBB)

        val condVal = node.cond.accept(this)!!
        builder.buildCondBr(condVal, bodyBB, contBB)
        builder.positionAtEnd(bodyBB)
        node.body.accept(this)
        if (bodyBB.terminator == null) {
            builder.buildBr(condBB)
        }
        builder.positionAtEnd(contBB)
        return null
    }

    override fun visit(node: DoWhileExpr): LLVMValueRef? {
        val bodyBB = addBasicBlock("do.body")
        val condBB = addBasicBlock("do.cond")
        val contBB = addBasicBlock("do.end")
        builder.buildBr(bodyBB)
        builder.positionAtEnd(bodyBB)
        node.body.accept(this)
        if (bodyBB.terminator == null) builder.buildBr(condBB)
        builder.positionAtEnd(condBB)
        val condVal = node.cond.accept(this)!!
        builder.buildCondBr(condVal, bodyBB, contBB)
        builder.positionAtEnd(contBB)
        return null
    }

    override fun visit(node: ForExpr): LLVMValueRef? {
        // for(init; cond; incr) body
        node.init.accept(this)

        val condBB = addBasicBlock("for.cond")
        val bodyBB = addBasicBlock("for.body")
        val incrBB = addBasicBlock("for.incr")
        val contBB = addBasicBlock("for.end")

        // Save and set loop targets
        val prevIncr = loopIncrementBlock
        val prevExit = loopExitBlock
        loopIncrementBlock = incrBB
        loopExitBlock = contBB

        builder.buildBr(condBB)
        builder.positionAtEnd(condBB)
        val condVal = node.cond.accept(this)!!
        builder.buildCondBr(condVal, bodyBB, contBB)
        builder.positionAtEnd(bodyBB)
        node.body.accept(this)
        val bodyEndBB = builder.insertBlock
        if (bodyEndBB != null && bodyEndBB.terminator == null) {
            builder.buildBr(incrBB)
        }
        builder.positionAtEnd(incrBB)
        node.incr.accept(this)
        if (incrBB.terminator == null) builder.buildBr(condBB)
        builder.positionAtEnd(contBB)

        // Restore previous loop targets
        loopIncrementBlock = prevIncr
        loopExitBlock = prevExit
        return null
    }

    override fun visit(node: SwitchExpr): LLVMValueRef? {
        val scrutinee = node.expr.accept(this)!!
        val contBB = addBasicBlock("switch.end")
        val defaultBB = addBasicBlock("switch.default")
        val sw = builder.buildSwitch(scrutinee, defaultBB, node.cases.size.toUInt())
        val caseVals = mutableListOf<LLVMBasicBlockRef>()
        val resultVals = mutableListOf<LLVMValueRef?>()
        node.cases.forEach { c ->
            val bb = addBasicBlock("case")
            sw.addCase(scrutinee.type.constInt(c.value.toULong()), bb)
            builder.positionAtEnd(bb)
            resultVals += c.result.accept(this)
            val caseEndBB = builder.insertBlock
            if (caseEndBB != null && caseEndBB.terminator == null) builder.buildBr(contBB)
            if (caseEndBB != null) caseVals += caseEndBB
        }
        // default
        builder.positionAtEnd(defaultBB)

        val defaultVal = node.defaultCase?.accept(this)
        val defaultEndBB = builder.insertBlock
        if (defaultEndBB != null && defaultEndBB.terminator == null) builder.buildBr(contBB)

        builder.positionAtEnd(contBB)
        // If all cases yield the same type, create phi
        val incomingVals = mutableListOf<LLVMValueRef>()
        val incomingBBs = mutableListOf<LLVMBasicBlockRef>()
        resultVals.forEachIndexed { i, v ->
            if (v != null && !v.type.isVoid) {
                incomingVals += v
                incomingBBs += caseVals[i]
            }
        }
        if (defaultVal != null && !defaultVal.type.isVoid && defaultEndBB != null) {
            incomingVals += defaultVal
            incomingBBs += defaultEndBB
        }
        if (incomingVals.isNotEmpty()) {
            val phiTy = incomingVals.first().type
            if (incomingVals.all { it.type == phiTy }) {
                val phi = builder.buildPhi(phiTy, "swt")
                phi.addIncoming(incomingVals, incomingBBs)
                return phi
            }
        }
        return null
    }

    override fun visit(node: SwitchCase) = null

    override fun visit(node: Cast) = null

    private fun buildIncDec(target: Expr, isIncrement: Boolean, isPrefix: Boolean): LLVMValueRef {
        val (ptr, type) = target.toLValue()
        val isFloat = type.isFloatLike
        val cur = load(value = ptr, type, name = "tmp")
        val one = if (isFloat) 1.0.f64() else 1.i32()
        val newVal = if (isIncrement) {
            when (isFloat) {
                true -> builder.buildFAdd(cur, one, "inc")
                else -> builder.buildAdd(cur, one, "inc")
            }
        } else {
            when (isFloat) {
                true -> builder.buildFSub(cur, one, "dec")
                else -> builder.buildSub(cur, one, "dec")
            }
        }
        builder.buildStore(newVal, ptr)
        return if (isPrefix) newVal else cur
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
            return module.printToString()
        }
    }
}

