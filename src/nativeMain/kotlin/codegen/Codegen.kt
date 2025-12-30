package codegen

import ast.*
import ast.visitor.AstVisitor
import ast.visitor.accept
import kotlinx.cinterop.ExperimentalForeignApi
import llvm.LLVMBasicBlockRef
import llvm.LLVMRealPredicate
import llvm.LLVMTypeRef
import llvm.LLVMValueRef
import type.KodeType
import type.isFloatLike

@OptIn(ExperimentalForeignApi::class)
class Codegen : CodegenContext(), AstVisitor<LLVMValueRef?> {

    override fun visit(node: Program): LLVMValueRef? {
        // First pass: declare opaque structs
        for (it in node.declarations) {
            when (it) {
                is ObjectDecl -> {
                    structs[it.name] = context.createNamedStruct(it.name)
                }

                is ObjectDef -> {
                    val structType = structs[it.name] ?: context.createNamedStruct(it.name)
                    registerStruct(it.name, structType, it.fields)

                    val fieldTypes = it.kodeType!!.fields.map { (_, fieldType) -> fieldType.toLLVM() }
                    structType.setBody(fieldTypes)
                }

                else -> continue
            }
        }

        // Declare prototypes for functions and globals
        for (it in node.declarations) {
            when (it) {
                is FunDecl -> ensureFunction(it.name, it.params, it.kodeType!!, alien = false)
                is FunDef -> ensureFunction(it.name, it.params, it.kodeType!!, alien = false)
                is AlienFunDecl -> ensureFunction(it.name, it.params, it.kodeType!!, alien = true)
                is GlobalVarDecl -> it.declarators.forEach { d ->
                    addGlobal(d.name, d.kodeType!!)
                }

                else -> continue
            }
        }

        // generate bodies
        node.declarations.forEach { it.accept(this) }
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

            val kodeType = node.kodeType!!
            val kodeParamTypes = kodeType.params
            node.params.forEachIndexed { i, p ->
                val paramType = kodeParamTypes[i]
                val alloca = buildAlloca(p.name, paramType.toLLVM())
                builder.buildStore(params[i], alloca)
                putLocal(p.name, alloca, paramType)
            }

            val bodyVal = node.body.accept(this)

            if (kodeType.ret == KodeType.Void) {
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
        node.declarators.forEach { d ->
            val gv = globals[d.name]!!.value
            when (d.init) {
                is WithInit -> {
                    val kodeType = d.kodeType!!
                    require(kodeType is KodeType.Arr) {
                        "Incorrect `with` keyword usage"
                    }
                    val initVal = d.init.expr.accept(this)!!
                    val totalSize = d.arrayDims.fold(1) { acc, dim -> dim.value * acc }
                    val elements = List(totalSize) { initVal }
                    val constArray = kodeType.base.toLLVM().constArray(elements)
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
        node.declarators.forEach { d ->
            val kodeType = d.kodeType!!
            val llvmType = kodeType.toLLVM()
            val alloca = buildAlloca(d.name, llvmType)
            putLocal(d.name, alloca, kodeType)

            when (d.init) {
                is WithInit -> {
                    require(kodeType is KodeType.Arr)

                    val initVal = d.init.expr.accept(this)!!
                    for (i in 0 until kodeType.totalSize()) {
                        val indices = listOf(zero, i.i32())
                        val elemPtr = builder.buildInBoundsGEP(llvmType, alloca, indices, name = "int_ptr")
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
        val symbol = getLocal(node.name) ?: globals[node.name]
        if (symbol != null) {
            return load(symbol.value, symbol.type.toLLVM(), node.name)
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
                builder.buildZExt(cond, i32Type)
            }

            UnaryOp.Deref -> {
                val symbol = node.toLValue()
                load(symbol.value, symbol.type.toLLVM(), name = "deref")
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

    private fun getZero(type: LLVMTypeRef) = when {
        type.isFloatLike -> type.constReal(0.0)
        else -> type.constInt(0u)
    }

    private fun LLVMValueRef.toBool(): LLVMValueRef {
        if (type == boolType) return this
        return if (type.isFloatLike) {
            builder.buildFCmp(LLVMRealPredicate.LLVMRealONE, lhs = this, rhs = getZero(this.type), name = "to_bool")
        } else {
            builder.buildICmp(LLVMIntNE, lhs = this, rhs = getZero(this.type), name = "to_bool")
        }
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
        return builder.buildZExt(cmp, i32Type)
    }

    private fun visitLogical(node: Binary): LLVMValueRef {
        val lv = node.left.accept(this)!!
        val lhsBool = lv.toBool()

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
        val rhsBool = rv.toBool()
        val rhsRes = builder.buildZExt(rhsBool, i32Type)
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
        val calleeValue = node.callee.accept(this)!!
        val symbol = functions[node.callee.name] ?: findSymbol(node.callee.name)
        val kodeType = symbol.type as KodeType.Fn
        val args = node.args.map { it.accept(this)!! }
        val name = if (kodeType.ret is KodeType.Void) "" else "call"
        return builder.buildCall(kodeType.toLLVMSignature(), calleeValue, args, name)
    }

    override fun visit(node: Index): LLVMValueRef {
        val symbol = node.toLValue()
        return load(symbol.value, symbol.type.toLLVM(), name = "index")
    }

    private fun Expr.toLValue(): Symbol = when (this) {
        is Ident -> findSymbol(name)

        is Index -> {
            val symbol = target.toLValue()
            val arrPtr = symbol.value

            val idxVal = index.accept(this@Codegen)!!
            require(idxVal.type == i32Type) { "Index is not i32 type" }

            when (symbol.type) {
                is KodeType.Arr -> {
                    val elementType = symbol.type.indexed()
                    val arrayType = symbol.type.toLLVM()
                    val gep = builder.buildInBoundsGEP(arrayType, arrPtr, indices = listOf(zero, idxVal), name = "idx")
                    Symbol(gep, elementType)
                }

                is KodeType.Ptr -> {
                    val elementType = symbol.type.referenced()
                    val actualPtr = builder.buildLoad(symbol.type.toLLVM(), arrPtr, "ptr_load")
                    val llvmType = elementType.toLLVM()
                    val gep = builder.buildInBoundsGEP(llvmType, actualPtr, indices = listOf(idxVal), name = "idx")
                    Symbol(gep, elementType)
                }

                else -> error("Can't get element of non-array type.")
            }
        }

        is Unary -> when (op) {
            UnaryOp.Deref -> {
                val symbol = expr.toLValue()
                require(symbol.type is KodeType.Ptr) { "Attempt to dereference non-pointer type" }
                val actualPtr = builder.buildLoad(type = symbol.type.toLLVM(), ptr = symbol.value, name = "deref_ptr")
                Symbol(actualPtr, symbol.type.referenced())
            }

            else -> error("Expression is not an lvalue: ${expr.prettyPrint()}")
        }

        is Member -> {
            val symbol = target.toLValue()
            val (objectPtr, objectType) = if (viaArrow) {
                require(symbol.type is KodeType.Ptr) { "Arrow operator requires pointer type" }
                require(symbol.type.base is KodeType.Obj) { "Arrow operator requires pointer to struct" }
                val ptrVal = builder.buildLoad(symbol.type.toLLVM(), symbol.value, "ptr_load")
                Pair(ptrVal, symbol.type.base)
            } else {
                require(symbol.type is KodeType.Obj) { "Arrow operator requires pointer type" }
                Pair(symbol.value, symbol.type)
            }

            val objectLLVMType = objectType.toLLVM()
            val fields = getStructFields(objectType.name)
            val fieldIndex = fields.indexOfFirst { it.name == name }
            require(fieldIndex != -1) { "Member not found: ${objectType.name}.$name" }

            val indices = listOf(zero, fieldIndex.i32())
            val fieldPtr = builder.buildInBoundsGEP(objectLLVMType, objectPtr, indices, name = "field_ptr")
            val fieldKodeType = objectType.fields[fieldIndex].second
            Symbol(fieldPtr, fieldKodeType)
        }

        else -> error("Unknown lvalue expression: ${prettyPrint()}")
    }

    override fun visit(node: Member): LLVMValueRef {
        val (fieldPtr, fieldType) = node.toLValue()
        return builder.buildLoad(fieldType.toLLVM(), fieldPtr, name = "field")
    }

    override fun visit(node: Assign): LLVMValueRef {
        val symbol = node.target.toLValue()
        val rValue = node.value.accept(this)!!
        builder.buildStore(rValue, ptr = symbol.value)
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
        builder.buildCondBr(condVal.toBool(), thenBB, elseBB)

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
        builder.buildCondBr(condVal.toBool(), bodyBB, contBB)
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
        builder.buildCondBr(condVal.toBool(), bodyBB, contBB)
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
        builder.buildCondBr(condVal.toBool(), bodyBB, contBB)
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

    override fun visit(node: Cast): LLVMValueRef {
        val value = node.expr.accept(this)!!
        val srcLLVMType = value.type
        val destKodeType = node.kodeType ?: error("Cast type not resolved at ${node.span}")
        val destLLVMType = destKodeType.toLLVM()

        if (srcLLVMType == destLLVMType) return value

        val srcIsFloat = srcLLVMType.isFloatLike
        val destIsFloat = destLLVMType.isFloatLike
        val srcIsPtr = srcLLVMType.isPointer
        val destIsPtr = destLLVMType.isPointer

        return when {
            srcIsFloat && !destIsFloat -> builder.buildFPToSI(value, destLLVMType)
            !srcIsFloat && destIsFloat -> builder.buildSIToFP(value, destLLVMType)
            srcIsPtr && destIsPtr -> builder.buildPointerCast(value, destLLVMType)
            !srcIsFloat && !srcIsPtr && destIsPtr -> builder.buildIntToPtr(value, destLLVMType)
            srcIsPtr && !destIsFloat && !destIsPtr -> builder.buildPtrToInt(value, destLLVMType)
            !srcIsFloat && !srcIsPtr && !destIsFloat && !destIsPtr -> {
                val srcWidth = srcLLVMType.getIntTypeWidth()
                val destWidth = destLLVMType.getIntTypeWidth()
                when {
                    srcWidth > destWidth -> builder.buildTrunc(value, destLLVMType)
                    srcWidth < destWidth -> builder.buildSExt(value, destLLVMType)
                    else -> builder.buildBitCast(value, destLLVMType)
                }
            }

            else -> builder.buildBitCast(value, destLLVMType)
        }
    }

    private fun buildIncDec(target: Expr, isIncrement: Boolean, isPrefix: Boolean): LLVMValueRef {
        val (ptr, type) = target.toLValue()
        val isFloat = type.isFloatLike
        val cur = load(value = ptr, type.toLLVM(), name = "tmp")
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

    override fun visit(node: StructInit): LLVMValueRef {
        val structType = getStructType(node.typeName)
            ?: error("Unknown struct type: ${node.typeName}")

        // Allocate struct on stack
        val structPtr = builder.buildAlloca(structType, "struct")

        // Get struct fields info
        val fields = getStructFields(node.typeName)

        // Initialize fields (named only)
        for (fieldInit in node.fieldInits) {
            val fieldName = fieldInit.name
            val fieldIndex = fields.indexOfFirst { it.name == fieldName }
            if (fieldIndex == -1) {
                error("Field $fieldName not found in ${node.typeName}")
            }

            val indices = listOf(zero, fieldIndex.i32())
            val fieldPtr = builder.buildInBoundsGEP(structType, structPtr, indices, "field_${fieldName}")
            val value = fieldInit.value.accept(this)!!
            builder.buildStore(value, fieldPtr)
        }

        return load(structPtr, structType, "struct_val")
    }

    override fun visit(node: FieldInit): LLVMValueRef? {
        return node.value.accept(this)
    }

    companion object {
        fun generate(ast: Program): String = with(Codegen()) {
            visit(ast)
            return module.printToString()
        }
    }
}

