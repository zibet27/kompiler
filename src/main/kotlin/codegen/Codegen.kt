package codegen

import ast.*
import ast.visitor.AstVisitor
import ast.visitor.accept
import llvm.*
import type.KodeType

class Codegen : CodegenContext(), AstVisitor<IRValue?> {

    override fun visit(node: Program): IRValue? {
        // First pass: declare opaque structs
        for (it in node.declarations) {
            when (it) {
                is ObjectDecl -> {
                    structs[it.name] = IRType.Struct(it.name, emptyList())
                }

                is ObjectDef -> {
                    val structType = structs[it.name] ?: IRType.Struct(it.name, emptyList())
                    registerStruct(it.name, structType, it.fields)

                    val fieldTypes = it.kodeType!!.fields.map { (_, fieldType) -> fieldType.toIR() }
                    // Update struct body
                    val updatedStruct = structType.copy(elementTypes = fieldTypes)
                    structs[it.name] = updatedStruct
                    module.namedStructs.remove(structType)
                    module.namedStructs.add(updatedStruct)
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

    override fun visit(node: FunDef): IRValue? {
        val fn = functions[node.name]?.value as? IRFunction ?: error("Function ${node.name} not found")
        currentFunction = fn

        val entry = addBasicBlock("entry")
        builder.positionAtEnd(entry)

        withScope {
            val kodeType = node.kodeType!!
            val kodeParamTypes = kodeType.params
            node.params.forEachIndexed { i, p ->
                val paramType = kodeParamTypes[i]
                val irParam = fn.parameters[i]
                val alloca = buildAlloca(p.name, paramType.toIR())
                builder.buildStore(irParam, alloca)
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

    override fun visit(node: AlienFunDecl): IRValue? = null
    override fun visit(node: ObjectDecl): IRValue? = null
    override fun visit(node: ObjectDef): IRValue? = null
    override fun visit(node: TypeAlias): IRValue? = null

    override fun visit(node: GlobalVarDecl): IRValue? {
        node.declarators.forEach { d ->
            val symbol = globals[d.name] ?: error("Global ${d.name} not found")
            val gv = symbol.value as IRGlobalVariable
            when (d.init) {
                is WithInit -> {
                    val kodeType = d.kodeType!!
                    require(kodeType is KodeType.Arr)
                    val initVal = d.init.expr.accept(this)!!
                    val totalSize = d.arrayDims.fold(1) { acc, dim -> dim.value * acc }
                    val elements = List(totalSize) { initVal }
                    gv.initializer = IRArrayConstant(elements, kodeType.toIR() as IRType.Array)
                }

                is AssignInit -> {
                    val init = d.init.expr.accept(this)!!
                    gv.initializer = init
                }
            }
        }
        return null
    }

    override fun visit(node: FieldDecl): IRValue? = null
    override fun visit(node: Param): IRValue? = null
    override fun visit(node: Declarator): IRValue? = null
    override fun visit(node: WithInit): IRValue? = null
    override fun visit(node: AssignInit): IRValue? = null
    override fun visit(node: BuiltinType): IRValue? = null
    override fun visit(node: NamedType): IRValue? = null
    override fun visit(node: PointerType): IRValue? = null
    override fun visit(node: FuncType): IRValue? = null

    override fun visit(node: Block): IRValue? = withScope {
        node.items.forEachRetLast { it.accept(this) }
    }

    override fun visit(node: LocalVarDecl): IRValue? {
        node.declarators.forEach { d ->
            val kodeType = d.kodeType!!
            val irType = kodeType.toIR()
            val alloca = buildAlloca(d.name, irType)
            putLocal(d.name, alloca, kodeType)

            when (d.init) {
                is WithInit -> {
                    require(kodeType is KodeType.Arr)
                    val initVal = d.init.expr.accept(this)!!
                    for (i in 0 until kodeType.totalSize()) {
                        val indices = listOf(zero, i.i32())
                        val elemPtr = builder.buildGEP(irType, alloca, indices)
                        builder.buildStore(initVal, elemPtr)
                    }
                }

                is AssignInit -> {
                    val v = d.init.expr.accept(this)!!
                    builder.buildStore(v, alloca)
                }
            }
        }
        return null
    }

    override fun visit(node: ExprStmt) = node.expr.accept(this)

    override fun visit(node: SkipStmt): IRValue? {
        loopIncrementBlock?.let { builder.buildBr(it) }
        return null
    }

    override fun visit(node: StopStmt): IRValue? {
        loopExitBlock?.let { builder.buildBr(it) }
        return null
    }

    override fun visit(node: IntLit): IRValue = node.value.i32()
    override fun visit(node: F64Lit): IRValue = node.value.f64()
    override fun visit(node: CharLit): IRValue = IRIntConstant(node.value.toLong(), i8Type)
    override fun visit(node: StringLit): IRValue = builder.buildGlobalStringPtr(node.value)

    override fun visit(node: Ident): IRValue {
        val symbol = findSymbol(node.name)
        return symbol.value as? IRFunction
            ?: builder.buildLoad(symbol.type.toIR(), symbol.value, node.name)
    }

    override fun visit(node: Unary): IRValue? {
        return when (node.op) {
            UnaryOp.PreInc -> buildIncDec(node.expr, isIncrement = true, isPrefix = true)
            UnaryOp.PreDec -> buildIncDec(node.expr, isIncrement = false, isPrefix = true)
            UnaryOp.AddressOf -> node.expr.toLValue().value
            UnaryOp.Plus -> node.expr.accept(this)
            UnaryOp.Minus -> {
                val v = node.expr.accept(this)!!
                if (v.type.isFloatLike) builder.buildFNeg(v) else builder.buildNeg(v)
            }

            UnaryOp.BitNot -> builder.buildNot(node.expr.accept(this)!!)
            UnaryOp.Not -> {
                val v = node.expr.accept(this)!!
                val zeroVal = getZero(v.type)
                val cond = if (v.type.isFloatLike) {
                    builder.buildFCmp(IRInstruction.FCmp.Predicate.OEQ, v, zeroVal)
                } else {
                    builder.buildICmp(IRInstruction.ICmp.Predicate.EQ, v, zeroVal)
                }
                builder.buildZExt(cond, i32Type)
            }

            UnaryOp.Deref -> {
                val symbol = node.toLValue()
                builder.buildLoad(symbol.type.toIR(), symbol.value)
            }
        }
    }

    override fun visit(node: Binary): IRValue? {
        if (node.op == BinaryOp.AndAnd || node.op == BinaryOp.OrOr) return visitLogical(node)

        val lv = node.left.accept(this)!!
        val rv = node.right.accept(this)!!
        val isFloat = lv.type.isFloatLike

        return when (node.op) {
            BinaryOp.Add -> if (isFloat) builder.buildFAdd(lv, rv) else builder.buildAdd(lv, rv)
            BinaryOp.Sub -> if (isFloat) builder.buildFSub(lv, rv) else builder.buildSub(lv, rv)
            BinaryOp.Mul -> if (isFloat) builder.buildFMul(lv, rv) else builder.buildMul(lv, rv)
            BinaryOp.Div -> if (isFloat) builder.buildFDiv(lv, rv) else builder.buildSDiv(lv, rv)
            BinaryOp.Mod -> builder.buildSRem(lv, rv)
            BinaryOp.Eq, BinaryOp.Ne, BinaryOp.Lt, BinaryOp.Le, BinaryOp.Gt, BinaryOp.Ge -> buildCmp(
                node,
                lv,
                rv,
                isFloat
            )

            BinaryOp.BitAnd -> builder.buildAnd(lv, rv)
            BinaryOp.BitOr -> builder.buildOr(lv, rv)
            BinaryOp.BitXor -> builder.buildXor(lv, rv)
            BinaryOp.Shl -> builder.buildShl(lv, rv)
            BinaryOp.Shr -> builder.buildAShr(lv, rv)
        }
    }

    private fun IRValue.toBool(): IRValue {
        if (type == boolType) return this
        val zeroVal = getZero(type)
        return if (type.isFloatLike) {
            builder.buildFCmp(IRInstruction.FCmp.Predicate.UNE, this, zeroVal)
        } else {
            builder.buildICmp(IRInstruction.ICmp.Predicate.NE, this, zeroVal)
        }
    }

    private fun buildCmp(node: Binary, lv: IRValue, rv: IRValue, isFloat: Boolean): IRValue {
        val cmp = if (isFloat) {
            val pred = when (node.op) {
                BinaryOp.Eq -> IRInstruction.FCmp.Predicate.OEQ
                BinaryOp.Ne -> IRInstruction.FCmp.Predicate.UNE
                BinaryOp.Lt -> IRInstruction.FCmp.Predicate.OLT
                BinaryOp.Le -> IRInstruction.FCmp.Predicate.OLE
                BinaryOp.Gt -> IRInstruction.FCmp.Predicate.OGT
                BinaryOp.Ge -> IRInstruction.FCmp.Predicate.OGE
                else -> error("Unreachable")
            }
            builder.buildFCmp(pred, lv, rv)
        } else {
            val pred = when (node.op) {
                BinaryOp.Eq -> IRInstruction.ICmp.Predicate.EQ
                BinaryOp.Ne -> IRInstruction.ICmp.Predicate.NE
                BinaryOp.Lt -> IRInstruction.ICmp.Predicate.SLT
                BinaryOp.Le -> IRInstruction.ICmp.Predicate.SLE
                BinaryOp.Gt -> IRInstruction.ICmp.Predicate.SGT
                BinaryOp.Ge -> IRInstruction.ICmp.Predicate.SGE
                else -> error("Unreachable")
            }
            builder.buildICmp(pred, lv, rv)
        }
        return builder.buildZExt(cmp, i32Type)
    }

    private fun visitLogical(node: Binary): IRValue {
        val lv = node.left.accept(this)!!
        val lhsBool = lv.toBool()
        val lhsBlock = builder.currentBlock!!
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
        val rhsEndBlock = builder.currentBlock!!
        builder.buildBr(mergeBlock)

        builder.positionAtEnd(mergeBlock)
        val phi = builder.buildPhi(i32Type, "logic.res")
        val shortCircuitVal = if (node.op == BinaryOp.AndAnd) 0 else 1
        phi.addIncoming(shortCircuitVal.i32(), lhsBlock)
        phi.addIncoming(rhsRes, rhsEndBlock)
        return phi
    }

    override fun visit(node: Call): IRValue {
        val calleeValue = node.callee.accept(this)!!
        val args = node.args.map { it.accept(this)!! }
        return builder.buildCall(calleeValue, args)
    }

    override fun visit(node: Index): IRValue {
        val symbol = node.toLValue()
        return builder.buildLoad(symbol.type.toIR(), symbol.value)
    }

    private fun Expr.toLValue(): Symbol = when (this) {
        is Ident -> findSymbol(name)
        is Index -> {
            val symbol = target.toLValue()
            val idxVal = index.accept(this@Codegen)!!
            when (val type = symbol.type) {
                is KodeType.Arr -> {
                    val elementType = type.indexed()
                    val gep = builder.buildGEP(type.toIR(), symbol.value, listOf(zero, idxVal))
                    Symbol(gep, elementType)
                }

                is KodeType.Ptr -> {
                    val elementType = type.referenced()
                    val actualPtr = builder.buildLoad(type.toIR(), symbol.value)
                    val gep = builder.buildGEP(elementType.toIR(), actualPtr, listOf(idxVal))
                    Symbol(gep, elementType)
                }

                else -> error("Can't index non-array")
            }
        }

        is Unary -> {
            require(op == UnaryOp.Deref)
            val symbol = expr.toLValue()
            val ptrType = symbol.type as KodeType.Ptr
            val actualPtr = builder.buildLoad(ptrType.toIR(), symbol.value)
            Symbol(actualPtr, ptrType.referenced())
        }

        is Member -> {
            val symbol = target.toLValue()
            val (objPtr, objType) = if (viaArrow) {
                val ptrType = symbol.type as KodeType.Ptr
                val actualPtr = builder.buildLoad(ptrType.toIR(), symbol.value)
                actualPtr to (ptrType.base as KodeType.Obj)
            } else {
                symbol.value to (symbol.type as KodeType.Obj)
            }
            val fields = getStructFields(objType.name)
            val idx = fields.indexOfFirst { it.name == name }
            val fieldPtr = builder.buildGEP(objType.toIR(), objPtr, listOf(zero, idx.i32()))
            Symbol(fieldPtr, objType.fields[idx].second)
        }

        else -> error("Not an lvalue")
    }

    override fun visit(node: Member): IRValue {
        val symbol = node.toLValue()
        return builder.buildLoad(symbol.type.toIR(), symbol.value)
    }

    override fun visit(node: Assign): IRValue {
        val symbol = node.target.toLValue()
        val rValue = node.value.accept(this)!!
        builder.buildStore(rValue, symbol.value)
        return rValue
    }

    override fun visit(node: BlockExpr) = withScope {
        node.block.items.forEachRetLast { it.accept(this) }
    }

    override fun visit(node: IfExpr): IRValue? {
        val condVal = node.cond.accept(this)!!
        val thenBB = addBasicBlock("then")
        val elseBB = addBasicBlock("else")
        val contBB = addBasicBlock("endif")

        builder.buildCondBr(condVal.toBool(), thenBB, elseBB)

        builder.positionAtEnd(thenBB)
        val thenVal = node.thenBlock.accept(this)
        val thenEndBB = builder.currentBlock!!
        if (thenEndBB.terminator == null) builder.buildBr(contBB)

        builder.positionAtEnd(elseBB)
        val elseVal = node.elseBlock.accept(this)
        val elseEndBB = builder.currentBlock!!
        if (elseEndBB.terminator == null) builder.buildBr(contBB)

        builder.positionAtEnd(contBB)
        if (thenVal != null && elseVal != null && thenVal.type != IRType.Void) {
            val phi = builder.buildPhi(thenVal.type)
            phi.addIncoming(thenVal, thenEndBB)
            phi.addIncoming(elseVal, elseEndBB)
            return phi
        }
        return null
    }

    override fun visit(node: WhileExpr): IRValue? {
        val condBB = addBasicBlock("while.cond")
        val bodyBB = addBasicBlock("while.body")
        val contBB = addBasicBlock("while.end")
        builder.buildBr(condBB)
        builder.positionAtEnd(condBB)
        val condVal = node.cond.accept(this)!!
        builder.buildCondBr(condVal.toBool(), bodyBB, contBB)
        builder.positionAtEnd(bodyBB)
        node.body.accept(this)
        if (builder.currentBlock!!.terminator == null) builder.buildBr(condBB)
        builder.positionAtEnd(contBB)
        return null
    }

    override fun visit(node: DoWhileExpr): IRValue? {
        val bodyBB = addBasicBlock("do.body")
        val condBB = addBasicBlock("do.cond")
        val contBB = addBasicBlock("do.end")
        builder.buildBr(bodyBB)
        builder.positionAtEnd(bodyBB)
        node.body.accept(this)
        if (builder.currentBlock!!.terminator == null) builder.buildBr(condBB)
        builder.positionAtEnd(condBB)
        val condVal = node.cond.accept(this)!!
        builder.buildCondBr(condVal.toBool(), bodyBB, contBB)
        builder.positionAtEnd(contBB)
        return null
    }

    override fun visit(node: ForExpr): IRValue? {
        node.init.accept(this)
        val condBB = addBasicBlock("for.cond")
        val bodyBB = addBasicBlock("for.body")
        val incrBB = addBasicBlock("for.incr")
        val contBB = addBasicBlock("for.end")
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
        if (builder.currentBlock!!.terminator == null) builder.buildBr(incrBB)
        builder.positionAtEnd(incrBB)
        node.incr.accept(this)
        if (builder.currentBlock!!.terminator == null) builder.buildBr(condBB)
        builder.positionAtEnd(contBB)
        loopIncrementBlock = prevIncr
        loopExitBlock = prevExit
        return null
    }

    override fun visit(node: SwitchExpr): IRValue? {
        var currentElseBB: IRBasicBlock? = null
        val contBB = addBasicBlock("switch.end")
        val scrutinee = node.expr.accept(this)!!

        val resultVals = mutableListOf<IRValue?>()
        val caseBBs = mutableListOf<IRBasicBlock>()

        node.cases.forEach { c ->
            val caseBB = addBasicBlock("switch.case")
            val nextElseBB = addBasicBlock("switch.next")

            if (currentElseBB != null) {
                builder.positionAtEnd(currentElseBB)
            } else {
                // First case, we are in the entry block of the switch
            }

            val cond = builder.buildICmp(IRInstruction.ICmp.Predicate.EQ, scrutinee, c.value.i32())
            builder.buildCondBr(cond, caseBB, nextElseBB)

            builder.positionAtEnd(caseBB)
            val res = c.result.accept(this)
            resultVals += res
            val endBB = builder.currentBlock!!
            caseBBs += endBB
            if (endBB.terminator == null) builder.buildBr(contBB)

            currentElseBB = nextElseBB
        }

        var defaultEndBB: IRBasicBlock? = null
        var defaultVal: IRValue? = null
        if (currentElseBB != null) {
            builder.positionAtEnd(currentElseBB)
            defaultVal = node.defaultCase?.accept(this)
            defaultEndBB = builder.currentBlock!!
            if (defaultEndBB.terminator == null) builder.buildBr(contBB)
        }

        builder.positionAtEnd(contBB)

        val incomingVals = mutableListOf<Pair<IRValue, IRBasicBlock>>()
        resultVals.forEachIndexed { i, v ->
            if (v != null && v.type != IRType.Void) {
                incomingVals += v to caseBBs[i]
            }
        }
        if (defaultVal != null && defaultVal.type != IRType.Void && defaultEndBB != null) {
            incomingVals += defaultVal to defaultEndBB
        }

        if (incomingVals.isNotEmpty()) {
            val firstType = incomingVals.first().first.type
            if (incomingVals.all { it.first.type == firstType }) {
                val phi = builder.buildPhi(firstType, "swt")
                incomingVals.forEach { (v, b) -> phi.addIncoming(v, b) }
                return phi
            }
        }

        return null
    }

    override fun visit(node: SwitchCase) = null

    override fun visit(node: Cast): IRValue {
        val value = node.expr.accept(this)!!
        val destType = node.kodeType!!.toIR()
        val srcType = value.type
        if (srcType == destType) return value

        return when {
            srcType is IRType.Int && destType is IRType.Int -> {
                if (srcType.bits > destType.bits) builder.buildTrunc(value, destType)
                else builder.buildSExt(value, destType)
            }

            srcType.isFloatLike && destType is IRType.Int -> builder.buildFPToSI(value, destType)
            srcType is IRType.Int && destType.isFloatLike -> builder.buildSIToFP(value, destType)
            srcType is IRType.Pointer && destType is IRType.Int -> builder.buildPtrToInt(value, destType)
            srcType is IRType.Int && destType is IRType.Pointer -> builder.buildIntToPtr(value, destType)
            else -> builder.buildBitCast(value, destType)
        }
    }

    private fun buildIncDec(target: Expr, isIncrement: Boolean, isPrefix: Boolean): IRValue {
        val symbol = target.toLValue()
        val cur = builder.buildLoad(symbol.type.toIR(), symbol.value)
        val one = if (cur.type.isFloatLike) 1.0.f64() else 1.i32()
        val newVal = if (isIncrement) {
            if (cur.type.isFloatLike) builder.buildFAdd(cur, one) else builder.buildAdd(cur, one)
        } else {
            if (cur.type.isFloatLike) builder.buildFSub(cur, one) else builder.buildSub(cur, one)
        }
        builder.buildStore(newVal, symbol.value)
        return if (isPrefix) newVal else cur
    }

    override fun visit(node: PostfixInc) = buildIncDec(node.target, true, false)
    override fun visit(node: PostfixDec) = buildIncDec(node.target, false, false)

    override fun visit(node: StructInit): IRValue {
        val structType = getStructType(node.typeName)!!
        val structPtr = builder.buildAlloca(structType)
        val fields = getStructFields(node.typeName)
        node.fieldInits.forEach { fieldInit ->
            val idx = fields.indexOfFirst { it.name == fieldInit.name }
            val fieldPtr = builder.buildGEP(structType, structPtr, listOf(zero, idx.i32()))
            builder.buildStore(fieldInit.value.accept(this)!!, fieldPtr)
        }
        return builder.buildLoad(structType, structPtr)
    }

    override fun visit(node: FieldInit) = node.value.accept(this)

    companion object {
        fun generate(ast: Program): String {
            val codegen = Codegen()
            codegen.visit(ast)
            return codegen.module.dump()
        }
    }
}

