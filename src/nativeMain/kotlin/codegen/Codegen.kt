@file:OptIn(ExperimentalForeignApi::class)

package codegen

import ast.*
import ast.visitor.AstVisitor
import ast.visitor.accept
import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.cValuesOf
import kotlinx.cinterop.toKString
import llvm.*

class Codegen : CodegenContext(), AstVisitor<LLVMValueRef?> {

    override fun visit(node: Program): LLVMValueRef? {
        // First pass: declare opaque structs
        for (it in node.decls) {
            when (it) {
                is ObjectDecl -> {
                    val structType = LLVMStructCreateNamed(context, it.name)
                    structs[it.name] = structType
                }

                is ObjectDef -> {
                    val structType = structs[it.name] ?: LLVMStructCreateNamed(context, it.name)
                    structs[it.name] = structType
                    structFields[it.name] = it.fields

                    val fieldTypes = it.fields.map { field ->
                        field.type.toLLVM()
                    }.toTypedArray()
                    val types = cValuesOf(*fieldTypes)
                    LLVMStructSetBody(structType, types, fieldTypes.size.toUInt(), 0)
                }

                else -> continue
            }
        }

        // Declare prototypes for functions and globals
        for (it in node.decls) {
            when (it) {
                is FunDecl -> ensureFunction(it.name, it.type.toLLVM())
                is FunDef -> ensureFunction(it.name, it.type.toLLVM())
                is AlienFunDecl -> ensureFunction(it.name, it.type.toLLVM())
                is GlobalVarDecl -> {
                    val baseType = it.type.toLLVM()
                    it.declarators.forEach { d ->
                        val ty = baseType.withDimensions(d.arrayDims)
                        globals[d.name] = LLVMAddGlobal(M = module, Ty = ty, Name = d.name)
                    }
                }

                else -> continue
            }
        }

        // generate bodies
        node.decls.forEach { it.accept(this) }
        return null
    }

    override fun visit(node: FunDecl) = null

    override fun visit(node: FunDef): LLVMValueRef? {
        val fn = functions[node.name] ?: error("Function ${node.name} not found")
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
            node.params.forEachIndexed { i, p ->
                val alloca = buildAlloca(p.type.toLLVM(), p.name)
                LLVMBuildStore(builder, params[i], alloca)
                putLocal(p.name, alloca)
            }

            node.body.accept(this)

            val retTy = node.returnType.toLLVM()
            // If function not terminated, emit return
            val currentBlock = LLVMGetInsertBlock(builder)
            val hasTerminator = LLVMGetBasicBlockTerminator(currentBlock) != null
            if (!hasTerminator) {
                require(isVoid(retTy))
                LLVMBuildRetVoid(builder)
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
            val gv = globals[d.name]!!
            when (d.init) {
                is WithInit -> {
                    require(!d.arrayDims.isEmpty()) {
                        "Incorrect `with` keyword usage"
                    }
                    val initVal = d.init.expr.accept(this)!!
                    val totalSize = d.totalArraySize
                    val elements = Array(totalSize) { initVal }
                    val constArray = LLVMConstArray(
                        ElementTy = baseType,
                        ConstantVals = cValuesOf(*elements),
                        Length = totalSize.toUInt()
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
            val alloca = buildAlloca(allocaType, d.name)
            putLocal(d.name, alloca)

            when (d.init) {
                is WithInit -> {
                    require(!d.arrayDims.isEmpty())

                    val initVal = d.init.expr.accept(this)!!
                    for (i in 0 until d.totalArraySize) {
                        val idx = constI32(i.toULong())
                        val indices = cValuesOf(zero, idx)
                        val elemPtr = LLVMBuildGEP2(builder, allocaType, alloca, indices, 2u, "init_ptr")
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
        val ty = LLVMInt32TypeInContext(context)
        return LLVMConstInt(IntTy = ty, node.value.toULong(), SignExtend = 1)!!
    }

    override fun visit(node: FloatLit): LLVMValueRef {
        val ty = LLVMDoubleTypeInContext(context)
        return LLVMConstReal(RealTy = ty, N = node.value)!!
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
        val local = getLocal(node.name)
        if (local != null) {
            val ty = LLVMTypeOf(local)
            val loadedTy = if (isPointer(ty)) LLVMGetElementType(ty) else ty
            return LLVMBuildLoad2(builder, loadedTy, PointerVal = local, node.name)!!
        }
        val glob = globals[node.name]
        if (glob != null) {
            val elemTy = LLVMGetElementType(LLVMTypeOf(glob))
            return LLVMBuildLoad2(builder, elemTy, PointerVal = glob, node.name)!!
        }
        return functions[node.name]!!
    }

    override fun visit(node: Unary): LLVMValueRef? {
        val v = node.expr.accept(this)
        return when (node.op) {
            UnaryOp.Plus -> v
            UnaryOp.Minus -> {
                if (isFloatLike(LLVMTypeOf(v))) {
                    LLVMBuildFNeg(builder, v, "neg")
                } else {
                    LLVMBuildNeg(builder, v, "neg")
                }
            }

            UnaryOp.BitNot -> LLVMBuildNot(builder, v, "not")!!
            UnaryOp.Not -> {
                // not supported precisely; return 0 (false)
                LLVMConstInt(LLVMInt1TypeInContext(context), 0u, 0)
            }

            UnaryOp.Deref -> {
                val elemTy = LLVMGetElementType(LLVMTypeOf(v))
                LLVMBuildLoad2(builder, elemTy, v, "deref")
            }

            UnaryOp.AddressOf -> {
                // assume operand was lvalue alloca; if not, no-op
                null
            }

            UnaryOp.PreInc -> {
                val one = LLVMConstInt(LLVMTypeOf(v), 1u, 0)
                LLVMBuildAdd(builder, v, one, "inc")
            }

            UnaryOp.PreDec -> {
                val one = LLVMConstInt(LLVMTypeOf(v), 1u, 0)
                LLVMBuildSub(builder, v, one, "dec")
            }
        }
    }

    override fun visit(node: Binary): LLVMValueRef? {
        val lv = node.left.accept(this)
        val rv = node.right.accept(this)
        val isFloat = isFloatLike(LLVMTypeOf(lv))

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
        val fnTy = LLVMTypeOf(fn)
        val argVals = node.args.map { it.accept(this) }.toTypedArray()
        val args = cValuesOf(*argVals)
        return LLVMBuildCall2(builder, fnTy, fn, args, argVals.size.toUInt(), "call")!!
    }

    override fun visit(node: Index): LLVMValueRef? {
        // Handle array indexing: target[index]
        val targetIdent = node.target as? Ident ?: return null
        val arrayAlloca = findAlloca(targetIdent.name)
        val indexVal = node.index.accept(this) ?: return null

        // Get pointer to array element using GEP
        val arrayType = LLVMGetElementType(LLVMTypeOf(arrayAlloca))
        val zero = LLVMConstInt(LLVMInt32TypeInContext(context), 0u, 0)
        val indices = cValuesOf(zero, indexVal)

        val elementPtr = LLVMBuildGEP2(
            builder,
            arrayType,
            arrayAlloca,
            indices,
            2u,
            "arrayptr"
        )

        // Load the value at the computed address
        val elementType = LLVMGetElementType(arrayType)
        return LLVMBuildLoad2(builder, elementType, elementPtr, "arrayelem")
    }

    private fun getMemberPtr(node: Member): Pair<LLVMValueRef, LLVMTypeRef>? {
        // Recursively resolve the target to get the struct pointer
        val (structPtr, structType) = when (val target = node.target) {
            is Ident -> {
                val alloca = findAlloca(target.name)
                val ty = LLVMGetElementType(LLVMTypeOf(alloca))
                Pair(alloca, ty)
            }

            is Member -> {
                getMemberPtr(target) ?: return null
            }

            else -> return null
        }

        // Find the struct type name
        val structTypeName = LLVMGetStructName(structType)?.toKString() ?: return null
        val fields = getStructFields(structTypeName) ?: return null
        val fieldIndex = fields.indexOfFirst { it.name == node.name }
        if (fieldIndex == -1) return null

        // Build GEP to access the field
        val indices = cValuesOf(zero, constI32(fieldIndex.toULong()))
        val fieldPtr = LLVMBuildGEP2(builder, structType, structPtr, indices, 2u, "field_ptr")

        // Get the field type
        val fieldType = LLVMStructGetTypeAtIndex(structType, fieldIndex.toUInt())
        return Pair(fieldPtr!!, fieldType!!)
    }

    override fun visit(node: Member): LLVMValueRef? {
        val (fieldPtr, fieldType) = getMemberPtr(node) ?: return null
        // Load the field value
        return LLVMBuildLoad2(builder, fieldType, fieldPtr, "field")
    }

    override fun visit(node: Assign): LLVMValueRef? {
        val v = node.value.accept(this)

        when (val target = node.target) {
            is Ident -> {
                // Simple variable assignment
                val alloca = findAlloca(target.name)
                LLVMBuildStore(builder, v, alloca)
            }

            is Index -> {
                // Array element assignment: arr[idx] = value
                val targetIdent = target.target as? Ident ?: error("Expected Ident in Index target")
                val arrayAlloca = findAlloca(targetIdent.name)
                val indexVal = target.index.accept(this)

                // Get pointer to array element using GEP
                val arrayType = LLVMGetElementType(LLVMTypeOf(arrayAlloca))
                val zero = LLVMConstInt(LLVMInt32TypeInContext(context), 0u, 0)
                val indices = cValuesOf(zero, indexVal)

                val elementPtr = LLVMBuildGEP2(
                    builder,
                    arrayType,
                    arrayAlloca,
                    indices,
                    2u,
                    "arrayptr"
                )

                LLVMBuildStore(builder, v, elementPtr)
            }

            is Member -> {
                // Struct field assignment: obj.field = value
                val (fieldPtr, _) = getMemberPtr(target) ?: error("Cannot resolve member")
                LLVMBuildStore(builder, v, fieldPtr)
            }

            else -> error("Unsupported assignment target")
        }
        return v
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
        if (LLVMGetBasicBlockTerminator(thenBB) == null) LLVMBuildBr(builder, contBB)

        // else
        LLVMPositionBuilderAtEnd(builder, elseBB)
        val elseVal = node.elseBlock.accept(this)
        if (LLVMGetBasicBlockTerminator(elseBB) == null) LLVMBuildBr(builder, contBB)

        LLVMPositionBuilderAtEnd(builder, contBB)

        // If both produce a value and types match, create phi
        if (thenVal != null && elseVal != null && LLVMTypeOf(thenVal) == LLVMTypeOf(elseVal)) {
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
        if (LLVMGetBasicBlockTerminator(bodyBB) == null) {
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
        if (LLVMGetBasicBlockTerminator(bodyBB) == null) LLVMBuildBr(builder, condBB)
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
        if (LLVMGetBasicBlockTerminator(bodyBB) == null) LLVMBuildBr(builder, incrBB)
        LLVMPositionBuilderAtEnd(builder, incrBB)
        node.incr.accept(this)
        if (LLVMGetBasicBlockTerminator(incrBB) == null) LLVMBuildBr(builder, condBB)
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
            LLVMAddCase(sw, LLVMConstInt(LLVMTypeOf(scrutinee), c.value.toULong(), 0), bb)
            LLVMPositionBuilderAtEnd(builder, bb)
            resultVals += c.result.accept(this)
            caseVals += LLVMGetInsertBlock(builder)
            if (LLVMGetBasicBlockTerminator(bb) == null) LLVMBuildBr(builder, contBB)
        }
        // default
        LLVMPositionBuilderAtEnd(builder, defaultBB)

        val defaultVal = node.defaultCase?.accept(this)
        if (LLVMGetBasicBlockTerminator(defaultBB) == null) LLVMBuildBr(builder, contBB)

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
            val phiTy = LLVMTypeOf(incomingVals.first())
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

    override fun visit(node: PostfixInc): LLVMValueRef {
        val alloca = findAlloca(node.target.name)
        val ty = LLVMGetElementType(LLVMTypeOf(alloca))
        val cur = LLVMBuildLoad2(builder, ty, alloca, "tmp")
        val one = LLVMConstInt(ty, 1u, 0)
        val inc = LLVMBuildAdd(builder, cur, one, "inc")
        LLVMBuildStore(builder, inc, alloca)
        return cur!!
    }

    override fun visit(node: PostfixDec): LLVMValueRef {
        val alloca = findAlloca(node.target.name)
        val ty = LLVMGetElementType(LLVMTypeOf(alloca))
        val cur = LLVMBuildLoad2(builder, ty, alloca, "tmp")
        val one = LLVMConstInt(ty, 1u, 0)
        val dec = LLVMBuildSub(builder, cur, one, "dec")
        LLVMBuildStore(builder, dec, alloca)
        return cur!!
    }
}