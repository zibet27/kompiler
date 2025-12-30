@file:OptIn(ExperimentalForeignApi::class)

package codegen

import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toCValues
import kotlinx.cinterop.toKString
import llvm.*

// ===== Context & Module Management =====

fun createContext(): LLVMContextRef = LLVMContextCreate()!!

fun LLVMContextRef.createBuilder(): LLVMBuilderRef =
    LLVMCreateBuilderInContext(C = this)!!

fun LLVMContextRef.createModule(name: String): LLVMModuleRef =
    LLVMModuleCreateWithNameInContext(ModuleID = name, C = this)!!

fun LLVMBuilderRef.dispose() = LLVMDisposeBuilder(Builder = this)

fun LLVMModuleRef.dispose() = LLVMDisposeModule(M = this)

fun LLVMContextRef.dispose() = LLVMContextDispose(C = this)

fun LLVMModuleRef.printToString(): String {
    val cstr = LLVMPrintModuleToString(M = this)
    try {
        return cstr?.toKString() ?: error("Failed to compile LLVM IR")
    } finally {
        LLVMDisposeMessage(cstr)
    }
}

// ===== Type Creation =====

fun LLVMContextRef.voidType(): LLVMTypeRef = LLVMVoidTypeInContext(this)!!

fun LLVMContextRef.i8Type(): LLVMTypeRef = LLVMInt8TypeInContext(this)!!

fun LLVMContextRef.i1Type(): LLVMTypeRef = LLVMInt1TypeInContext(this)!!
fun LLVMContextRef.i32Type(): LLVMTypeRef = LLVMInt32TypeInContext(this)!!

fun LLVMContextRef.f32Type(): LLVMTypeRef = LLVMFloatTypeInContext(this)!!

fun LLVMContextRef.f64Type(): LLVMTypeRef = LLVMDoubleTypeInContext(this)!!

fun LLVMTypeRef.pointerType(addressSpace: UInt = 0u): LLVMTypeRef =
    LLVMPointerType(ElementType = this, AddressSpace = addressSpace)!!

fun LLVMTypeRef.arrayType(elementCount: UInt): LLVMTypeRef =
    LLVMArrayType(ElementType = this, ElementCount = elementCount)!!

fun functionType(
    returnType: LLVMTypeRef,
    paramTypes: List<LLVMTypeRef>,
    isVarArg: Boolean = false
): LLVMTypeRef = LLVMFunctionType(
    ReturnType = returnType,
    ParamTypes = paramTypes.toCValues(),
    ParamCount = paramTypes.size.toUInt(),
    IsVarArg = if (isVarArg) 1 else 0
)!!

// ===== Struct Types =====

fun LLVMContextRef.createNamedStruct(name: String): LLVMTypeRef =
    LLVMStructCreateNamed(this, name)!!

fun LLVMTypeRef.setBody(fieldTypes: List<LLVMTypeRef>, packed: Boolean = false) =
    LLVMStructSetBody(
        StructTy = this,
        ElementTypes = fieldTypes.toCValues(),
        ElementCount = fieldTypes.size.toUInt(),
        Packed = if (packed) 1 else 0
    )

// ===== Type Queries =====

val LLVMValueRef.type: LLVMTypeRef
    get() = LLVMTypeOf(this)!!

val LLVMTypeRef.kind: LLVMTypeKind
    get() = LLVMGetTypeKind(Ty = this)

// ===== Constants =====

fun LLVMTypeRef.constInt(value: ULong, signExtend: Boolean = false): LLVMValueRef =
    LLVMConstInt(IntTy = this, N = value, SignExtend = if (signExtend) 1 else 0)!!

fun LLVMTypeRef.constReal(value: Double): LLVMValueRef =
    LLVMConstReal(RealTy = this, N = value)!!

fun LLVMTypeRef.constArray(constantVals: List<LLVMValueRef>): LLVMValueRef =
    LLVMConstArray(
        ElementTy = this,
        ConstantVals = constantVals.toCValues(),
        Length = constantVals.size.toUInt()
    )!!

// ===== Function Management =====

fun LLVMModuleRef.addFunction(name: String, functionType: LLVMTypeRef): LLVMValueRef =
    LLVMAddFunction(M = this, name, FunctionTy = functionType)!!

fun LLVMValueRef.getParam(index: UInt): LLVMValueRef =
    LLVMGetParam(this, index)!!

val LLVMValueRef.paramCount: UInt
    get() = LLVMCountParams(this)

var LLVMValueRef.valueName: String
    get() = LLVMGetValueName(this)?.toKString() ?: ""
    set(value) = LLVMSetValueName(this, value)

var LLVMValueRef.linkage: LLVMLinkage
    get() = LLVMGetLinkage(this)
    set(value) = LLVMSetLinkage(this, value)

// ===== Global Variables =====

fun LLVMModuleRef.addGlobal(type: LLVMTypeRef, name: String): LLVMValueRef =
    LLVMAddGlobal(this, type, name)!!

var LLVMValueRef.initializer: LLVMValueRef?
    get() = LLVMGetInitializer(this)
    set(value) = LLVMSetInitializer(GlobalVar = this, ConstantVal = value)

// ===== Basic Blocks =====

fun LLVMValueRef.appendBasicBlock(context: LLVMContextRef, name: String): LLVMBasicBlockRef =
    LLVMAppendBasicBlockInContext(C = context, Fn = this, name)!!

val LLVMValueRef.entryBasicBlock: LLVMBasicBlockRef
    get() = LLVMGetEntryBasicBlock(Fn = this)!!

val LLVMBasicBlockRef.firstInstruction: LLVMValueRef?
    get() = LLVMGetFirstInstruction(BB = this)

val LLVMBasicBlockRef.terminator: LLVMValueRef?
    get() = LLVMGetBasicBlockTerminator(BB = this)

val LLVMBuilderRef.insertBlock: LLVMBasicBlockRef?
    get() = LLVMGetInsertBlock(this)

// ===== Builder Positioning =====

fun LLVMBuilderRef.positionAtEnd(block: LLVMBasicBlockRef) =
    LLVMPositionBuilderAtEnd(this, block)

fun LLVMBuilderRef.positionBefore(instruction: LLVMValueRef) =
    LLVMPositionBuilderBefore(this, instruction)

// ===== Memory Operations =====

fun LLVMBuilderRef.buildAlloca(type: LLVMTypeRef, name: String = ""): LLVMValueRef =
    LLVMBuildAlloca(this, type, name)!!

fun LLVMBuilderRef.buildLoad(type: LLVMTypeRef, ptr: LLVMValueRef, name: String = ""): LLVMValueRef =
    LLVMBuildLoad2(this, type, ptr, name)!!

fun LLVMBuilderRef.buildStore(value: LLVMValueRef, ptr: LLVMValueRef): LLVMValueRef =
    LLVMBuildStore(this, value, ptr)!!

fun LLVMBuilderRef.buildGEP(
    type: LLVMTypeRef,
    ptr: LLVMValueRef,
    indices: List<LLVMValueRef>,
    name: String = ""
): LLVMValueRef = LLVMBuildGEP2(
    this, type, ptr, indices.toCValues(), indices.size.toUInt(), name
)!!

fun LLVMBuilderRef.buildInBoundsGEP(
    type: LLVMTypeRef,
    ptr: LLVMValueRef,
    indices: List<LLVMValueRef>,
    name: String = ""
): LLVMValueRef = LLVMBuildInBoundsGEP2(
    this, type, ptr, indices.toCValues(), indices.size.toUInt(), name
)!!

fun LLVMBuilderRef.buildGlobalStringPtr(str: String, name: String = "str"): LLVMValueRef =
    LLVMBuildGlobalStringPtr(this, str, name)!!

// ===== Arithmetic Operations =====

fun LLVMBuilderRef.buildAdd(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "add"): LLVMValueRef =
    LLVMBuildAdd(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildSub(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "sub"): LLVMValueRef =
    LLVMBuildSub(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildMul(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "mul"): LLVMValueRef =
    LLVMBuildMul(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildSDiv(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "div"): LLVMValueRef =
    LLVMBuildSDiv(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildSRem(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "mod"): LLVMValueRef =
    LLVMBuildSRem(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildNeg(value: LLVMValueRef, name: String = "neg"): LLVMValueRef =
    LLVMBuildNeg(this, value, name)!!

// ===== Floating Point Operations =====

fun LLVMBuilderRef.buildFAdd(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "add"): LLVMValueRef =
    LLVMBuildFAdd(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildFSub(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "sub"): LLVMValueRef =
    LLVMBuildFSub(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildFMul(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "mul"): LLVMValueRef =
    LLVMBuildFMul(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildFDiv(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "div"): LLVMValueRef =
    LLVMBuildFDiv(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildFNeg(value: LLVMValueRef, name: String = "neg"): LLVMValueRef =
    LLVMBuildFNeg(this, value, name)!!

// ===== Bitwise Operations =====

fun LLVMBuilderRef.buildAnd(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "and"): LLVMValueRef =
    LLVMBuildAnd(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildOr(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "or"): LLVMValueRef =
    LLVMBuildOr(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildXor(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "xor"): LLVMValueRef =
    LLVMBuildXor(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildNot(value: LLVMValueRef, name: String = "not"): LLVMValueRef =
    LLVMBuildNot(this, value, name)!!

fun LLVMBuilderRef.buildShl(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "shl"): LLVMValueRef =
    LLVMBuildShl(this, lhs, rhs, name)!!

fun LLVMBuilderRef.buildAShr(lhs: LLVMValueRef, rhs: LLVMValueRef, name: String = "shr"): LLVMValueRef =
    LLVMBuildAShr(this, lhs, rhs, name)!!

// ===== Comparison Operations =====

fun LLVMBuilderRef.buildICmp(
    predicate: LLVMIntPredicate,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    name: String = "icmp"
): LLVMValueRef = LLVMBuildICmp(this, predicate, lhs, rhs, name)!!

fun LLVMBuilderRef.buildFCmp(
    predicate: LLVMRealPredicate,
    lhs: LLVMValueRef,
    rhs: LLVMValueRef,
    name: String = "fcmp"
): LLVMValueRef = LLVMBuildFCmp(this, predicate, lhs, rhs, name)!!

val LLVMIntEQ: LLVMIntPredicate
    get() = 32u

val LLVMIntNE: LLVMIntPredicate
    get() = 33u


val LLVMIntSGT: LLVMIntPredicate = 38u
val LLVMIntSGE: LLVMIntPredicate = 39u

val LLVMIntSLT: LLVMIntPredicate = 40u

val LLVMIntSLE: LLVMIntPredicate = 41u

// ===== Cast Operations =====

fun LLVMBuilderRef.buildZExt(
    value: LLVMValueRef,
    destType: LLVMTypeRef,
    name: String = "z_ext"
): LLVMValueRef = LLVMBuildZExt(this, value, destType, name)!!

fun LLVMBuilderRef.buildSExt(
    value: LLVMValueRef,
    destType: LLVMTypeRef,
    name: String = "s_ext"
): LLVMValueRef = LLVMBuildSExt(this, value, destType, name)!!

fun LLVMBuilderRef.buildTrunc(
    value: LLVMValueRef,
    destType: LLVMTypeRef,
    name: String = "trunc"
): LLVMValueRef = LLVMBuildTrunc(this, value, destType, name)!!

fun LLVMBuilderRef.buildFPToSI(
    value: LLVMValueRef,
    destType: LLVMTypeRef,
    name: String = "fp_to_si"
): LLVMValueRef = LLVMBuildFPToSI(this, value, destType, name)!!

fun LLVMBuilderRef.buildSIToFP(
    value: LLVMValueRef,
    destType: LLVMTypeRef,
    name: String = "si_to_fp"
): LLVMValueRef = LLVMBuildSIToFP(this, value, destType, name)!!

fun LLVMBuilderRef.buildBitCast(
    value: LLVMValueRef,
    destType: LLVMTypeRef,
    name: String = "bitcast"
): LLVMValueRef = LLVMBuildBitCast(this, value, destType, name)!!

fun LLVMBuilderRef.buildPointerCast(
    value: LLVMValueRef,
    destType: LLVMTypeRef,
    name: String = "ptr_cast"
): LLVMValueRef = LLVMBuildPointerCast(this, value, destType, name)!!

fun LLVMTypeRef.getIntTypeWidth(): UInt = LLVMGetIntTypeWidth(this)

fun LLVMBuilderRef.buildIntToPtr(
    value: LLVMValueRef,
    destType: LLVMTypeRef,
    name: String = "int_to_ptr"
): LLVMValueRef = LLVMBuildIntToPtr(this, value, destType, name)!!

fun LLVMBuilderRef.buildPtrToInt(
    value: LLVMValueRef,
    destType: LLVMTypeRef,
    name: String = "ptr_to_int"
): LLVMValueRef = LLVMBuildPtrToInt(this, value, destType, name)!!

// ===== Control Flow =====

fun LLVMBuilderRef.buildRet(value: LLVMValueRef): LLVMValueRef =
    LLVMBuildRet(this, value)!!

fun LLVMBuilderRef.buildRetVoid(): LLVMValueRef =
    LLVMBuildRetVoid(this)!!

fun LLVMBuilderRef.buildBr(dest: LLVMBasicBlockRef): LLVMValueRef =
    LLVMBuildBr(this, dest)!!

fun LLVMBuilderRef.buildCondBr(
    condition: LLVMValueRef,
    thenBlock: LLVMBasicBlockRef,
    elseBlock: LLVMBasicBlockRef
): LLVMValueRef = LLVMBuildCondBr(this, condition, thenBlock, elseBlock)!!

fun LLVMBuilderRef.buildSwitch(
    value: LLVMValueRef,
    elseBlock: LLVMBasicBlockRef,
    numCases: UInt
): LLVMValueRef = LLVMBuildSwitch(this, value, elseBlock, numCases)!!

fun LLVMValueRef.addCase(onVal: LLVMValueRef, dest: LLVMBasicBlockRef) =
    LLVMAddCase(this, onVal, dest)

// ===== PHI Nodes =====

fun LLVMBuilderRef.buildPhi(type: LLVMTypeRef, name: String = "phi"): LLVMValueRef =
    LLVMBuildPhi(this, type, name)!!

fun LLVMValueRef.addIncoming(
    values: List<LLVMValueRef>,
    blocks: List<LLVMBasicBlockRef>
) = LLVMAddIncoming(
    PhiNode = this,
    IncomingValues = values.toCValues(),
    IncomingBlocks = blocks.toCValues(),
    Count = values.size.toUInt()
)

// ===== Function Calls =====

fun LLVMBuilderRef.buildCall(
    functionType: LLVMTypeRef,
    function: LLVMValueRef,
    args: List<LLVMValueRef>,
    name: String = ""
): LLVMValueRef = LLVMBuildCall2(
    this, functionType, function, args.toCValues(), args.size.toUInt(), name
)!!
