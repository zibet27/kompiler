package wasm

import llvm.*

/**
 * WebAssembly Backend - Main compiler class.
 *
 * This class orchestrates the compilation from LLVM IR to WebAssembly binary format.
 *
 * Compilation Pipeline:
 * 1. Analyze functions and allocate locals (SSA → Local mapping)
 * 2. Resolve PHI nodes by inserting copies
 * 3. Use Relooper to reconstruct a structured control flow
 * 4. Emit instructions using InstructionEmitter
 * 5. Generate complete .wasm binary with sections
 *
 * Usage:
 *   val wasm = WasmBackend().compile(irModule)
 *   File("output.wasm").writeBytes(wasm)
 */
class WasmBackend {

    // Map to store global variable memory addresses
    private val globalAddressMap = mutableMapOf<IRGlobalVariable, Int>()

    // Map from function signature to type index
    private val typeIndexMap = mutableMapOf<WasmFuncType, Int>()

    /**
     * Compile an LLVM IR module to WebAssembly binary format
     */
    fun compile(module: IRModule): ByteArray {
        return buildWasmModule {
            // Type Section: Define all function signatures
            val typeSection = buildTypeSection(module)
            writeSection(WasmSection.TYPE) {
                writeBytes(typeSection)
            }

            // Import Section: Import external functions
            val imports = buildImportSection(module)
            if (imports.isNotEmpty()) {
                writeSection(WasmSection.IMPORT) {
                    writeVector(imports) { import ->
                        writeName(import.moduleName)
                        writeName(import.name)
                        writeByte(0x00) // function import
                        writeU32Leb(import.typeIndex)
                    }
                }
            }

            // Function Section: Declare function type indices
            val functionIndices = buildFunctionSection(module)
            writeSection(WasmSection.FUNCTION) {
                writeVector(functionIndices) { idx -> writeU32Leb(idx) }
            }

            // Table Section: Declare a function table for indirect calls
            val numFunctions = module.functions.size
            writeSection(WasmSection.TABLE) {
                writeVector(listOf(WasmTableType())) {
                    writeByte(0x70) // funcref type
                    writeByte(0x00) // no maximum limit
                    writeU32Leb(numFunctions) // minimum size
                }
            }

            // Memory Section: Declare linear memory
            writeSection(WasmSection.MEMORY) {
                writeVector(listOf(WasmLimits(min = 1, max = 100))) { limits ->
                    limits.encode(this)
                }
            }

            // Export Section: Export functions
            val exports = buildExportSection(module)
            writeSection(WasmSection.EXPORT) {
                writeVector(exports) { export ->
                    writeName(export.name)
                    writeByte(export.kind.code)
                    writeU32Leb(export.index)
                }
            }

            // Build data segments first (populates globalAddressMap)
            val dataSegments = buildDataSection(module)

            // Element Section: Populate the function table with all functions
            val elementSegments = buildElementSection(module)
            if (elementSegments.isNotEmpty()) {
                writeSection(WasmSection.ELEMENT) {
                    writeVector(elementSegments) { segment ->
                        writeBytes(segment)
                    }
                }
            }

            // Code Section: Function bodies (uses globalAddressMap)
            val codeBodies = buildCodeSection(module)
            writeSection(WasmSection.CODE) {
                writeVector(codeBodies) { body ->
                    writeBytes(body)
                }
            }

            // Data Section: Initialize memory with global data
            if (dataSegments.isNotEmpty()) {
                writeSection(WasmSection.DATA) {
                    writeVector(dataSegments) { segment ->
                        writeBytes(segment)
                    }
                }
            }
        }
    }

    /**
     * Build the Type section containing all function signatures
     */
    private fun buildTypeSection(module: IRModule): ByteArray {
        val types = mutableListOf<WasmFuncType>()

        // Collect unique function types and populate typeIndexMap
        for (function in module.functions) {
            val funcType = WasmFuncType.fromIRFunction(
                function.returnType,
                function.parameters.map { it.type }
            )
            if (funcType !in typeIndexMap) {
                typeIndexMap[funcType] = types.size
                types.add(funcType)
            }
        }

        // Encode types
        val emitter = WasmEmitter()
        emitter.writeVector(types) { type ->
            type.encode(this)
        }
        return emitter.toByteArray()
    }

    /**
     * Build the Function section with type indices
     */
    private fun buildFunctionSection(module: IRModule): List<Int> {
        val functionIndices = mutableListOf<Int>()

        // Map each function to its type index (typeIndexMap already populated by buildTypeSection)
        for (function in module.functions) {
            if (!function.isExternal) {
                val funcType = WasmFuncType.fromIRFunction(
                    function.returnType,
                    function.parameters.map { it.type }
                )
                functionIndices.add(typeIndexMap[funcType]!!)
            }
        }

        return functionIndices
    }

    /**
     * Build the Import section for external functions
     */
    private fun buildImportSection(module: IRModule): List<WasmImport> {
        val imports = mutableListOf<WasmImport>()

        // Import external functions (typeIndexMap already populated by buildTypeSection)
        for (function in module.functions) {
            if (function.isExternal) {
                val funcType = WasmFuncType.fromIRFunction(
                    function.returnType,
                    function.parameters.map { it.type }
                )
                imports.add(
                    WasmImport(
                        moduleName = "env",  // Standard module name for imports
                        name = function.name,
                        typeIndex = typeIndexMap[funcType]!!
                    )
                )
            }
        }

        return imports
    }

    /**
     * Build the Export section
     */
    private fun buildExportSection(module: IRModule): List<WasmExport> {
        val exports = mutableListOf<WasmExport>()

        // Count external functions (they come first in function index space)
        val numImports = module.functions.count { it.isExternal }

        // Export all non-external functions
        var functionIndex = numImports  // Start after imports
        for (function in module.functions) {
            if (!function.isExternal) {
                exports.add(
                    WasmExport(
                        name = function.name,
                        kind = WasmExportKind.FUNC,
                        index = functionIndex
                    )
                )
                functionIndex++
            }
        }

        // Export memory
        exports.add(
            WasmExport(
                name = "memory",
                kind = WasmExportKind.MEMORY,
                index = 0
            )
        )

        return exports
    }

    /**
     * Build the Data section for global string constants
     */
    private fun buildDataSection(module: IRModule): List<ByteArray> {
        val dataSegments = mutableListOf<ByteArray>()
        var memoryOffset = 0

        for (global in module.globals) {
            if (global.initializer is IRStringConstant) {
                // Store the memory address for this global
                globalAddressMap[global] = memoryOffset

                val strConst = global.initializer as IRStringConstant
                val strBytes = strConst.value.toByteArray(Charsets.UTF_8) + 0 // null-terminated

                val emitter = WasmEmitter()
                // Mode 0: active segment with memory index
                emitter.writeByte(0x00)
                // Offset expression: i32.const <offset>
                emitter.writeByte(0x41) // i32.const
                emitter.writeS32Leb(memoryOffset)
                emitter.writeByte(0x0B) // end
                // Data bytes
                emitter.writeU32Leb(strBytes.size)
                emitter.writeBytes(strBytes)

                dataSegments.add(emitter.toByteArray())
                memoryOffset += strBytes.size
            }
        }

        return dataSegments
    }

    /**
     * Build the Element section to populate the function table
     */
    private fun buildElementSection(module: IRModule): List<ByteArray> {
        val elements = mutableListOf<ByteArray>()

        // Build a list of all function indices
        val functionIndices = mutableListOf<Int>()
        var index = 0

        // Add all functions (imports first, then defined functions)
        for (func in module.functions) {
            if (func.isExternal) {
                functionIndices.add(index++)
            }
        }
        for (func in module.functions) {
            if (!func.isExternal) {
                functionIndices.add(index++)
            }
        }

        if (functionIndices.isNotEmpty()) {
            val emitter = WasmEmitter()
            // Mode 0: active segment with table index 0
            emitter.writeByte(0x00)
            // Offset expression: i32.const 0 (start at index 0)
            emitter.writeByte(0x41) // i32.const
            emitter.writeS32Leb(0)
            emitter.writeByte(0x0B) // end
            // Function indices vector
            emitter.writeVector(functionIndices) { funcIndex ->
                writeU32Leb(funcIndex)
            }

            elements.add(emitter.toByteArray())
        }

        return elements
    }

    /**
     * Build the Code section with function bodies
     */
    private fun buildCodeSection(module: IRModule): List<ByteArray> {
        val codeBodies = mutableListOf<ByteArray>()

        // Calculate stack frame sizes for each function
        val stackFrameSizes = mutableMapOf<IRFunction, Int>()
        for (function in module.functions) {
            if (!function.isExternal) {
                stackFrameSizes[function] = calculateStackFrameSize(function)
            }
        }

        // Assign non-overlapping stack frames starting after globals
        var stackOffset = 1024
        val stackFrameStarts = mutableMapOf<IRFunction, Int>()
        for (function in module.functions) {
            if (!function.isExternal) {
                stackFrameStarts[function] = stackOffset
                stackOffset += stackFrameSizes[function]!!
            }
        }

        for (function in module.functions) {
            if (!function.isExternal) {
                val stackFrameStart = stackFrameStarts[function]!!
                val body = compileFunctionBody(function, module, stackFrameStart)
                codeBodies.add(body)
            }
        }

        return codeBodies
    }

    /**
     * Calculate the stack frame size needed for a function's allocas
     */
    private fun calculateStackFrameSize(function: IRFunction): Int {
        var size = 0
        for (block in function.basicBlocks) {
            for (instruction in block.instructions) {
                if (instruction is IRInstruction.Alloca) {
                    val allocaSize = when (val type = instruction.allocatedType) {
                        is IRType.Int -> (type.bits + 7) / 8
                        is IRType.Float -> 4
                        is IRType.Double -> 8
                        is IRType.Pointer -> 4
                        is IRType.Array -> {
                            val elementSize = calculateTypeSize(type.elementType)
                            type.size * elementSize
                        }

                        is IRType.Struct -> calculateStructSize(type)
                        else -> 0
                    }
                    // Align to 4-byte boundary
                    val alignedSize = (allocaSize + 3) and 3.inv()
                    size += alignedSize
                }
            }
        }
        return size
    }

    private fun calculateTypeSize(type: IRType): Int {
        return when (type) {
            is IRType.Int -> (type.bits + 7) / 8
            is IRType.Float -> 4
            is IRType.Double -> 8
            is IRType.Pointer -> 4
            is IRType.Array -> {
                val elementSize = calculateTypeSize(type.elementType)
                type.size * elementSize
            }

            is IRType.Struct -> calculateStructSize(type)
            else -> 0
        }
    }

    private fun calculateStructSize(structType: IRType.Struct): Int {
        var size = 0
        for (elementType in structType.elementTypes) {
            size += calculateTypeSize(elementType)
        }
        return size
    }

    /**
     * Compile a single function body
     */
    private fun compileFunctionBody(function: IRFunction, module: IRModule, stackFrameStart: Int): ByteArray {
        // Build function index map - imports come first, then defined functions
        val functionIndexMap = mutableMapOf<IRFunction, Int>()
        var index = 0

        // Add imports first
        for (func in module.functions) {
            if (func.isExternal) {
                functionIndexMap[func] = index++
            }
        }

        // Then add defined functions
        for (func in module.functions) {
            if (!func.isExternal) {
                functionIndexMap[func] = index++
            }
        }

        // Step 0: Reorder basic blocks for better CFG layout
        val reorderer = IRReorderBlocks()
        reorderer.reorder(function)

        // Step 1: Allocate locals
        val localsManager = LocalsManager(function)
        localsManager.analyzeAndAllocateLocals()

        // Step 2: Resolve PHI nodes
        val phiResolver = PhiResolver(function, localsManager)
        phiResolver.resolve()

        // Step 3: Generate code
        val bodyEmitter = WasmEmitter()

        // Encode locals (excluding parameters)
        val localVars = localsManager.getLocalVarsByType()
        bodyEmitter.writeVector(localVars) { (count, type) ->
            writeU32Leb(count)
            writeByte(type.code)
        }

        // Step 4: Emit instructions using production Relooper
        val instructionEmitter = InstructionEmitter(
            bodyEmitter,
            localsManager,
            functionIndexMap,
            globalAddressMap,
            typeIndexMap,
            stackFrameStart
        )
        val relooper = Relooper(function, bodyEmitter, instructionEmitter, phiResolver)
        relooper.reloop()

        // Encode function body with size prefix
        val bodyBytes = bodyEmitter.toByteArray()
        val sizedBody = WasmEmitter()
        sizedBody.writeU32Leb(bodyBytes.size)
        sizedBody.writeBytes(bodyBytes)
        return sizedBody.toByteArray()
    }

    /**
     * Helper data class for exports
     */
    private data class WasmExport(
        val name: String,
        val kind: WasmExportKind,
        val index: Int
    )

    /**
     * Helper data class for imports
     */
    private data class WasmImport(
        val moduleName: String,
        val name: String,
        val typeIndex: Int
    )
}
