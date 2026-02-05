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
            val typeSection = module.buildTypeSection()
            writeSection(WasmSection.TYPE) {
                writeBytes(typeSection)
            }

            // Import Section: Import external functions
            val imports = module.buildImportSection()
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
            val functionIndices = module.buildFunctionSection()
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
            val codeBodies = module.buildCodeSection()
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

    private fun IRFunction.wasmType(): WasmFuncType {
        return WasmFuncType.from(returnType, paramTypes = parameters.map { it.type })
    }

    /**
     * Build the Type section containing all function signatures
     */
    private fun IRModule.buildTypeSection(): ByteArray {
        // Collect unique function types and populate typeIndexMap
        val types = buildList {
            for (function in functions) {
                val funcType = function.wasmType()
                if (funcType !in typeIndexMap) {
                    typeIndexMap[funcType] = size
                    add(funcType)
                }
            }
        }
        return emitWasm {
            writeVector(types) { type -> type.encode(emitter = this) }
        }
    }

    /**
     * Build the Function section with type indices
     */
    private fun IRModule.buildFunctionSection() = buildList {
        // Map each function to its type index (typeIndexMap already populated by buildTypeSection)
        for (function in functions) {
            if (function.isExternal) continue
            add(typeIndexMap[function.wasmType()]!!)
        }
    }

    /**
     * Build the Import section for external functions
     */
    private fun IRModule.buildImportSection(): List<WasmImport> = buildList {
        // Import external functions (typeIndexMap already populated by buildTypeSection)
        for (function in functions) {
            if (!function.isExternal) continue
            val import = WasmImport(
                moduleName = "env",  // Standard module name for imports
                name = function.name,
                typeIndex = typeIndexMap[function.wasmType()]!!
            )
            add(import)
        }
    }

    /**
     * Build the Export section
     */
    private fun buildExportSection(module: IRModule): List<WasmExport> {
        val exports = mutableListOf<WasmExport>()

        // Count external functions (they come first in the function index space)
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

                val dataSegment = emitWasm {
                    // Mode 0: active segment with memory index
                    writeByte(0x00)
                    // Offset expression: i32.const <offset>
                    writeByte(0x41) // i32.const
                    writeS32Leb(memoryOffset)
                    writeByte(0x0B) // end
                    // Data bytes
                    writeU32Leb(strBytes.size)
                    writeBytes(strBytes)
                }

                dataSegments.add(dataSegment)
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
            if (!func.isExternal) continue
            functionIndices.add(index++)
        }
        for (func in module.functions) {
            if (func.isExternal) continue
            functionIndices.add(index++)
        }

        if (functionIndices.isNotEmpty()) {
            emitWasm {
                // Mode 0: active segment with table index 0
                writeByte(0x00)
                // Offset expression: i32.const 0 (start at index 0)
                writeByte(0x41) // i32.const
                writeS32Leb(0)
                writeByte(0x0B) // end
                // Function indices vector
                writeVector(functionIndices) { funcIndex ->
                    writeU32Leb(funcIndex)
                }
            }.also { elements.add(it) }
        }

        return elements
    }

    /**
     * Build the Code section with function bodies
     */
    private fun IRModule.buildCodeSection(): List<ByteArray> {
        // Assign non-overlapping stack frames starting after globals
        var stackOffset = 1024
        val stackFrameStarts = buildMap {
            for (function in functions) {
                if (function.isExternal) continue
                set(function, stackOffset)
                stackOffset += function.stackFrameSize()
            }
        }

        return functions.mapNotNull {
            if (it.isExternal) return@mapNotNull null
            val stackFrameStart = stackFrameStarts[it]!!
            it.compileFunctionBody(module = this, stackFrameStart)
        }
    }

    /**
     * Calculate the stack frame size needed for a function's allocas
     */
    private fun IRFunction.stackFrameSize(): Int {
        var size = 0
        for (block in basicBlocks) {
            for (instruction in block.instructions) {
                if (instruction is IRInstruction.Alloca) {
                    val allocaSize = when (val type = instruction.allocatedType) {
                        is IRType.Int -> (type.bits + 7) / 8
                        is IRType.Float -> 4
                        is IRType.Double -> 8
                        is IRType.Pointer -> 4
                        is IRType.Array -> type.size * type.elementType.calculateSize()
                        is IRType.Struct -> type.calculateSize()
                        else -> 0
                    }
                    // Align to the 4-byte boundary
                    val alignedSize = (allocaSize + 3) and 3.inv()
                    size += alignedSize
                }
            }
        }
        return size
    }

    private fun IRType.calculateSize(): Int {
        return when (this) {
            is IRType.Int -> (bits + 7) / 8
            is IRType.Float -> 4
            is IRType.Double -> 8
            is IRType.Pointer -> 4
            is IRType.Array -> size * elementType.calculateSize()
            is IRType.Struct -> this.calculateSize()
            else -> 0
        }
    }

    private fun IRType.Struct.calculateSize(): Int {
        var size = 0
        for (elementType in elementTypes) {
            size += elementType.calculateSize()
        }
        return size
    }

    /**
     * Compile a single function body
     */
    private fun IRFunction.compileFunctionBody(module: IRModule, stackFrameStart: Int): ByteArray {
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
        reorderer.reorder(function = this)

        // Step 1: Allocate locals
        val localsManager = LocalsManager(function = this)
        localsManager.analyzeAndAllocateLocals()

        // Step 2: Resolve PHI nodes
        val phiResolver = PhiResolver(function = this, localsManager)
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
        val relooper = Relooper(function = this, bodyEmitter, instructionEmitter, phiResolver)
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
