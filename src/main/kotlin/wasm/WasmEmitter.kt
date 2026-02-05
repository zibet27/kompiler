package wasm

import java.io.ByteArrayOutputStream

/**
 * Utility class for emitting WebAssembly binary format.
 * Handles LEB128 encoding, section management, and binary output.
 */
class WasmEmitter {
    private val output = ByteArrayOutputStream()

    /**
     * Get the final binary output
     */
    fun toByteArray(): ByteArray = output.toByteArray()

    // ===== Primitive Writers =====

    /**
     * Write a single byte
     */
    fun writeByte(value: Int) {
        output.write(value and 0xFF)
    }

    /**
     * Write multiple bytes
     */
    fun writeBytes(bytes: ByteArray) {
        output.write(bytes)
    }

    /**
     * Write an unsigned 32-bit integer in LEB128 format
     */
    fun writeU32Leb(value: Int) {
        var v = value
        do {
            var byte = v and 0x7F
            v = v ushr 7
            if (v != 0) {
                byte = byte or 0x80
            }
            writeByte(byte)
        } while (v != 0)
    }

    /**
     * Write an unsigned 64-bit long in LEB128 format
     */
    fun writeU64Leb(value: Long) {
        var v = value
        do {
            var byte = (v and 0x7FL).toInt()
            v = v ushr 7
            if (v != 0L) {
                byte = byte or 0x80
            }
            writeByte(byte)
        } while (v != 0L)
    }

    /**
     * Write a signed 32-bit integer in LEB128 format
     */
    fun writeS32Leb(value: Int) {
        var v = value
        var more = true
        while (more) {
            var byte = v and 0x7F
            v = v shr 7

            // Check if more bytes are needed
            more = !((v == 0 && (byte and 0x40) == 0) || (v == -1 && (byte and 0x40) != 0))

            if (more) {
                byte = byte or 0x80
            }
            writeByte(byte)
        }
    }

    /**
     * Write a signed 64-bit long in LEB128 format
     */
    fun writeS64Leb(value: Long) {
        var v = value
        var more = true
        while (more) {
            var byte = (v and 0x7FL).toInt()
            v = v shr 7

            more = !((v == 0L && (byte and 0x40) == 0) || (v == -1L && (byte and 0x40) != 0))

            if (more) {
                byte = byte or 0x80
            }
            writeByte(byte)
        }
    }

    /**
     * Write a float32
     */
    fun writeF32(value: Float) {
        val bits = value.toBits()
        writeByte(bits)
        writeByte(bits shr 8)
        writeByte(bits shr 16)
        writeByte(bits shr 24)
    }

    /**
     * Write a float64
     */
    fun writeF64(value: Double) {
        val bits = value.toBits()
        for (i in 0..7) {
            writeByte((bits shr (i * 8)).toInt())
        }
    }

    /**
     * Write a UTF-8 encoded string with length prefix
     */
    fun writeString(value: String) {
        val bytes = value.toByteArray(Charsets.UTF_8)
        writeU32Leb(bytes.size)
        writeBytes(bytes)
    }

    /**
     * Write a name (same as string in Wasm binary format)
     */
    fun writeName(value: String) = writeString(value)

    // ===== Section Management =====

    /**
     * Write a complete section with automatic size calculation
     */
    fun writeSection(sectionId: Int, content: WasmEmitter.() -> Unit) {
        // Write section ID
        writeByte(sectionId)

        // Buffer the content to calculate size
        val sectionEmitter = WasmEmitter()
        sectionEmitter.content()
        val sectionBytes = sectionEmitter.toByteArray()

        // Write size and content
        writeU32Leb(sectionBytes.size)
        writeBytes(sectionBytes)
    }

    /**
     * Write a vector (length-prefixed array)
     */
    fun <T> writeVector(items: List<T>, writeItem: WasmEmitter.(T) -> Unit) {
        writeU32Leb(items.size)
        items.forEach { writeItem(it) }
    }

    // ===== Wasm Binary Header =====

    /**
     * Write the Wasm magic number and version
     */
    fun writeHeader() {
        // Magic number: \0asm
        writeByte(0x00)
        writeByte(0x61)
        writeByte(0x73)
        writeByte(0x6D)

        // Version: 1
        writeByte(0x01)
        writeByte(0x00)
        writeByte(0x00)
        writeByte(0x00)
    }
}

/**
 * Wasm section IDs
 */
object WasmSection {
    const val CUSTOM = 0
    const val TYPE = 1
    const val IMPORT = 2
    const val FUNCTION = 3
    const val TABLE = 4
    const val MEMORY = 5
    const val GLOBAL = 6
    const val EXPORT = 7
    const val START = 8
    const val ELEMENT = 9
    const val CODE = 10
    const val DATA = 11
    const val DATA_COUNT = 12
}

/**
 * Helper to build a complete Wasm module
 */
fun buildWasmModule(builder: WasmEmitter.() -> Unit): ByteArray {
    val emitter = WasmEmitter()
    emitter.writeHeader()
    emitter.builder()
    return emitter.toByteArray()
}
