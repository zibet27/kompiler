package test

import Kompiler
import kotlin.test.Test
import kotlin.test.assertTrue
import kotlin.test.assertEquals
import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.readString

class ExamplesE2ETest {
    private fun readFile(path: Path): String =
        SystemFileSystem.source(path).buffered().use { it.readString() }

    private fun compileAndRun(source: String): String {
        val compiler = Kompiler()

        // Compile the source
        compiler.compile(source)

        // Run wasmtime and capture output
        val process = ProcessBuilder("wasmtime", "build/out/output.wasm", "--invoke", "main")
            .redirectErrorStream(true)
            .start()

        val output = process.inputStream.bufferedReader().use { it.readText() }
        process.waitFor()

        return output.trim()
    }

    @Test
    fun `run all valid examples and verify output`() {
        val examplesDir = Path("examples")
        assertTrue(SystemFileSystem.exists(examplesDir), "examples directory not found")

        val files = SystemFileSystem.list(examplesDir)
            .filter { it.name.endsWith(".kode") }
            .filter { !it.name.contains("error") } // skip negative tests
            .sortedBy { it.name }
            .toList()

        assertTrue(files.isNotEmpty(), "No valid .kode files found in examples/")

        val failures = mutableListOf<String>()

        for (path in files) {
            runCatching {
                val source = readFile(path)
                val resultPath = Path(path.toString().replace(".kode", ".result"))
                val expectedOutput = readFile(resultPath).trim()
                val result = compileAndRun(source)

                assertEquals(expectedOutput, result)
                println("✓ ${path.name}")
            }.onFailure {
                failures += "${path.name}: ${it.message}"
            }
        }

        assertEquals(0, failures.size, "Some examples failed:\n${failures.joinToString("\n\n")}")
    }
}
