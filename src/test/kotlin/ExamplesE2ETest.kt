import kotlin.test.Test
import kotlin.test.assertTrue
import kotlin.test.assertEquals
import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.readString
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException

class ExamplesE2ETest {
    private fun readFile(path: Path): String =
        SystemFileSystem.source(path).buffered().use { it.readString() }

    private fun compileAndRun(source: String): String {
        val compiler = Kompiler()

        // Compile the source
        compiler.compile(source)

        // Run with a Node.js test runner that provides alien functions
        val process = ProcessBuilder("node", "test_runner.js", "build/out/output.wasm")
            .redirectErrorStream(true)
            .start()

        // Wait for a process with timeout
        val completed = process.waitFor(2, TimeUnit.SECONDS)
        if (!completed) {
            process.destroyForcibly()
            throw TimeoutException("Test execution exceeded timeout")
        }

        val output = process.inputStream.bufferedReader().use { it.readText() }
        if (process.exitValue() != 0) {
            throw RuntimeException("Process exited with code ${process.exitValue()}: $output")
        }

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
