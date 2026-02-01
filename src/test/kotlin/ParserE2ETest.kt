package test

import kotlin.test.Test
import kotlin.test.assertTrue
import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.readString
import parser.KodeParser
import kotlin.test.assertEquals

class ParserE2ETest {
    private fun readFile(path: Path): String =
        SystemFileSystem.source(path).buffered().use { it.readString() }

    @Test
    fun parse_all_examples_without_crashing() {
        val examplesDir = Path("examples")
        assertTrue(SystemFileSystem.exists(examplesDir), "examples directory not found")

        val files = SystemFileSystem.list(examplesDir)
            .filter { it.name.endsWith(".kode") }
            .sortedBy { it.name }
            .toList()

        assertTrue(files.isNotEmpty(), "No .kode files found in examples/")

        val parser = KodeParser()

        var exceptionCount = 0
        files.forEach { path ->
            val source = readFile(path)
            val shouldThrow = path.name.contains("error")
            runCatching {
                parser.parse(source)
                assertTrue(!shouldThrow, "Expected to fail on $path")
            }.onFailure {
                if (shouldThrow) return@onFailure
                println("Failed to parse $path: $it")
                exceptionCount++
            }
        }
        assertEquals(exceptionCount, 0, "One or more examples failed to parse")
    }
}
