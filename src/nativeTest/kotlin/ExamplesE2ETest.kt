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

    @Test
    fun typecheck_all_valid_examples() {
        val examplesDir = Path("examples")
        assertTrue(SystemFileSystem.exists(examplesDir), "examples directory not found")

        val files = SystemFileSystem.list(examplesDir)
            .filter { it.name.endsWith(".kode") }
            .filter { !it.name.contains("error") } // skip negative tests
            .sortedBy { it.name }
            .toList()

        assertTrue(files.isNotEmpty(), "No valid .kode files found in examples/")

        val compiler = Kompiler()

        var failed = 0
        for (path in files) {
            val source = readFile(path)
            val result = runCatching { compiler.run(source) }
            if (result.isFailure) {
                println("Failed to run ${path.name}: ${result.exceptionOrNull()?.message}")
                failed++
            }
        }

        assertEquals(0, failed, "Some examples failed to run")
    }
}
