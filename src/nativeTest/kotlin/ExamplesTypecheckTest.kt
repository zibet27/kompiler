package test

import kotlin.test.Test
import kotlin.test.assertTrue
import kotlin.test.assertEquals
import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.readString
import parser.KodeParser
import ast.visitor.TypeChecker

class ExamplesTypecheckTest {
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

        val parser = KodeParser()

        var failed = 0
        files.forEach { path ->
            val source = readFile(path)
            val program = parser.parse(source)
            val checker = TypeChecker(program)
            val diags = checker.check()
            if (diags.isNotEmpty()) {
                println("Type errors in ${path.name}:")
                diags.forEach { println("  ${it.span}: ${it.message}") }
                failed++
            }
        }

        assertEquals(0, failed, "Some examples failed type checking")
    }
}
