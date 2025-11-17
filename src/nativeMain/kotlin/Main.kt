import ast.visitor.TypeChecker
import codegen.Codegen
import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.readString
import parser.KodeParser

fun main(args: Array<String>) {
    val path = args.firstOrNull()
    val source: String = if (path != null) {
        SystemFileSystem.source(Path(path)).buffered().readString()
    } else {
        println("No source code provided. Exiting.")
        return
    }

    val parser = KodeParser()
    val programAst = parser.parse(source)

    val typeErrors = TypeChecker(programAst).check()
    if (typeErrors.isNotEmpty()) {
        typeErrors.forEach { println(it) }
        return
    }

    // Generate LLVM IR and print it
    val ir = Codegen.generate(programAst)
    println(ir)
}
