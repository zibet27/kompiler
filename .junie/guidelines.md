Project: kompiler — Kotlin/Native LALR(1) compiler that emits LLVM IR and WebAssembly

Last verified on: 2025-11-27 (macOS, Apple Silicon)

Scope: Advanced, project-specific guidance only. This document records the exact commands and conventions that worked here today, plus gotchas discovered while running the suite.

1) Build and configuration
- Toolchain auto-selection: build.gradle.kts selects a Kotlin/Native target named "native" based on host OS/arch. On macOS aarch64 it uses macosArm64("native"). No JVM target exists.
- External tools required at runtime by Main.kt (invoked via platform.posix.system):
  - llc (LLVM) — used to lower LLVM IR to wasm object: `llc -march=wasm32 -filetype=obj output.ll -o output.o`
  - clang (LLVM) — used to compile C alien runtime for wasm: `clang --target=wasm32-unknown-unknown -O2 -c src/runtime/alien_runtime.c -o alien.o`
  - wasm-ld (LLD) — used to link wasm objects: `wasm-ld output.o alien.o -o output.wasm --no-entry --export-all`
  - wasmtime (optional) — used to run output.wasm when passing `--run` to our program
- Quick sanity checks (expect non-zero versions):
  - `llc --version`
  - `clang --version`
  - `wasm-ld --version`
  - `wasmtime --version` (only if you plan to execute wasm)
- Gradle wrapper drives everything:
  - Build library and executable: `./gradlew nativeBuild`
  - Run the compiler executable (debug): `./gradlew runDebugExecutableNative --args="examples/2_variables.kode"`
    - To immediately produce and run WebAssembly: `./gradlew runDebugExecutableNative --args="examples/2_variables.kode --run"`
  - Outputs written by Main.kt: LLVM IR at `output.ll`; wasm object at `output.o`; C runtime object at `alien.o`; final wasm at `output.wasm`.
- C interop: src/nativeInterop/cinterop/llvm.def is configured for the `main` compilation only; nativeTest does not link to LLVM directly (tests run fine without system LLVM headers).

2) Testing — configure, run, add new tests
- Test framework: kotlin.test for Kotlin/Native. Source root: `src/nativeTest/kotlin`.
- Run all native tests: `./gradlew nativeTest`
  - As of 2025-11-27, baseline on this branch: 73 tests total, 15 failing. This is expected right now; do not treat it as an environment issue. Use the single-test execution flow below when you need green verification during development.
- Single-test execution for Kotlin/Native (reliable):
  1) Build the debug test binary (Gradle does this automatically the first time you run tests): `./gradlew nativeTest`
  2) Execute the test runner directly with a filter to avoid unrelated failing tests:
     - List available tests: `build/bin/native/debugTest/test.kexe --ktest_list_tests`
     - Run specific class or method:
       - Class: `build/bin/native/debugTest/test.kexe --ktest_gradle_filter='test.DemoSetupTest*'`
       - Method: `build/bin/native/debugTest/test.kexe --ktest_filter='test.DemoSetupTest.sanity_check_addition'`
     - Useful flags: `--ktest_logger=SIMPLE`, `--ktest_no_exit_code` for exploratory runs.
  Notes:
  - Standard Gradle filters like `--tests` aren’t honored by Kotlin/Native’s `nativeTest` task; use the executable + `--ktest_*` flags instead.

- How to add a new test
  - Create a file under `src/nativeTest/kotlin`, use package `test`, and import from `kotlin.test`.
  - Example that passed today (2025-11-27):
    ```kotlin
    class DemoSetupTest {
        @kotlin.test.Test
        fun sanity_check_addition() {
            kotlin.test.assertEquals(4, 2 + 2)
        }
    }
    ```
  - Discover/run it:
    - `./gradlew nativeTest` (will run entire suite — may fail due to unrelated tests)
    - Or directly via runner: `build/bin/native/debugTest/test.kexe --ktest_gradle_filter='test.DemoSetupTest*'`

- End-to-end (E2E) parser tests
  - `ParserE2ETest` reads from `examples/` via `kotlinx-io` file APIs. When adding new examples, place `.kode` files in `examples/` and ensure error cases contain `error` in the filename (the test expects failures for such files).
  - Parser table caching: The LALR(1) parsing table is cached to `cache/kode-parsing-table.json` with a lock at `cache/kode-parsing-table.lock`. When grammar changes (docs/kode.grammar or KodeGrammar.kt), the cache is auto-invalidated by a content hash and regenerated; if anything looks stale or corrupted, delete both files and rerun tests.

3) Development and debugging notes
- Code organization highlights
  - Frontend: `lexer/`, `parser/`, `ast/`. Grammar construction at `parser/KodeGrammar.kt`; LALR table generator at `parser/generator/*`.
  - Type checking: `ast/visitor/TypeChecker.kt`.
  - Codegen to LLVM IR: `codegen/Codegen.kt` + `codegen/CodegenContext.kt`.
  - CLI entrypoint: `Main.kt` (parses, typechecks, emits IR, drives LLVM → wasm pipeline).
  - Runtime glue: `src/runtime/alien_runtime.c` compiled to `alien.o` and linked into `output.wasm`.
- Style and conventions
  - Follow existing Kotlin formatting and package structure; tests are under package `test` and prefer `kotlin.test` assertions.
  - Keep compiler phases pure where possible; side effects (file I/O, shelling out) are centralized in `Main.kt`.
  - Parser generator code heavily uses data classes and sealed hierarchies; mirror existing patterns when extending.
- Debugging tips
  - To inspect the parsing table lifecycle: set breakpoints or add temporary `println` in `ParsingTableCache` and `LALRParserGenerator`.
  - To debug E2E parsing: run `ParserE2ETest` via the test executable with `--ktest_gradle_filter='test.ParserE2ETest*'` and switch logger to SIMPLE.
  - To debug LLVM pipeline quickly: run `./gradlew runDebugExecutableNative --args="examples/1_hello_world.kode"` and inspect `output.ll` before wasm steps.
  - If external tools are missing, `Main.kt` prints exit codes from `llc/clang/wasm-ld`; verify PATH and versions.

4) Verified commands this session (macOS, 2025-11-27)
- All tests (expect some failures currently): `./gradlew nativeTest`
- Single demo test (passed): `build/bin/native/debugTest/test.kexe --ktest_gradle_filter='test.DemoSetupTest*'`
- List tests: `build/bin/native/debugTest/test.kexe --ktest_list_tests`
- Build and run compiler on an example: `./gradlew runDebugExecutableNative --args="examples/2_variables.kode --run"`

5) Housekeeping
- Test artifacts live under `build/bin/native/debugTest/` and `build/reports/tests/nativeTest/`.
- Parser cache lives under `cache/`. Safe to delete; it will be regenerated.
- Generated outputs (`output.ll`, `output.o`, `alien.o`, `output.wasm`) are created at project root by `Main.kt`. Clean them with `git clean -fx output.* alien.o` if needed.
