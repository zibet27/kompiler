# Kompiler 🧩

**Kompiler** is a compiler for the **Kode** programming language written in **Kotlin**.
It translates a simple, statically typed language into **WebAssembly (WASM)** via **LLVM IR** with multiple optimization
passes.

---

## 🚀 Overview

**Kompiler** demonstrates how a compiler works:

1. **Lexing & Parsing** → Abstract Syntax Tree (AST)
2. **Semantic Analysis** → Type checking
3. **Code Generation** → LLVM IR
4. **Optimization** → mem2reg (SSA promotion), function inlining
5. **Backend** → WebAssembly bytecode

The project prioritizes education and experimentation with real compiler techniques.

---

## 🧩 Architecture

**Kompiler** consists of several pipeline stages:

- **Parser** → Grammar-based parsing with custom recursive descent
- **Type Checker** → Static type analysis and error reporting
- **Codegen** → LLVM IR generation with an SSA form
- **Optimization Passes** → mem2reg, inlining (with visualization support)
- **WASM Backend** → Direct LLVM IR to WebAssembly compilation (no LLVM dependency)

---

## 📜 Example

```kode
alien fun print_int{ x: i32 }: void;

fun add{ a: i32, b: i32 }: i32 (
    a + b;
);

fun main{}: i32 (
    i32 result = add{ 2, 3 };
    print_int{ result };
    0;
);
```

### Basic Compilation

```bash
./gradlew run --args="example.kode"
# Compiles to build/out/example.wasm
```

### Run After Compilation

```bash
./gradlew run --args="example.kode --run"
# Compiles and executes using Node.js
```

### Visualize Optimizations

```bash
./gradlew run --args="example.kode --print-opt"
# Shows IR before and after each optimization pass
```

### Advanced Options

```bash
# Disable specific optimizations
./gradlew run --args="example.kode --no-mem2reg --no-inline"

# Print WebAssembly text format (requires wabt)
./gradlew run --args="example.kode --print-wasm"

# Combine options
./gradlew run --args="example.kode --print-opt --run"
```

---

## 🎓 Optimization Examples

The `examples/opt/` directory contains programs demonstrating different optimizations:

| Example                   | Description                                       |
|---------------------------|---------------------------------------------------|
| **1_mem2reg_simple**      | Basic stack-to-register promotion                 |
| **2_mem2reg_conditional** | PHI node insertion at control flow merge points   |
| **3_inlining_simple**     | Single function inlining                          |
| **4_inlining_nested**     | Nested function inlining (square → multiply)      |
| **5_mem2reg_loop**        | PHI nodes for loop variables                      |
| **6_combined**            | Both mem2reg and inlining working together        |
| **7_no_optimize**         | Comparison example with various flag combinations |

### Running Examples

```bash
# See mem2reg optimization in action
./gradlew run --args="examples/opt/1_mem2reg_simple.kode --print-opt"

# Compare with optimization disabled
./gradlew run --args="examples/opt/1_mem2reg_simple.kode --print-opt --no-mem2reg"

# See function inlining
./gradlew run --args="examples/opt/3_inlining_simple.kode --print-opt"

# See nested inlining (square calls multiply, both get inlined)
./gradlew run --args="examples/opt/4_inlining_nested.kode --print-opt"
```

---

## 🛠️ CLI Options

```
Usage: kompiler <path-to-file> [options]

Options:
  --run           Run the compiled WebAssembly after compilation
  --no-mem2reg    Disable mem2reg optimization (alloca to SSA promotion)
  --no-inline     Disable function inlining optimization
  --print-opt     Print IR visualization before/after each optimization pass
  --print-wasm    Print generated WebAssembly text format (requires wabt/wasm2wat)
  -h, --help      Show this help message
```

---

---

## 🧠 Acknowledgments

Developed as part of a NIE-GEN (Code Generators) course project.
The goal is to learn compiler design hands-on.
