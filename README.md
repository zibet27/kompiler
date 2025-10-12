# Kompiler 🧩

**Kompiler** is a small C-like compiler written in **Kotlin Multiplatform**.  
It translates a simple, statically typed language into **WebAssembly (WASM)** and can run both on the **JVM** and **in the browser**.

> A compiler that runs in your browser, visualizes its IR, and lets you play with optimizations — because learning compilers should be interactive.

---

## 🚀 Overview

**Kompiler** explores how a modern compiler can be built from scratch, end to end —  
from parsing and semantic analysis to LLVM IR generation and WASM output.

The project’s focus is educational: understanding how real compilers are structured while experimenting with modern tooling and deployment targets.  
The multiplatform approach keeps the compiler logic shared and platform-independent, while separate modules handle CLI and browser execution.

---

## 🧩 Architecture

- **🔗 Common Core:** shared across all targets (no platform dependencies)
- **⚡ CLI Module:** compiles and runs programs using LLVM and Wasmtime
- **🌐 Web Module:** visualizes IR and runs compiled code in the browser 

---

## 📜 Example

```c
int add(int a, int b) {
    return a + b;
}

int main() {
    return add(2, 3);
}
```

```bash
$ kompiler example.k -o example.wasm
$ wasmtime example.wasm
# => 5
```

## 🗺️ Roadmap

- [ ] Basic grammar and AST
- [ ] Semantic analysis
- [ ] IR generation
- [ ] Optimisations...
- [ ] WASM emission
- [ ] Browser UI with IR visualization
- [ ] Live execution and optimization toggles

---

## 🧠 Acknowledgments

Developed as part of a NIE-GEN (Code Generators) course project.
The goal is to learn compiler design hands-on, experiment with Kotlin Multiplatform.