const fs = require('fs');

const wasmBuffer = fs.readFileSync(process.argv[2]);

let wasmMemory = null;

WebAssembly.instantiate(wasmBuffer, {
    env: {
        print_int: (x) => process.stdout.write(x + " "),
        print_char: (c) => process.stdout.write(String.fromCharCode(c) + " "),
        print_double: (d) => process.stdout.write(d.toFixed(2) + " "),
        print_string: (ptr) => {
            // Read null-terminated string from WASM memory
            const memory = wasmMemory || new Uint8Array();
            let str = '';
            for (let i = ptr; memory[i] !== 0; i++) {
                str += String.fromCharCode(memory[i]);
            }
            process.stdout.write(str);
        }
    }
}).then(result => {
    // Store reference to WASM memory
    wasmMemory = new Uint8Array(result.instance.exports.memory.buffer);
    result.instance.exports.main();
}).catch(err => {
    console.error("Error:", err.message);
    process.exit(1);
});
