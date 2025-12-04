// Minimal WASI-backed runtime for alien functions used by the Kode compiler output.
// Implements printing without relying on libc/printf by calling WASI fd_write directly.
// Target: wasm32 (unknown-unknown or wasi); wasmtime provides wasi_snapshot_preview1.

#include <stdint.h>
#include <stddef.h>

// Minimal libc shim(s) for bare wasm32-unknown-unknown builds
// Provide strlen because clang may lower simple loops to libc calls at -O2.
__attribute__((weak))
size_t strlen(const char *s) {
    const char *p = s;
    while (*p) p++;
    return (size_t)(p - s);
}

// WASI import: fd_write
typedef struct {
    const void *buf;
    uint32_t buf_len; // size in bytes
} wasi_ciovec_t;

__attribute__((import_module("wasi_snapshot_preview1"), import_name("fd_write")))
extern int wasi_fd_write(uint32_t fd, const wasi_ciovec_t *iovs, uint32_t iovs_len, uint32_t *nwritten);

static void write_buf(const char *s, uint32_t len) {
    wasi_ciovec_t iov = { s, len };
    uint32_t nw = 0;
    (void)wasi_fd_write(1, &iov, 1, &nw); // fd=1 -> stdout
}

static void write_char(char c) {
    write_buf(&c, 1);
}

static void write_str(const char *s) {
    // s is null-terminated
    const char *p = s;
    while (*p) p++;
    write_buf(s, (uint32_t)(p - s));
}

static void write_u32(uint32_t v) {
    char buf[10]; // max 4294967295 -> 10 digits
    int i = 0;
    if (v == 0) {
        write_char('0');
        return;
    }
    while (v > 0) {
        buf[i++] = (char)('0' + (v % 10));
        v /= 10;
    }
    while (i--) write_char(buf[i]);
}

static void write_i32(int32_t x) {
    if (x < 0) {
        write_char('-');
        // beware of INT32_MIN
        uint32_t ux = (uint32_t)(-(int64_t)x);
        write_u32(ux);
    } else {
        write_u32((uint32_t)x);
    }
}

static void write_u32_padded(uint32_t v, int width) {
    char buf[10];
    int i = 0;
    do {
        buf[i++] = (char)('0' + (v % 10));
        v /= 10;
    } while (v > 0 && i < (int)sizeof(buf));
    while (i < width) { // leading zeros
        write_char('0');
        width--;
    }
    while (i--) write_char(buf[i]);
}

static void write_f64_2(double d) {
    // Very small, naive formatting: sign, integer part, '.', two fractional digits (rounded).
    if (d != d) { // NaN check
        write_str("nan");
        return;
    }
    if (d < 0) {
        write_char('-');
        d = -d;
    }
    // integer part
    int64_t ip = (int64_t)d;
    write_i32((int32_t)ip); // clamp to 32-bit for simplicity
    write_char('.');
    double frac = d - (double)ip;
    uint32_t frac2 = (uint32_t)(frac * 100.0 + 0.5); // round to 2 decimals
    if (frac2 >= 100) { // handle rounding overflow like 1.995 -> 2.00
        frac2 = 0;
        write_i32((int32_t)ip + 1);
        write_char('.');
    }
    write_u32_padded(frac2, 2);
}

// --- Alien functions ---
void print_int(int32_t x) {
    write_i32(x);
    write_char(' ');
}

void print_char(uint8_t c) {
    write_char((char)c);
    write_char(' ');
}

void print_double(double d) {
    write_f64_2(d);
    write_char(' ');
}

// Additional alien used by some examples
void print_string(uint8_t *s) {
    write_str((const char *)s);
    // match formatting style (space after value)
    write_char(' ');
}

__attribute__((import_module("wasi_snapshot_preview1"), import_name("proc_exit")))
extern void wasi_proc_exit(uint32_t code);

extern int main(void);

__attribute__((export_name("_start")))
void _start(void) {
    wasi_proc_exit(main());
}