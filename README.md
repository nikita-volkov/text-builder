# Summary

- Efficient monoidal builder of strict textual values
- Text formatting library, an alternative to `printf` or the "formatting" Haskell library

# Performance

The benchmarks show that it's 2-5 times faster than the lazy text builder supplied with the "text" package. In the years of existence of this package the collected user feedback proves the same.

```
benchmarking TextBuilder/Small input
time                 137.4 ns   (136.0 ns .. 139.9 ns)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 137.3 ns   (136.0 ns .. 139.8 ns)
std dev              5.664 ns   (1.587 ns .. 9.682 ns)
variance introduced by outliers: 61% (severely inflated)

benchmarking TextBuilder/Large input
time                 6.799 ms   (6.772 ms .. 6.832 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.768 ms   (6.743 ms .. 6.788 ms)
std dev              68.99 μs   (56.84 μs .. 83.06 μs)

benchmarking Data.Text.Lazy.Builder/Small input
time                 252.1 ns   (251.2 ns .. 253.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 253.5 ns   (252.7 ns .. 254.5 ns)
std dev              3.273 ns   (2.477 ns .. 4.225 ns)
variance introduced by outliers: 13% (moderately inflated)

benchmarking Data.Text.Lazy.Builder/Large input
time                 35.77 ms   (35.04 ms .. 36.14 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 35.94 ms   (35.51 ms .. 36.33 ms)
std dev              805.3 μs   (628.5 μs .. 1.043 ms)
```

# How it works

It constructs text in two phases. In the first one it estimates the size of the byte array and in the second one it allocates it once and populates it in one go.

# What is text-builder-dev?

It is a lower-level library which this one wraps and provides a stable interface for. It serves as a testing ground for new features and design exploration.
