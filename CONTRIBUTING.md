# Contributing to JuicyPixels-webp

Thank you for your interest in contributing to JuicyPixels-webp!

## Development Setup

1. Clone the repository
2. Install Stack: https://docs.haskellstack.org/
3. Build the project:
   ```bash
   stack build
   ```
4. Run tests:
   ```bash
   stack test
   ```

## Project Structure

```
JuicyPixels-webp/
├── src/                        # Source code (40 modules, ~11,273 lines)
│   └── Codec/Picture/
│       ├── WebP.hs            # Public API
│       └── WebP/Internal/     # Internal modules
├── test/                       # Test suite (27 modules, ~5,244 lines)
├── docs/                       # Specification documents
│   ├── webp-format.md         # VP8L lossless spec (RFC 9649)
│   ├── vp8-bitstream.md       # VP8 lossy spec (RFC 6386)
│   └── libwebp/               # Reference implementation
├── PLAN.md                     # Original implementation plan
├── STATUS.md                   # Current implementation status
└── README.md
```

## Code Style

- Use Ormolu for formatting: `nix fmt` or `ormolu --mode inplace src/ test/`
- Follow existing naming conventions
- Add Haddock documentation for public functions
- Use strict evaluation (`!`) for performance-critical paths
- Prefer explicit type signatures

## Making Changes

### Adding Features

1. Check `STATUS.md` for current state and future enhancements
2. Create a branch: `git checkout -b feature/your-feature`
3. Implement with tests
4. Ensure all tests pass: `stack test`
5. Update documentation
6. Submit a pull request

### Fixing Bugs

1. Add a failing test that demonstrates the bug
2. Fix the bug
3. Verify the test now passes
4. Update test/TestNotes.md if relevant

### Improving Performance

1. Add benchmarks (see `bench/` directory structure below)
2. Profile with `stack build --profile`
3. Optimize hot paths
4. Verify correctness with existing tests

## Testing Requirements

All contributions must:
- ✅ Pass existing tests (`stack test`)
- ✅ Add tests for new functionality
- ✅ Maintain or improve test coverage

Test guidelines:
- Unit tests for individual functions
- Integration tests for components
- Property tests for algorithmic correctness
- Real image tests when applicable

## Documentation

- Update `README.md` for API changes
- Update `test/TestNotes.md` for new tests
- Update `STATUS.md` for implementation progress
- Add Haddock comments for public APIs

## Areas for Improvement

### Encoding Quality
- **VP8L transforms** — predictor, color, and color-indexing transforms not yet used in encoding (larger files, correct output)
- **VP8 coefficient probability updates** — encoder uses fixed defaults
- **VP8 segmentation** — decoder supports it, encoder doesn't use it
- **Advanced mode selection** — encoder uses simple heuristics

### Performance
- Benchmark against libwebp
- SIMD acceleration
- Streaming decode for very large images

### Testing
- Fuzz testing with random/malformed inputs
- Comparison against `dwebp` output for more image variants

## Building and Testing

```bash
# Development build (faster)
stack build --fast

# Optimized build
stack build

# Run tests
stack test

# Run specific test
stack test --test-arguments "-m BitReader"

# With coverage
stack test --coverage

# Format code
nix fmt

# Or with ormolu directly
ormolu --mode inplace $(find src test -name '*.hs')
```

## Debugging

### Enable detailed error messages
```haskell
case decodeWebP bs of
  Left err -> putStrLn $ "Detailed error: " ++ err
  Right img -> ...
```

### Debug BitReader
```haskell
import Codec.Picture.WebP.Internal.BitReader
let reader = initBitReader bs
print $ getBytesRemaining reader
```

### Debug Container
```haskell
import Codec.Picture.WebP.Internal.Container
case parseWebP bs of
  Right webpFile -> print webpFile
  Left err -> putStrLn err
```

## Submission Checklist

Before submitting a PR:

- [ ] Code compiles without warnings
- [ ] All tests pass (`stack test`)
- [ ] New tests added for new functionality
- [ ] Code formatted with Ormolu
- [ ] Documentation updated
- [ ] Commit messages are descriptive
- [ ] No unrelated changes included

## Questions?

- Check `STATUS.md` for current implementation state
- Check `test/TestNotes.md` for test structure
- Open an issue for discussions

## License

By contributing, you agree that your contributions will be licensed under the BSD-3-Clause license.
