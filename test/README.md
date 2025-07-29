# Unit Tests for mudserver

This directory contains unit tests for the mudserver project. The tests are written using the [hspec](https://hspec.github.io/) testing framework, which provides a BDD-style syntax for writing tests.

## Test Structure

The tests follow the standard Haskell project structure:

- `test/Spec.hs`: The main entry point for running all tests. It uses `hspec-discover` to automatically find and run all test files that end with `Spec.hs` and export a function named `spec`.
- `test/Game/Types/PlayerSpec.hs`: Tests for the `Game.Types.Player` module, specifically for the `mkDefaultPlayer` function.
- `test/PlayerTest.hs`: A standalone test file that can be run directly without using the test-suite configuration in the cabal file.

## Running the Tests

### Using cabal

The tests can be run using cabal:

```bash
cabal test
```

This will build and run all the tests defined in the `test-suite` section of the cabal file.

### Manually

If you encounter build issues with cabal, you can try running the tests manually:

1. Install the required dependencies:
   ```bash
   cabal install --lib hspec containers text
   ```

2. Run the tests using runghc:
   ```bash
   runghc -isrc test/PlayerTest.hs
   ```

## Test Coverage

The tests for `mkDefaultPlayer` verify that:

1. The returned Player has the correct username and password hash
2. Default values are set correctly (XP, equipped items, wielded items)
3. Available slots and hands are set correctly
4. Object properties are set correctly (reference, name, environment, inventory, visibility, persistence)

## Adding New Tests

To add new tests:

1. Create a new file in the `test` directory with a name ending in `Spec.hs`
2. Export a function named `spec` of type `Spec`
3. Write your tests using the hspec syntax

The `hspec-discover` tool will automatically find and run your tests when you run `cabal test`.