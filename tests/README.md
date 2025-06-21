# Lira Emacs Configuration Tests

This directory contains comprehensive unit tests for the Lira Emacs configuration system.

## Test Coverage

The test suite covers:

### ✅ Module Loading Tests
- **test-module-loading** - Verifies all modules load without errors
- **test-utilities-module** - Tests utilities module functions
- **test-completion-module** - Tests completion module functions
- **test-ui-module** - Tests UI module functions
- **test-os-module** - Tests OS module functions

### ✅ Keybinding Tests
- **test-keymap-helper-functions** - Tests keymap helper functions
- **test-user-keybindings** - Tests user keybindings (F1-F8)
- **test-macos-keybindings** - Tests MacOS-specific keybindings

### ✅ SSH Configuration Tests
- **test-ssh-functions** - Tests SSH server functions
- **test-ssh-keybindings** - Tests SSH keybindings (C-c 1-5, 8)

### ✅ Utility Function Tests
- **test-lira-delete-line** - Tests line deletion function
- **test-lira-duplicate-line** - Tests line duplication function
- **test-lira-toggle-buffer** - Tests buffer toggle function

### ✅ Error Handling Tests
- **test-missing-module-handling** - Tests graceful handling of missing modules
- **test-missing-user-config** - Tests graceful handling of missing user config

### ✅ Theme Loading Tests
- **test-theme-loading-functions** - Tests theme loading functions

### ✅ Completion Mode Tests
- **test-completion-mode-toggle** - Tests completion mode switching

### ✅ Font Function Tests
- **test-font-functions** - Tests font-related functions

### ✅ Integration Tests
- **test-full-configuration-load** - Tests complete configuration loading
- **test-loading-performance** - Tests loading performance (should be < 5s)

## Running Tests

### From Command Line

```bash
# Run all tests
make test

# Run tests interactively
make test-interactive

# Quick configuration test
make quick-test

# Test specific components
make test-keybindings
make test-ssh
make test-macos
```

### From Emacs

```elisp
;; Load and run all tests
(load "tests/test-configuration")
(lira-run-all-tests)

;; Run specific test
(ert "test-module-loading")
```

### Individual Test Commands

```bash
# Test keybindings
emacs --batch --eval "(load-file \"init.el\")" --eval "(message \"F1 binding: %s\" (lookup-key global-map (kbd \"<f1>\")))"

# Test SSH functions
emacs --batch --eval "(load-file \"init.el\")" --eval "(message \"SSH function: %s\" (fboundp 'nikos))"

# Test MacOS keybindings
emacs --batch --eval "(load-file \"init.el\")" --eval "(message \"MacOS Option+8: %s\" (lookup-key global-map (kbd \"M-8\")))"
```

## Test Structure

```
tests/
├── test-configuration.el    # Main test suite
├── run-tests.el            # Test runner script
└── README.md              # This file
```

## Adding New Tests

To add new tests:

1. Add your test to `test-configuration.el`
2. Use the `ert-deftest` macro
3. Follow the naming convention `test-*`
4. Include proper setup and cleanup

Example:

```elisp
(ert-deftest test-new-function ()
  "Test new function functionality."
  (lira-test-setup)
  (should (fboundp 'new-function))
  (should (equal (new-function) expected-result)))
```

## Test Dependencies

- **ERT** - Emacs' built-in testing framework
- **Emacs 28+** - Required for fido-mode and other features

## Continuous Integration

These tests can be integrated into CI/CD pipelines:

```yaml
# Example GitHub Actions step
- name: Test Emacs Configuration
  run: make test
```

## Troubleshooting

### Common Issues

1. **ERT not available** - Ensure you're using Emacs 24.3+
2. **Tests fail on missing modules** - This is expected behavior for error handling tests
3. **Performance tests fail** - May need to adjust timing thresholds based on system

### Debug Mode

Run tests with verbose output:

```bash
emacs --batch -l tests/run-tests.el --eval "(setq ert-quiet nil)"
``` 