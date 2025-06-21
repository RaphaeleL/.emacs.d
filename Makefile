# Makefile for Lira Emacs Configuration

.PHONY: test test-interactive clean help

# Default target
help:
	@echo "Available targets:"
	@echo "  test           - Run all tests in batch mode"
	@echo "  test-interactive - Run tests interactively"
	@echo "  clean          - Clean up temporary files"
	@echo "  help           - Show this help message"

# Run all tests in batch mode
test:
	@echo "Running Lira configuration tests..."
	emacs --batch -l tests/run-tests.el

# Run tests interactively
test-interactive:
	@echo "Running Lira configuration tests interactively..."
	emacs --batch -l tests/test-configuration.el --eval "(ert-run-tests-interactively t)"

# Clean up temporary files
clean:
	@echo "Cleaning up temporary files..."
	@find . -name "*.elc" -delete
	@find . -name "*~" -delete
	@find . -name ".#*" -delete

# Quick configuration test
quick-test:
	@echo "Running quick configuration test..."
	emacs --batch --eval "(load-file \"init.el\")" --eval "(message \"Configuration loaded successfully\")"

# Test specific components
test-keybindings:
	@echo "Testing keybindings..."
	emacs --batch --eval "(load-file \"init.el\")" --eval "(message \"F1 binding: %s\" (lookup-key global-map (kbd \"<f1>\")))"

test-ssh:
	@echo "Testing SSH functions..."
	emacs --batch --eval "(load-file \"init.el\")" --eval "(message \"SSH function: %s\" (fboundp 'nikos))"

test-macos:
	@echo "Testing MacOS keybindings..."
	emacs --batch --eval "(load-file \"init.el\")" --eval "(message \"MacOS Option+8: %s\" (lookup-key global-map (kbd \"M-8\")))" 