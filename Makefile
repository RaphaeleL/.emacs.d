# Makefile for Lira Emacs Configuration

.PHONY: test roletests clean help config uninstall install

# Default target
help:
	@echo "Available targets:"
	@echo ""
	@echo "  test           - Run all tests in batch mode"
	@echo "  roletests      - Run tests interactively"
	@echo "  clean          - Clean up temporary files"
	@echo "  install    	- Install Lira CLI command"
	@echo "  uninstall 		- Uninstall Lira CLI command"
	@echo "  config         - Install Lira configuration"
	@echo "  help           - Show this help message"
	@echo ""
	@echo "After installing, you can use the ´lira´ command to manage your Emacs."


# Run all tests in batch mode
test:
	@echo "Running Lira configuration tests..."
	emacs --batch -l tests/run-tests.el

# Run tests interactively
roletests:
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

# Install Lira configuration
config:
	@echo "Installing Lira configuration..."
	cp -r ~/.emacs.d/templates/lira ~/.config/lira
	@echo "Lira configuration installed successfully."

# Install CLI command
install:
	@echo "Installing Lira CLI command..."
	@chmod +x scripts/lira
	@if [ "$(shell id -u)" -eq 0 ]; then \
		INSTALL_DIR="/usr/local/bin"; \
		echo "Installing system-wide to $$INSTALL_DIR"; \
	else \
		INSTALL_DIR="$(HOME)/.local/bin"; \
		echo "Installing to user directory $$INSTALL_DIR"; \
		mkdir -p "$$INSTALL_DIR"; \
	fi; \
	INSTALL_PATH="$$INSTALL_DIR/lira"; \
	if [ -L "$$INSTALL_PATH" ]; then \
		echo "Removing existing symlink..."; \
		rm "$$INSTALL_PATH"; \
	fi; \
	echo "Creating symlink from $(PWD)/scripts/lira to $$INSTALL_PATH"; \
	ln -s "$(PWD)/scripts/lira" "$$INSTALL_PATH"; \
	echo "✓ Lira CLI installed successfully!"; \
	echo ""; \
	echo "You can now use the following commands:"; \
	echo "  lira health     - Check your Lira configuration health"; \
	echo "  lira test       - Run tests on your configuration"; \
	echo "  lira restart    - Clean and restart Emacs"; \
	echo "  lira uninstall  - Completely remove Lira"; \
	echo "  lira help       - Show help"; \
	echo ""; \
	if ! command -v lira >/dev/null 2>&1; then \
		echo "⚠ 'lira' command not found in PATH yet"; \
		echo "You may need to restart your terminal or add $$INSTALL_DIR to your PATH"; \
	fi

# Uninstall CLI command
uninstall:
	@echo "Uninstalling Lira CLI command..."
	@CLI_PATHS="$(HOME)/.local/bin/lira /usr/local/bin/lira"; \
	for cli_path in $$CLI_PATHS; do \
		if [ -L "$$cli_path" ]; then \
			echo "Removing CLI command from $$cli_path..."; \
			rm "$$cli_path"; \
			echo "✓ CLI command removed"; \
		fi; \
	done; \
	echo "✓ Lira CLI uninstalled successfully!"