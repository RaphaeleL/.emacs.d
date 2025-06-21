SHELL := /bin/bash
.PHONY: unittests roletests clean help config health restart nuke stop startup-time

# Configuration
EMACS_DIR := $(HOME)/.emacs.d
LIRA_CONFIG_DIR := $(HOME)/.config/lira

# Default target
help:
	@echo "Available targets:"
	@echo ""
	@echo "  unittests       - Run all tests in batch mode"
	@echo "  roletests      - Run tests interactively"
	@echo "  test           - Run quick configuration test"
	@echo "  clean          - Clean up temporary files"
	@echo "  config         - Install Lira configuration"
	@echo "  health         - Check the health of your Lira configuration"
	@echo "  restart        - Clean and restart Emacs (deletes elpa, eln-cache, etc.)"
	@echo "  nuke           - Completely remove Lira configuration and clean up"
	@echo "  stop           - Stop Emacs"
	@echo "  startup-time   - Measure Emacs startup time"
	@echo "  help           - Show this help message"
	@echo ""
	@echo "Use 'make <target>' to run any of the above commands."


test:
	@echo "Running quick configuration test..."
	emacs --batch --eval "(load-file \"init.el\")" --eval "(message \"Configuration loaded successfully\")"

unittests:
	@echo "Running Lira configuration tests..."
	emacs --batch -l tests/run-tests.el

roletests:
	@echo "Running Lira configuration tests interactively..."
	emacs --batch -l tests/test-configuration.el --eval "(ert-run-tests-interactively t)"

clean:
	@echo "Cleaning up temporary files..."
	@find . -name "*.elc" -delete
	@find . -name "*~" -delete
	@find . -name ".#*" -delete

config:
	@echo "Installing Lira configuration..."
	cp -r $(EMACS_DIR)/templates/lira $(LIRA_CONFIG_DIR)
	@echo "Lira configuration installed successfully."

health:
	@echo "🔍 Running Lira health check..."
	@echo ""
	@errors=0; \
	if command -v emacs >/dev/null 2>&1; then \
		version=$$(emacs --version | head -n1); \
		echo "✓ Emacs found: $$version"; \
	else \
		echo "✗ Emacs not found in PATH"; \
		errors=$$((errors + 1)); \
	fi; \
	echo ""; \
	if [ -d "$(LIRA_CONFIG_DIR)" ]; then \
		echo "✓ Lira config directory found: $(LIRA_CONFIG_DIR)"; \
		missing_files=""; \
		for file in init.el keymaps.el custom.el; do \
			if [ ! -f "$(LIRA_CONFIG_DIR)/$$file" ]; then \
				missing_files="$$missing_files $$file"; \
			fi; \
		done; \
		if [ -z "$$missing_files" ]; then \
			echo "✓ All essential Lira config files present"; \
		else \
			echo "⚠ Missing config files:$$missing_files"; \
		fi; \
	else \
		echo "✗ Lira config directory not found: $(LIRA_CONFIG_DIR)"; \
		errors=$$((errors + 1)); \
	fi; \
	echo ""; \
	dirs="elpa eln-cache auto-save-list backups"; \
	missing_dirs=""; \
	for dir in $$dirs; do \
		if [ ! -d "$(EMACS_DIR)/$$dir" ]; then \
			missing_dirs="$$missing_dirs $$dir"; \
		fi; \
	done; \
	if [ -z "$$missing_dirs" ]; then \
		echo "✓ All Emacs directories present"; \
	else \
		echo "⚠ Missing directories:$$missing_dirs"; \
	fi; \
	echo ""; \
	if [ -d ".git" ]; then \
		echo "✓ Git repository detected - symlink not needed"; \
	else \
		echo "⚠ No git repository found - consider using a symlink for $(EMACS_DIR)/init.el"; \
	fi; \
	echo ""; \
	if [ $$errors -eq 0 ]; then \
		echo "✓ Health check completed successfully!"; \
	else \
		echo "✗ Health check found $$errors error(s)"; \
		exit 1; \
	fi

startup-time:
	@echo "⏱️  Measuring Emacs startup time..."
	@echo ""
	@echo "Starting Emacs in batch mode to measure startup..."
	@start_time=$$(date +%s.%N); \
	emacs --batch --eval "(load-file \"init.el\")" --eval "(message \"Startup complete\")" 2>/dev/null; \
	end_time=$$(date +%s.%N); \
	startup_time=$$(echo "$$end_time - $$start_time" | bc -l); \
	echo "✓ Emacs startup time: $$startup_time seconds"
	@echo ""
	@echo "For detailed startup profiling, start Emacs normally and check the *Messages* buffer."

stop:
	@echo "🛑 Stopping Emacs..."
	@pkill -f "emacs" 2>/dev/null || true
	@echo "✓ Emacs stopped"

restart: stop clean
	@echo "🔄 Restarting Emacs..."
	emacs &
	@echo "✓ Emacs started successfully!"

nuke: 
	@echo "💥 Uninstalling Lira..."
	@echo ""
	@echo "⚠ This will completely remove Lira and clean up your Emacs configuration!"
	@echo ""
	@echo "This will:"
	@echo "  - Remove Lira config directory ($(LIRA_CONFIG_DIR))"
	@echo "  - Remove symlink from $(EMACS_DIR)/init.el"
	@echo "  - Clean up Emacs directories (elpa, eln-cache, etc.)"
	@echo ""
	@read -p "Are you sure you want to continue? (y/N): " REPLY; \
	if [ "$$REPLY" != "y" ] && [ "$$REPLY" != "Y" ]; then \
		echo "Uninstall cancelled."; \
		exit 0; \
	fi; \
	echo "Stopping existing Emacs processes..."; \
	pkill -f "emacs" 2>/dev/null || true; \
	sleep 2; \
	if [ -d "$(LIRA_CONFIG_DIR)" ]; then \
		echo "Removing Lira config directory..."; \
		rm -rf "$(LIRA_CONFIG_DIR)"; \
		echo "✓ Lira config directory removed"; \
	else \
		echo "⚠ Lira config directory not found"; \
	fi; \
	if [ -L "$(EMACS_DIR)/init.el" ]; then \
		echo "Removing symlink..."; \
		rm "$(EMACS_DIR)/init.el"; \
		echo "✓ Symlink removed"; \
	else \
		echo "⚠ No symlink found at $(EMACS_DIR)/init.el"; \
	fi; \
	echo "Cleaning Emacs directories..."; \
	dirs_to_clean="elpa eln-cache auto-save-list backups"; \
	for dir in $$dirs_to_clean; do \
		if [ -d "$(EMACS_DIR)/$$dir" ]; then \
			echo "Cleaning $$dir..."; \
			rm -rf "$(EMACS_DIR)/$$dir"; \
		fi; \
	done; \
	echo "Cleaning temporary files..."; \
	find "$(EMACS_DIR)" -name "*.elc" -delete 2>/dev/null || true; \
	find "$(EMACS_DIR)" -name "*~" -delete 2>/dev/null || true; \
	find "$(EMACS_DIR)" -name "#*#" -delete 2>/dev/null || true; \
	echo "✓ Uninstall completed successfully!"