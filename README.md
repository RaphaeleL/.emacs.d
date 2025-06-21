# Lira - Emacs Configuration Framework

A modern, modular Emacs configuration framework with Makefile-based management tools.

## Quick Start

Just download this repo and open Emacs. The rest should be done automatically.

```bash
git clone --recurse-submodules https://github.com/RaphaeleL/.emacs.d
```

## Management Commands

Lira uses a Makefile for all configuration management tasks:

### Available Commands

- `make health` - Check the health of your Lira configuration
- `make test` - Run tests on your Lira configuration  
- `make restart` - Clean and restart Emacs (deletes elpa, eln-cache, etc.)
- `make nuke` - Completely remove Lira configuration and clean up
- `make config` - Install Lira configuration
- `make clean` - Clean up temporary files
- `make help` - Show help message

### Examples

```bash
# Check if everything is working correctly
make health

# Run configuration tests
make test

# Clean restart Emacs (useful after updates)
make restart

# Completely remove Lira (with confirmation)
make nuke

# Install Lira configuration
make config
```

## Makefile Targets

The project includes a comprehensive Makefile with all management tasks:

```bash
make help           # Show all available targets
make test           # Run configuration tests
make roletests      # Run tests interactively
make health         # Check configuration health
make restart        # Clean restart Emacs
make nuke           # Complete removal with confirmation
make config         # Install Lira configuration
make clean          # Clean up temporary files
```

## Features

- **Modular Design**: Easy to enable/disable features
- **Cross-platform**: Works on macOS, Linux, and Windows
- **Makefile Management**: Simple, platform-independent management
- **Health Checks**: Built-in diagnostics and testing
- **Clean Restart**: Easy cleanup and restart functionality
- **Complete Uninstall**: Safe removal with minimal fallback configuration 