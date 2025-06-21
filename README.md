# Lira - A Fast, Modular Emacs Configuration

A lightweight, modular Emacs configuration framework designed for speed and customization. Lira features lazy loading, startup optimizations, and a clean separation of concerns.

## ✨ Features

- **🚀 Fast Startup**: Lazy loading and aggressive optimizations for sub-second startup times
- **📦 Modular Design**: Enable only the features you need
- **🎯 Heavy Module Optimization**: Tools and language support load only when needed
- **🔧 Easy Management**: Simple Makefile-based commands for all operations
- **📊 Built-in Profiling**: Monitor startup times and performance
- **🔄 Template System**: Multiple preset configurations for different use cases

## 🚀 Performance

Lira is optimized for speed with:

- **Lazy Loading**: Heavy modules (tools, languages) load only when used
- **Startup Optimizations**: Aggressive GC tuning and deferred initialization
- **Autoloads**: Commands load only when invoked
- **Three-tier Loading**: Core → Lazy → Heavy modules

Measure your startup time:
```bash
make startup-time
```

## 📦 Installation

### Quick Start

1. **Clone the repository**:
   ```bash
   git clone https://github.com/yourusername/lira ~/.emacs.d
   cd ~/.emacs.d
   ```

2. **Install dependencies**:
   ```bash
   make install
   ```

3. **Set up your configuration**:
   ```bash
   make setup
   ```

4. **Start Emacs**:
   ```bash
   emacs
   ```

### Manual Setup

1. **Copy a template** (optional):
   ```bash
   make template TEMPLATE=lira
   ```

2. **Customize your modules** in `~/.config/lira/init.el`:
   ```elisp
   (lira! completion
          company                ; code completion
          fido)                  ; fallback completion

   (lira! ui
          mood-line              ; modeline
          hl-todo                ; highlight TODOs
          line-numbers)          ; line numbers

   (lira-heavy! tools
          magit                  ; git interface
          lookup)                ; documentation lookup

   (lira-heavy! lang
          emacs-lisp             ; lisp support
          markdown               ; markdown support
          rust)                  ; rust support
   ```

## 🎛️ Management Commands

### Basic Operations

```bash
make install          # Install packages
make update           # Update packages
make clean            # Clean build artifacts
make health           # Check configuration health
make startup-time     # Measure startup performance
```

### Configuration Management

```bash
make setup            # Set up user configuration
make template TEMPLATE=lira  # Use a template
make backup           # Backup current configuration
make restore          # Restore from backup
```

### Development

```bash
make test             # Run configuration tests
make compile          # Byte-compile all files
make lint             # Check for issues
```

## 📁 Project Structure

```
.emacs.d/
├── init.el              # Main entry point
├── modules/             # Feature modules
│   ├── completion.el    # Completion systems
│   ├── editor.el        # Editing features
│   ├── ui.el           # User interface
│   ├── tools.el        # Development tools
│   ├── lang.el         # Language support
│   └── ...
├── templates/          # Configuration templates
│   ├── lira/          # Default configuration
│   ├── bare-minimum/  # Minimal setup
│   ├── ide-like/      # IDE-like experience
│   └── ...
└── Makefile           # Management commands
```

## 🎯 Module System

### Core Modules (Load Immediately)

- **completion**: Code completion systems
- **ui**: User interface enhancements
- **editor**: Editing features
- **emacs**: Core Emacs improvements
- **checkers**: Syntax checking
- **os**: Operating system integration

### Heavy Modules (Lazy Loaded)

- **tools**: Development tools (loads when magit is used)
- **lang**: Language support (loads when file types are opened)

### Module Configuration

```elisp
;; Load immediately
(lira! completion company fido)

;; Load when features are used
(lira-lazy! ui which-key)

;; Load only when specific modes are activated
(lira-heavy! tools magit lookup)
(lira-heavy! lang rust go python)
```

## 🔧 Customization

### User Configuration

Your personal settings go in `~/.config/lira/`:

- `init.el`: Module definitions
- `keymaps.el`: Key bindings
- `custom.el`: Custom settings

### Adding Custom Modules

1. Create `modules/my-module.el`
2. Add module definition in `~/.config/lira/init.el`:
   ```elisp
   (lira! my-module)
   ```

### Performance Tuning

See `PERFORMANCE.md` for detailed optimization guide.

## 📊 Templates

Choose from multiple preset configurations:

- **lira**: Balanced configuration (default)
- **bare-minimum**: Minimal setup
- **ide-like**: IDE-like experience
- **linux**: Linux-specific optimizations
- **windows**: Windows-specific optimizations

## 🐛 Troubleshooting

### Health Check

```bash
make health
```

This will check:
- Emacs installation
- Configuration files
- Required directories
- Git repository status

### Common Issues

1. **Slow startup**: Check `make startup-time` and disable unused modules
2. **Missing packages**: Run `make install` to install dependencies
3. **Configuration errors**: Check `~/.config/lira/` files for syntax errors

### Getting Help

1. Check the `*Messages*` buffer for error details
2. Run `make health` to diagnose issues
3. Review `PERFORMANCE.md` for optimization tips

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test with `make test`
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🙏 Acknowledgments

- Inspired by Doom Emacs and Spacemacs
- Built with modern Emacs best practices
- Optimized for performance and usability 