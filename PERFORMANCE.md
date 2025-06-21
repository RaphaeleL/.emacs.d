# Lira Performance Optimization Guide

## Overview

This guide explains the performance optimizations implemented in Lira and how to further improve Emacs startup time.

## Current Optimizations

### 1. Lazy Loading System

Lira implements a three-tier loading system:

- **Core Modules**: Load immediately (essential functionality)
- **Lazy Modules**: Load when their features are first used
- **Heavy Modules**: Load only when specific modes or commands are activated

### 2. Heavy Module Optimization

Heavy modules like `tools` and `lang` are now loaded only when needed:

- **Tools Module**: Loads when `magit` is first used
- **Language Modules**: Load when specific file types are opened

### 3. Startup Optimizations

- Increased GC threshold during startup (200MB)
- Disabled auto-save and desktop save during startup
- Deferred package initialization
- Disabled file name handler cache during startup

### 4. Package Loading Optimizations

- All packages use `:defer t` for lazy loading
- Autoloads for commands to prevent premature loading
- `:init` and `:config` separation for better performance

## Measuring Performance

### Startup Time Measurement

```bash
make startup-time
```

This will show:
- Cold startup time (first run)
- Warm startup time (cached)
- Detailed component timing

### Manual Profiling

Start Emacs normally and check the `*Messages*` buffer for detailed timing information.

## Further Optimizations

### 1. Disable Unused Features

Comment out unused modules in `~/.config/lira/init.el`:

```elisp
(lira! completion
       company                ; Keep this
       ; vertico                ; Disable if not used
       ; orderless              ; Disable if not used
       fido)                  ; Keep this
```

### 2. Reduce Package Dependencies

Remove packages you don't use:

```elisp
(lira-heavy! lang
       emacs-lisp             ; Essential
       markdown               ; Keep if you write docs
       ; web                    ; Disable if not doing web dev
       ; rust                   ; Disable if not using Rust
       sh)                    ; Essential
```

### 3. Optimize Heavy Packages

For packages you must use, consider alternatives:

- Use `consult` instead of `ivy`/`helm`
- Use `vertico` instead of `helm`
- Use `eglot` instead of `lsp-mode` (already configured)

### 4. System-Level Optimizations

#### macOS
```bash
# Disable App Nap for Emacs
defaults write org.gnu.Emacs NSAppSleepDisabled -bool true
```

#### Linux
```bash
# Add to ~/.bashrc or ~/.zshrc
export EMACS_SERVER_FILE=/tmp/emacs$(id -u)/server
```

### 5. Emacs Configuration

Add to your `~/.config/lira/custom.el`:

```elisp
;; Disable features you don't use
(setq auto-save-default nil)           ; Disable auto-save
(setq create-lockfiles nil)            ; Disable lock files
(setq make-backup-files nil)           ; Disable backup files
(setq auto-save-list-file-prefix nil)  ; Disable auto-save list

;; Optimize for speed over memory
(setq gc-cons-threshold (* 16 1024 1024))  ; 16MB
(setq gc-cons-percentage 0.1)

;; Disable expensive features
(setq ring-bell-function 'ignore)      ; Disable bell
(setq visible-bell nil)                ; Disable visible bell
```

## Performance Monitoring

### Built-in Profiling

Lira includes built-in startup profiling. Check the `*Messages*` buffer after startup for timing information.

### External Tools

- **Emacs Profiler**: `M-x profiler-start` then `M-x profiler-report`
- **Benchmark**: `M-x benchmark-run` for specific operations

## Troubleshooting

### Slow Startup

1. Check which modules are taking the most time in `*Messages*`
2. Disable unused modules
3. Consider using `emacs --daemon` for faster subsequent starts

### Memory Issues

1. Reduce `gc-cons-threshold` if you have limited RAM
2. Disable heavy language servers if not needed
3. Use `M-x memory-report` to identify memory hogs

### Package Conflicts

1. Check `*Messages*` for package loading errors
2. Use `M-x package-list-packages` to review installed packages
3. Remove conflicting packages

## Best Practices

1. **Start Small**: Begin with minimal configuration and add features as needed
2. **Profile Regularly**: Use `make startup-time` to monitor performance
3. **Update Regularly**: Keep packages updated for performance improvements
4. **Use Daemon Mode**: Consider using `emacs --daemon` for faster subsequent starts
5. **Monitor Resources**: Use system tools to monitor CPU and memory usage

## Advanced Configuration

For power users, consider:

- Using `straight.el` instead of `package.el` for faster package loading
- Implementing custom lazy loading for specific features
- Using `use-package` with `:demand t` only for essential packages
- Implementing conditional loading based on system resources 