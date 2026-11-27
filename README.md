# Configuration of my Emacs

Just Clone this Repo and open Emacs. The Rest should be done automatically.

```bash
git clone --depth 1 --recurse-submodules https://github.com/RaphaeleL/.emacs.d ~/.emacs.d
```

In case you want to install emacs from source you need following commands:

```bash

# Get Emacs
curl -LO https://ftpmirror.gnu.org/emacs/emacs-31.1.tar.xz
tar xf emacs-31.1.tar.xz
cd emacs-31.1

# MacOS
brew install pkgconf texinfo gmp gnutls tree-sitter libxml2 ncurses
brew install autoconf gnu-sed # only needed for building from git:
make distclean
ac_cv_func_pipe2=no CC=clang ./configure --prefix="$HOME/.local" \
  --with-native-compilation --with-gnutls --with-xml2 --with-modules \
  --with-tree-sitter --without-x --with-ns --without-dbus \
  --without-imagemagick --without-selinux --disable-acl
make -j"$(sysctl -n hw.ncpu)"
make install


# Optional: Move the Emacs.app to a better location
cp -R nextstep/Emacs.app ~/Applications/
open ~/Applications/Emacs.app

# Optional: Make Emacs executable through the CLI 
mkdir -p ~/.local/bin
cat > ~/.local/bin/emacs <<'EOF'
#!/bin/sh
exec "$HOME/Applications/Emacs.app/Contents/MacOS/Emacs" "$@"
EOF
chmod +x ~/.local/bin/emacs
ln -sf /Applications/Emacs.app/Contents/MacOS/bin/emacsclient ~/.local/bin/emacsclient
```

If you want some useful alias' consider following:

```bash
em() { emacs "$@" >/dev/null 2>&1 &; disown }                                 # gui emacs
eml() { emacs -q -l .emacs.d/minimal.el "$@" >/dev/null 2>&1 &; disown }      # minimal gui emacs
emd() { emacsclient -c "$@" >/dev/null 2>&1 &; disown }                       # gui emacs with daemon
emt() { emacs "$@" -nw }                                                      # tty emacs
emdt() { emacsclient -c --nw "$@" }                                           # tty emacs with daemon
emdaemon() { emacs --daemon >/dev/null 2>&1 & disown }                        # start emacs daemon
emds() { emacsclient -s "main" -a "" -c -n "$@" &>/dev/null }                 # gui emacs with autostarted daemon
```
