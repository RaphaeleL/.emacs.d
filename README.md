# Configuration of my Emacs

Just Clone this Repo and open Emacs. The Rest should be done automatically.

```bash
git clone --depth 1 --recurse-submodules https://github.com/RaphaeleL/.emacs.d ~/.emacs.d
```

In case you want to install emacs from source you need following commands:

```bash
git clone --depth 1 https://github.com/emacs-mirror/emacs && cd emacs

# Linux
CFLAGS="-O3 -g0" ./configure --with-ns && make -j

# MacOS
CFLAGS="-O3 -march=native -g0" ./configure --with-ns && make -j
```

If you want some useful alias' consider

```bash
em() { emacs "$@" >/dev/null 2>&1 &; disown }                                 # gui emacs
eml() { emacs -q -l .emacs.d/minimal.el "$@" >/dev/null 2>&1 &; disown }      # minimal gui emacs
emd() { emacsclient -c "$@" >/dev/null 2>&1 &; disown }                       # gui emacs with daemon
emt() { emacs "$@" -nw }                                                      # tty emacs
emdt() { emacsclient -c --nw "$@" }                                           # tty emacs with daemon
emdaemon() { emacs --daemon >/dev/null 2>&1 & disown }                        # start emacs daemon
```

You can also start the emacs daemon automatically

```bash
em() {
    local server_name="main_configuration"
    if emacsclient -s "$server_name" -e t >/dev/null 2>&1; then
        emacsclient -s "$server_name" -c "$@" >/dev/null 2>&1 & disown
    else
        emacs --daemon="$server_name" >/dev/null 2>&1
        emacsclient -s "$server_name" -c "$@" >/dev/null 2>&1 & disown
    fi
}
```

In case you want to have two configurations (like one for GUI and one for TUI) you can seperate that as well

```bash
eml() {
    local init_dir="$HOME/.emacs.d/minimal"
    local server_name="second_configuration"
    if emacsclient -s "$server_name" -e '(+ 1 1)' >/dev/null 2>&1; then
        emacsclient -s "$server_name" -c "$@" >/dev/null 2>&1 & disown
    else
        emacs --init-directory "$init_dir" --daemon="$server_name" >/dev/null 2>&1
        emacsclient -s "$server_name" -c "$@" >/dev/null 2>&1 & disown
    fi
}
```
