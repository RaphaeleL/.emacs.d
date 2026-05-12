# Configuration of my Emacs

Just Clone this Repo and open Emacs. The Rest should be done automatically.

```bash
git clone --depth 1 --recurse-submodules https://github.com/RaphaeleL/.emacs.d ~/.emacs.d
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
