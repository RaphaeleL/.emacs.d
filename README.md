# Configuration of my Emacs

Just Clone this Repo and open Emacs. The Rest should be done automatically.

```bash
git clone --depth 1 --recurse-submodules https://github.com/RaphaeleL/.emacs.d ~/.emacs.d
```

In case you want to install emacs from source you need following commands:

```bash
git clone --depth 1 https://github.com/emacs-mirror/emacs && cd emacs
./autogen.sh
make bootstrap
CFLAGS="-O3 -g0" ./configure --with-ns && make -j               # Linux
CFLAGS="-O3 -march=native -g0" ./configure --with-ns && make -j # MacOS
make -j$(sysctl -n hw.ncpu)
make install && open nextstep/Emacs.app
```

If you want some useful alias' consider

```bash
em() { emacs "$@" >/dev/null 2>&1 &; disown }                                 # gui emacs
eml() { emacs -q -l .emacs.d/minimal.el "$@" >/dev/null 2>&1 &; disown }      # minimal gui emacs
emd() { emacsclient -c "$@" >/dev/null 2>&1 &; disown }                       # gui emacs with daemon
emt() { emacs "$@" -nw }                                                      # tty emacs
emdt() { emacsclient -c --nw "$@" }                                           # tty emacs with daemon
emdaemon() { emacs --daemon >/dev/null 2>&1 & disown }                        # start emacs daemon
emds() { emacsclient -s "main" -a "" -c -n "$@" &>/dev/null }                 # gui emacs with autostarted daemon
```
