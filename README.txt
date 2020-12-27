* `emacs-init.el`
location: `~/.emacs.d/init.el`
loaded automatically during emacs startup

* `xterm-xresources.ad`
location: `~/.Xresources`
loaded automatically during boot
update manually using `xrdb -merge <filename>`

* `bash-bashrc.sh`
location: `~/.bashrc`
loaded automatically when an interactive bash shell starts

* `readline-inputrc.sh`
location: `~/.inputrc`
loaded automatically by readline (used by many programs, including bash)

---

Note: xterm-24bit was redundant. Set TERM to "xterm-direct" (or the -direct variant of whatever terminal you use, if available)
