My computing environment. This is centered around:
  - Linux
  - A single X window session running one of:
    - xmonad
    - i3
  - Emacs
  - Bash
  - rxvt-unicode

This manages installation/uninstallation using [GNU Stow](https://www.gnu.org/software/stow/) to create/delete symlinks to files in this repository. Consequently, you need the following dependencies:

```bash
apt install stow make
```

You can then install symlinks with

```bash
make
```

You can uninstall symlinks with:

```bash
make unstow
```
