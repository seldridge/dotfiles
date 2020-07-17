My computing environment. This is centered around:
  - Linux
  - A bare X windows session running xmonad
  - Emacs
  - Bash
  - rxvt-unicode

This manages installation/uninstallation using [GNU Stow](https://www.gnu.org/software/stow/) to create/delete symlinks to files in this repository. Consequently, you need the following dependencies to bootstrap:

```bash
# For Debian-based distributions
apt install stow make

# For Fedora-based distributions
dnf install stow make
```

You can then install symlinks with

```bash
make
```

You can uninstall symlinks with:

```bash
make unstow
```

## Dependencies

- stow
- make

## Programs I Usually Install

- xmonad, xmobar, xrandr, dunst
- stalonetray, nm-applet, blueman-applet
- pass
- imagemagick
- maim
- i3lock
- pdftk or qpdf
- rxvt-unicode

## Fonts I Typically Use

- IBM Plex Mono
- Noto Sans Mono CJK
