#!/bin/bash

# scratchpad
urxvt -name urxvt-scratchpad &

xrandr_dwim

i3-msg "workspace 1t; append_layout ~/.i3/saved/1t.json"

# 1t
urxvt -name 1t-dmesg -e dmesg -w &
urxvt -name 1t-free -e free -m -s 10 &
urxvt -name 1t-top -e top &
urxvt -name 1t-0 -cd ~ &
urxvt -name 1t-1 -cd ~ &
urxvt -name 1t-2 -cd ~ &

i3-msg "workspace 2n; append_layout ~/.i3/saved/2n.json"
# 2n
google-chrome-stable &

# 3e
emacs &
