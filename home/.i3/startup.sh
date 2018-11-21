#!/bin/bash

# scratchpad
urxvtc -name urxvt-scratchpad &

# 1t
i3-msg "workspace 1t; append_layout ~/.i3/saved/1t.json"
urxvtc -name 1t-dmesg -e dmesg -w &
urxvtc -name 1t-free -e free -mh -s 10 &
urxvtc -name 1t-top -e htop &
urxvtc -name 1t-0 -cd ~ &
urxvtc -name 1t-1 -cd ~ &
urxvtc -name 1t-2 -cd ~ &


# 2n
i3-msg "workspace 2n; append_layout ~/.i3/saved/2n.json"
chromium-browser &

# 3e
i3-msg "workspace 3e; append_layout ~/.i3/saved/3e.json"
emacs &

# 5m
i3-msg "workspace 5m; append_layout ~/.i3/saved/5m.json"
slack &
sametime-connect &

# 8
i3-msg "workspace 8; append_layout ~/.i3/saved/8.json"
urxvtc -name weechat -e weechat &
