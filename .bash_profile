TEXINPUTS=:.:..:~/texinputs:
export TEXINPUTS

xrdb $HOME/.Xresources
PATH=$HOME/usr/bin:$PATH
export PATH

eval $(ssh-agent)
