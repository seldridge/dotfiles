TEXINPUTS=:.:..:~/texinputs:
export TEXINPUTS

PATH=$HOME/usr/bin:$PATH
PATH=/opt/cuda/bin:$PATH
export PATH

eval $(ssh-agent)
