TEXINPUTS=:.:..:~/texinputs:
export TEXINPUTS

PATH=$HOME/usr/bin:$PATH
PATH=/opt/cuda/bin:$PATH
export PATH

source $HOME/opt/Xilinx/Vivado/2014.4/settings64.sh

eval $(ssh-agent)
