source /usr/share/git/completion/git-prompt.sh
GIT_PS1_SHOWUPSTREAM="auto"

alias l="ls -lah --color";
alias grep="grep --exclude=*~ --exclude=*# --color $@";
alias p="pushd"
alias o="popd"
alias d="dirs -v"
alias wget="wget --progress=bar"
alias mount-user="sudo mount -o gid=users,fmask=113,dmask=002"
if   [ $TERM == "emacs" ]
then
  PS1="emacs -> [\u@\H: \w]\n> ";
elif [ $TERM == "dumb" ]
then
  PS1="? -> [\u@\H: \w]\n> ";
else
    PS1='\e[0;36m\u@\h:\e[m\e[1;36m$(__git_ps1 "[%s]")\e[0;33m\w\$\e[m\n> '
    # PS1="\e[0;36m\u@\h:$(__git_ps1 "(%s)")\w\$\n> ";
  PS1="\[\033[G\]$PS1"
fi

PATH="$(ruby -e 'puts Gem.user_dir')/bin:$PATH"
export EDITOR=ef
if [[ -n `which google-chrome-unstable 2> /dev/null` ]]; then
    export BROWSER=google-chrome-unstable;
elif [[ -n `which conkeror 2> /dev/null` ]]; then
    export BROWSER=conkeror;
fi

ssh-add -l > /dev/null || alias ssh='ssh-add -l > /dev/null || ssh-add && unalias ssh; ssh'

PATH="/home/se/perl5/bin${PATH+:}${PATH}"; export PATH;
PERL5LIB="/home/se/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/se/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/se/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/se/perl5"; export PERL_MM_OPT;

PATH="$PATH:$HOME/node_modules/.bin"; export PATH;

# Research related
export RISCV=$HOME/research_local/riscv
export PATH=$PATH:$RISCV/bin

export XILINXD_LICENSE_FILE=2100@XilinxLM.bu.edu

# added by travis gem
[ -f /home/se/.travis/travis.sh ] && source /home/se/.travis/travis.sh

. /home/se/usr/src/torch/install/bin/torch-activate

stty -ixon
