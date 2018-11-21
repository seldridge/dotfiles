source /usr/share/git/git-prompt.sh
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

export EDITOR=ef
if [[ -n `which google-chrome-unstable 2> /dev/null` ]]; then
    export BROWSER=google-chrome-unstable;
elif [[ -n `which conkeror 2> /dev/null` ]]; then
    export BROWSER=conkeror;
fi

ssh-add -l > /dev/null || alias ssh='ssh-add -l > /dev/null || ssh-add && unalias ssh; ssh'

stty -ixon
