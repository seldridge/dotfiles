alias l="ls -lah --color";
alias ll="ls -U --color";
alias less="less -r";
#alias gcc="gcc44";
if   [ $TERM == "emacs" ]
then
  PS1="emacs -> [\u@\H: \w]\n> ";
elif [ $TERM == "dumb" ]
then
  PS1="? -> [\u@\H: \w]\n> ";
else
  PS1="\e[0;36m\u@\H:\e[m\w\$\n> ";
  PS1="\[\033[G\]$PS1"
fi

# PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
