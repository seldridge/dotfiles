alias l="ls -lah";
if   [ $TERM == "emacs" ]
then
  PS1="emacs -> [\u@\h: \w]\n> ";
elif [ $TERM == "dumb" ]
then
  PS1="? -> [\u@\h: \w]\n> ";
else
  PS1="\e[0;36m\u@\h:\e[m\w\$\n> ";
  PS1="\[\033[G\]$PS1"
fi
