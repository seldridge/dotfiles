alias l="ls -la";
if   [ $TERM == "emacs" ]
then
  PS1="emacs -> [\u@\h: \w]\n> ";
elif [ $TERM == "dumb" ]
then
  PS1="? -> [\u@\h: \w]\n> ";
else
  PS1="\e[0;34m\u@\h: \w\$\n>\e[m ";
fi
