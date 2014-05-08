alias l="ls -lah --color";
alias grep="grep --color $@";
alias p="pushd"
alias o="popd"
alias d="dirs -v"
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

PATH="$(ruby -e 'puts Gem.user_dir')/bin:$PATH"
