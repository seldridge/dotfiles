# Exit quickly if this is a non-interactive shell
if [ -z "$PS1" ]; then
  return
fi

# Load git-prompt if found
F_git_prompt=1
if [ -f /usr/share/git/git-prompt.sh ]; then
  source /usr/share/git/git-prompt.sh
elif [ -f /etc/bash_completion.d/git-prompt ]; then
  source /etc/bash_completion.d/git-prompt;
else
  echo "[info] Unable to find a suitable git-prompt. (Did you have git installed? File a PR if it's in a different location.)"
  F_git_prompt=0
fi

# Setup bash completion
if [ -f /etc/bash_completion ]; then
  source /etc/bash_completion
fi

# Setup terminal prompt
if   [ $TERM == "emacs" ]; then
  PS1="emacs -> [\u@\H: \w]\n> ";
elif [ $TERM == "dumb" ]; then
  PS1="? -> [\u@\H: \w]\n> ";
else
  if [ $F_git_prompt -eq 1 ]; then
    GIT_PS1_SHOWUPSTREAM="auto"
    PS1='\e[0;36m\u@\h:\e[m\e[1;36m$(__git_ps1 "[%s]")\e[0;33m\w\$\e[m\n> '
  else
    PS1='\e[0;36m\u@\h:\e[m\e[1;36m\e[0;33m\w\$\e[m\n> '
  fi
  PS1="\[\033[G\]$PS1"
fi

# Bash aliases
alias l="ls -lah --color";
alias grep="grep --exclude=*~ --exclude=*# --color $@";
alias p="pushd"
alias o="popd"
alias d="dirs -v"
alias wget="wget --progress=bar"
alias mount-user="sudo mount -o gid=users,fmask=113,dmask=002"

# Setup default editor, browser, etc.
export EDITOR=ef
if [ `which google-chrome-unstable` ]; then
  export BROWSER=google-chrome-unstable
elif [ `which google-chrome` ]; then
  export BROWSER=google-chrome
elif [ `which chromium` ]; then
  export BROWSER=chromium
elif [ `which firefox` ]; then
  export BROWSER=firefox
elif [ `which conkeror` ]; then
  export BROWSER=conkeror
else
  echo "[info] No known browser found. (Did you install it? File a PR to add another browser.)"
fi

# Arch Linux solution to statup an ssh-agent if it isn't already running
#   - https://wiki.archlinux.org/index.php/SSH_keys#ssh-agent
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
   ssh-agent > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
   eval "$(<~/.ssh-agent-thing)"
fi

# stty -ixon
