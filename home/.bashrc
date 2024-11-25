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
elif [ -f /usr/share/git-core/contrib/completion/git-prompt.sh ]; then
  source /usr/share/git-core/contrib/completion/git-prompt.sh
else
  echo "[info] Unable to find a suitable git-prompt. (Did you have git installed? File a PR if it's in a different location.)"
  F_git_prompt=0
fi

# Setup bash completion
if [ -f /etc/bash_completion ]; then
  source /etc/bash_completion
elif [ -f /etc/profile.d/bash_completion.sh ]; then
  source /etc/profile.d/bash_completion.sh
fi

BASE_PS1=
if [ $VIRTUAL_ENV_PROMPT ]; then
  BASE_PS1="(venv:$VIRTUAL_ENV_PROMPT) "
fi

# Setup terminal prompt
if   [ $TERM == "emacs" ]; then
  PS1="${BASE_PS1}emacs -> [\u@\H: \w]\n# ";
elif [ $TERM == "dumb" ]; then
  PS1="${BASE_PS1}? -> [\u@\H: \w]\n# ";
else
  if [ $F_git_prompt -eq 1 ]; then
    GIT_PS1_SHOWUPSTREAM="auto"
    PS1='${BASE_PS1}\e[0;36m\u@\h:\e[m\e[1;36m$(__git_ps1 "[%s]")\e[0;33m\w\$\e[m\n# '
  else
    PS1='${BASE_PS1}\e[0;36m\u@\h:\e[m\e[1;36m\e[0;33m\w\$\e[m\n# '
  fi
  PS1="${BASE_PS1}\[\033[G\]$PS1"
fi

# Bash aliases
alias l="ls -lah --color";
alias grep="grep --exclude=*~ --exclude=*# --color $@";
alias p="pushd"
alias o="popd"
alias d="dirs -v"
alias wget="wget --progress=bar"
alias mount-user="sudo mount -o gid=users,fmask=113,dmask=002"
alias vncviewer="vncviewer -FullScreenAllMonitors -SecurityTypes=TLSVnc -FullScreen"

# Setup default editor, browser, etc.
export EDITOR=ef
if [ `command -v google-chrome-unstable` ]; then
  export BROWSER=google-chrome-unstable
elif [ `command -v google-chrome` ]; then
  export BROWSER=google-chrome
elif [ `command -v chromium` ]; then
  export BROWSER=chromium
elif [ `command -v firefox` ]; then
  export BROWSER=firefox
elif [ `command -v conkeror` ]; then
  export BROWSER=conkeror
else
  echo "[info] No known browser found. (Did you install it? File a PR to add another browser.)"
fi

export PATH=$HOME/usr/bin:$PATH

export PATH=$HOME/.cabal/bin:$PATH

# Pip user install location
export PATH=$PATH:$HOME/.local/bin

# Ruby gems
export PATH=$HOME/bin:$PATH

# added by travis gem
[ -f /home/schuyler/.travis/travis.sh ] && source /home/schuyler/.travis/travis.sh

# Load other non-version controlled .bashrc files.  E.g., if there is
# per-machine configuration that doesn't make sense to publicly version control,
# then add this into additional files in `$HOME` named `.*.bashrc`.  These will
# then be automatically loaded.
for f in $(find ~ -maxdepth 1 -executable -type f -name '.*.bashrc'); do
    source $f || echo "Failed to run $f"
done
