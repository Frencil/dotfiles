HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=50000
setopt PROMPT_SUBST
setopt appendhistory autocd extendedglob
setopt autopushd pushdminus
unsetopt beep
bindkey -e

autoload -Uz compinit
compinit

# grab some colors
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
  colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
  eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
  eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
  (( count = $count + 1 ))
done
PR_NO_COLOR="%{$terminfo[sgr0]%}"

function parse_git_branch {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "("${ref#refs/heads/}")"
}

case "$HOST" in

  # Dev environments
  'gfg-dev-chriclrk')
    PR_HOST_COLOR=$PR_CYAN;
    ;;
  'gfg-dev-kevinwli')
    PR_HOST_COLOR=$PR_CYAN;
    ;;

  # Personal machines
  'Okkoto')
    PR_HOST_COLOR=$PR_GREEN;
    ;;
  'Eboshi')
    PR_HOST_COLOR=$PR_GREEN;
    ;;
  'Ashitaka')
    PR_HOST_COLOR=$PR_GREEN;
    ;;

  # Beta / Staging environments
  'gfg-staging')
    PR_HOST_COLOR=$PR_BLUE;
    ;;

  # Production environments
  'gfg-app')
    PR_HOST_COLOR=$PR_RED;
    ;;
  'gfg-db-questionnaire')
    PR_HOST_COLOR=$PR_RED;
    ;;

  # Other environments
  'wonderland')
    PR_HOST_COLOR=$PR_YELLOW;
    ;;
  'gfg-data')
    PR_HOST_COLOR=$PR_YELLOW;
    ;;

  # Default
  *)
    PR_HOST_COLOR=$PR_WHITE;
    ;;

esac

# set up prompt
export PS1="$PR_HOST_COLOR%n@%m $PR_CYAN%* $PR_YELLOW%d $(parse_git_branch) $PR_NO_COLOR$ "

# bind up and down arrows for history search
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward

# load custom shell aliases
source ~/.aliases

# Python Virtualenv
export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh
