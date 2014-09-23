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
  'chris-dev')
    PR_HOST_COLOR=$PR_CYAN;
    ;;

  # Personal machines
  'kodama')
    PR_HOST_COLOR=$PR_GREEN;
    ;;
  'Okkoto')
    PR_HOST_COLOR=$PR_GREEN;
    ;;
  'kamajii')
    PR_HOST_COLOR=$PR_GREEN;
    ;;

  # Beta environments
  'beta')
    PR_HOST_COLOR=$PR_BLUE;
    ;;

  # Production environments
  'spino')
    PR_HOST_COLOR=$PR_RED;
    ;;
  'seismo')
    PR_HOST_COLOR=$PR_RED;
    ;;
  'ps266333')
    PR_HOST_COLOR=$PR_RED;
    ;;
  *)

  # Default
    PR_HOST_COLOR=$PR_WHITE;
    ;;

esac

# set up prompt
export PS1="$PR_HOST_COLOR%n@%m $PR_CYAN%* $PR_YELLOW%d $(parse_git_branch) $PR_NO_COLOR$ "

# common shell aliases
source ~/.aliases

