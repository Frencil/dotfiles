HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=50000
setopt PROMPT_SUBST
setopt appendhistory autocd extendedglob
setopt autopushd pushdminus
unsetopt beep
bindkey -e

zstyle :compinstall filename '/home/brennen/.zshrc'

autoload -Uz compinit
compinit

# bpb fucking around below this line, kind of stoned
# some stuff from:
# http://stuff.mit.edu/~jdong/misc/zshrc

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
  'chris-dev')
    PR_HOST_COLOR=$PR_GREEN;
    ;;
  'hyakutake')
    PR_HOST_COLOR=$PR_LIGHT_BLUE;
    ;;
  'beta')
    PR_HOST_COLOR=$PR_BLUE;
    ;;
  'spino')
    PR_HOST_COLOR=$PR_RED;
    ;;
  'seismo')
    PR_HOST_COLOR=$PR_RED;
    ;;
  *)
    PR_HOST_COLOR=$PR_BLUE;
    ;;
esac

# set up ze prompt
export PS1="$PR_HOST_COLOR%n@%m $PR_CYAN%* $PR_YELLOW%d $(parse_git_branch) $PR_NO_COLOR$ "

# common aliases for both shells i actually use
source ~/.aliases

