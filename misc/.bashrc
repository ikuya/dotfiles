## Alias

alias ls='ls -F'
alias lsa='ls -AF'
alias ll='ls -lF'
alias lla='ls -AlF'
alias llt='ls -tlF'
alias sl='ls'
alias ..='cd ../'
alias tm='tmux'
alias tms='tmux ls'
alias tma='tmux attach'
alias changekey-tmux='tmux set-option -t 0 prefix C-z'
alias revertkey-tmux='tmux set-option -t 0 prefix C-t'
alias g='git'
alias gst='git status'
alias gd='git diff'

## Full-text search
function search() {
    dir=.
    file=*
    case $# in
        0)
            echo 'usage: search [DIR [FILE]] STRING'
            return 1
        ;;
        1)
            string=$2
        ;;
        2)
            string=$2
            dir=$1
        ;;
        3)
            string=$3
            dir=$1
            file=$2
        ;;
    esac
    find $dir -name "$file" -exec grep -iIHn $string {} \; 2>/dev/null;
}

## Editor
export EDITOR=$(which vi)

## Lang
export LANG=en_US.UTF-8
