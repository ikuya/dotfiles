## .zshrc

autoload -U colors
autoload -U compinit

colors
compinit

# Key bind
bindkey -e

# Ignore ctrl-D
setopt ignore_eof

# cd history
setopt auto_pushd

# Command auto correct
#setopt correct
setopt noautoremoveslash

# Auto cd
setopt auto_cd

# Prompt
PROMPT="%{${fg[green]}%}[%n@%m:%~]
%(!.#.$)%{${reset_color}%} "
PROMPT2="%{${fg[green]}%}%_> %{${reset_color}%}"
#SPROMPT="%{${fg[red]}%}correct: %R -> %r [nyae]? %{${reset_color}%}"

## Alias
alias ls='ls -F'
alias lsa='ls -aF'
alias ll='ls -lFh'
alias lla='ls -alFh'
alias llt='ls -tlFh'
alias sl='ls'
alias ks='ls'
alias chx='chmod +x'
alias du='du -ch'
alias df='df -h'
alias vw='view'
alias cal='cal -m3'
alias sc='screen'
alias sls='screen -ls'
alias scx='screen -x'
alias tm='tmux'
alias tms='tmux ls'
alias tma='tmux attach'
alias ei='eijiro'
alias changekey-tmux='tmux set-option -t 0 prefix C-z'
alias revertkey-tmux='tmux set-option -t 0 prefix C-t'
# Git
alias gad='git add'
alias gco='git commit'
alias gst='git status'
alias gd='git diff'
alias gp='git pull'
alias gf='git fetch'
alias gdf='git diff FETCH_HEAD'
alias gmf='git merge FETCH_HEAD'

# Command history
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
setopt hist_ignore_dups # ignore duplication
setopt share_history    # share command history data

# Historical backward/forward search with linehead string binded to ^P/^N
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# PATH
PATH=$HOME/bin:/usr/local/bin:/usr/bin:/opt/local/bin:/opt/local/sbin:/sw/bin:/sw/sbin:/bin:/usr/sbin:/sbin:/usr/X11R6/bin:/Applications/android-sdk-mac_86/tools:$HOME/Library/Haskell/bin:$PATH

# LANG
export LANG=en_US.UTF-8


## Shell functions
# ful-text search
function search() {
    dir=.
    file=*
    case $# in
        0)
        echo usage: search STRING [DIR [FILE]]
        ;;
        1)
        string=$1
        ;;
        2)
        string=$1
        dir=$2
        ;;
        3)
        string=$1
        dir=$2
        file=$3
        ;;
    esac
    find $dir -name "$file" -exec grep -IHn $string {} \; ;
}

# Ignore C-s, C-q
setopt no_flow_control

# Environment variances
#export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk-1.7-i586/Contents/Home
export PYTHONSTARTUP=$HOME/.pythonstartup
export EDITOR=/usr/local/bin/vim
export HREF_DATADIR=/usr/local/share/href

# Gathering information from version control systems
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
precmd() {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
RPROMPT="%(v|%F{cyan}%1v%f|)"

# alc
function alc() {
    if [ $# != 0 ]; then
        w3m "http://eow.alc.co.jp/$*/UTF-8/?ref=sa" |more +30
    else
        w3m "http://www.alc.co.jp/"
    fi
}
