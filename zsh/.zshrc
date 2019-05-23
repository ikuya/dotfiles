#=======
# .zshrc
#=======
# zsh startup files loading order
# log in shell: /etc/zshenv -> $ZDOTDIR/.zshenv -> /etc/zprofile -> $ZDOTDIR/.zprofile -> /etc/zshrc -> $ZDOTDIR/.zshrc -> /etc/zlogin -> $ZDOTDIR/.zlogin
# interactive shell: /etc/zshenv -> $ZDOTDIR/.zshenv -> /etc/zshrc -> $ZDOTDIR/.zshrc

## ----------
## General Settings
## ----------

autoload -Uz colors
autoload -Uz compinit
colors
compinit

## Key bind
bindkey -e
## Ignore ctrl-D
setopt ignore_eof
## cd history
setopt auto_pushd
## Command auto correct
#setopt correct
setopt noautoremoveslash
## Auto cd
setopt auto_cd
## auto complete
zstyle ':completion:*:default' menu select=1

## Prompt
PROMPT="%{${fg[green]}%}[%n@%m:%~]
%(!.#.$)%{${reset_color}%} "
PROMPT2="%{${fg[green]}%}%_> %{${reset_color}%}"
#SPROMPT="%{${fg[red]}%}correct: %R -> %r [nyae]? %{${reset_color}%}"

## Command history
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
setopt hist_ignore_all_dups # ignore duplication
setopt hist_ignore_space    # ignore command biginning with <Space>.
setopt share_history        # share command history data

## Historical backward/forward search with linehead string binded to ^P/^N
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

## Ignore C-s, C-q
setopt no_flow_control

## LANG
export LANGUAGE=en_US.UTF-8
export LC_ALL=$LANGUAGE

## Editor
export EDITOR=$(which vi)

## PATH
# path はPATHの内容と同期している配列変数。
# 末尾に(N-/)をつけると、存在しないパスの場合に空文字に置換される。
# echo ${(F)path} とやると配列要素を改行で連結するので見やすくなる
# 環境固有のPATHは zshenv に記載
path=(
    $HOME/bin(N-/)
    /usr/local/bin
    /anaconda3/bin
    $HOME/.emacs.d/.cask/bin(N-/)
    /usr/sbin
    /sbin
    $path
)

## ----------
## Alias
## ----------

## General
alias ls='ls -F'
alias lsa='ls -AF'
alias ll='ls -lF'
alias lla='ls -AlF'
alias llt='ls -tlF'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias ..='cd ../'
alias ..2='cd ../..'
alias ..3='cd ../../..'
alias ..4='cd ../../../..'
## Tmux
alias tm='tmux'
alias tms='tmux ls'
alias tma='tmux attach'
alias changekey-tmux='tmux set-option -t 0 prefix C-z'
alias revertkey-tmux='tmux set-option -t 0 prefix C-t'
## Misc
alias g='git'
alias vag='vagrant'
alias va='vagrant'
alias zshrc="vi $HOME/.zshrc"
alias psall='ps aux'
alias grep='grep --color=auto'

case ${OSTYPE} in
    darwin*)
    alias tmem='top -o mem'
    alias tcpu='top -o cpu'
    ;;
    linux*)
    alias tmem='top -o %MEM'
    alias tcpu='top -o %CPU'
    ;;
esac

## ----------
## Functions
## ----------

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
    find $dir -name "$file" -exec grep -iIHn --color=auto $string {} \; 2>/dev/null;
}

## Automatically rename tmux window using the current working directory.
function rename_tmux_window() {
   if [ $TERM = "screen" ]; then
       local current_path=`pwd | sed -e s/\ /_/g`
       local current_dir=`basename $current_path |cut -c 1-5`
       tmux rename-window $current_dir >/dev/null 2>&1
   fi
}
autoload -Uz add-zsh-hook
add-zsh-hook precmd rename_tmux_window

## ----------
## VCS
## ----------

## Gathering information from version control systems
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
precmd() {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
RPROMPT="%(v|%F{cyan}%1v%f|)"

## GPG key
export GPG_TTY=$(tty)

# --- cdr ---
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':completion:*' recent-dirs-insert both
zstyle ':chpwd:*' recent-dirs-max 500
zstyle ':chpwd:*' recent-dirs-default true
zstyle ':chpwd:*' recent-dirs-file "$HOME/.cache/shell/chpwd-recent-dirs"
zstyle ':chpwd:*' recent-dirs-pushd true

# --- zaw ---
# See. https://github.com/zsh-users/zaw
source $HOME/.zaw/zaw.zsh
bindkey '^X' zaw
bindkey '^R' zaw-history
bindkey '^X^D' zaw-cdr
