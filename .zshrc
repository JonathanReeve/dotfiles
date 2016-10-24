# Don't auto-update oh-my-zsh;
# let zgen handle that.
DISABLE_AUTO_UPDATE=true

if [[ -n $SSH_CONNECTION ]]; then
	#I tend to keep dotfiles here in VMs
	export DOTFILES=~/dotfiles
else
	#...and here on my personal machines.
	export DOTFILES=~/Documents/Settings/dotfiles
fi

source $DOTFILES/scripts/zgen/zgen.zsh

# -- Plugins --
zgen oh-my-zsh
zgen oh-my-zsh plugins/autojump
zgen oh-my-zsh plugins/git
zgen oh-my-zsh plugins/common-aliases
zgen oh-my-zsh plugins/colorize
zgen oh-my-zsh plugins/jump
zgen oh-my-zsh plugins/extract
zgen oh-my-zsh plugins/vagrant
zgen oh-my-zsh plugins/vi-mode
zgen oh-my-zsh plugins/virtualenvwrapper
zgen load zsh-users/zsh-syntax-highlighting
zgen load zsh-users/zsh-completions src

# -- Theme --
#zgen oh-my-zsh themes/juanghurtado
zgen load caiogondim/bullet-train-oh-my-zsh-theme bullet-train

# -- Settings --

# Enable command auto-correction.
ENABLE_CORRECTION="true"

# Display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# SSH Keys
export SSH_KEY_PATH="~/.ssh/rsa_id"

# Get history search working again
bindkey "^R" history-incremental-search-backward

# Aliases for package management.
if [ -f "/etc/arch-release" ]; then
	OS="Arch"
else
	OS=$(lsb_release -si)
fi

if [[ $OS == "Ubuntu" ]] || [[ $OS == "Debian" ]]
then
	alias install='sudo apt-get install'
	alias search='apt-cache search'
	alias update='sudo apt-get update && sudo apt-get upgrade'
fi
if [[ $OS == "Fedora" ]]
then
	alias install='sudo yum install'
	alias search='yum search'
	alias update='sudo yum update'
fi
if [[ $OS == "Arch" ]]
then
	alias install='sudo pacman -S'
	alias search='pacman -Ss'
	alias update='yaourt -Syu --aur --noconfirm'
fi

# -- Shortcuts --

# If connected over SSH, this is probably a vagrant box.
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]
then
	export WP=/srv/www/commons/current
else
	export WP=$HOME/app
fi

alias edit='nvim'
export EDITOR='nvim'
alias open='gnome-open'

# -- Aliases --

# Extra Git Aliases

# `lg` is defined in .gitconfig
alias glg='git lg'
compdef _git glg=git-log

alias gcm='git commit -m'
compdef _git gcm=git-commit

alias gcam='git commit -a -m'
compdef _git gcam=git-commit

alias gcom='git checkout master'
compdef _git gcd=git-checkout

alias gcod='git checkout develop'
compdef _git gcod=git-checkout

# Git checkout a new branch
alias gcob='git checkout -b'
compdef _git gcob=git-checkout

# Todo File Shortcut
alias t='$DOTFILES/scripts/todo/todo.sh -d $DOTFILES/todo.cfg'
alias tb='t birdseye | less'
alias todo='edit ~/Dropbox/Personal/Todo/todo.txt'

#opens Journal
alias jnl='vim ~/Dropbox/Personal/.jnl.txt'

#opens password file
alias pw='vim ~/Dropbox/Personal/.p10.txt'

alias vault='encfs ~/Dropbox/Personal/.Vault_encfs ~/Documents/Settings/.private-mount'

alias unvault='fusermount -u ~/Documents/Settings/.private-mount'

#google calendar alias, requires googlecl
alias caladd="gcalcli --calendar 'jon.reeve@gmail.com' quick" 

#python alias
alias py='python3'

#makes find commmand more useful
f() { find . -iname "$1" }

#Use SSH on GitHub instead of HTTPs
alias git-ssh='git config url.ssh://git@github.com/.insteadOf https://github.com/'

#Smart rsync copy. (a)rchival, (i)temized, (b)ackup,
#(u) - only newer files, (z) compression, (P)artial and progress.
alias rsync-smart='rsync -abviuzP'

# makes find commmand more useful
f() { find . -iname *"$1"* }

# Open SSH sessions in new tmux window and connect to a nested tmux session.
function tmux-ssh () {
	tmux new-window -n "$1" """ssh -t "$1" '(command -v tmux >/dev/null 2>&1 && (tmux attach || tmux new-session -s ssh)) || bash -l'"""
}

#dictionary hack
#d() { wn "$1" -over |tee -a ~/Notes/vocab }

#searches vocab file
#v() { grep "$1" ~/Notes/vocab }

# Opens a note
n() { edit note:"$*" }

## New Note: calls vim notes plugin
nn() { edit -c :Note }

# Searches Notes
nls() { ls -c ~/Notes/ | egrep -i "$*" }

# Better grepping
a() { ack-grep -i "$1" * }

# Vim all the things!
alias :q='exit'
alias :wq='exit'
alias :e='edit'

# Change GitHub URLs to SSH
alias git-ssh='git config url.ssh://git@github.com/.insteadOf https://github.com/'

tagwatch() { 
	while inotifywait -e close_write "$1"
	do 
		ctags "$1"
	done
} 
# -- Colorized Man Pages --

man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
    LESS_TERMCAP_md=$'\E[01;38;5;74m' \
    LESS_TERMCAP_me=$'\E[0m' \
    LESS_TERMCAP_se=$'\E[0m' \
    LESS_TERMCAP_so=$'\E[38;5;246m' \
    LESS_TERMCAP_ue=$'\E[0m' \
    LESS_TERMCAP_us=$'\E[04;38;5;146m' \
    man "$@"
}

# -- PATH --
export PATH="$DOTFILES/scripts:/home/jon/Dropbox/Settings/scripts:/usr/local/heroku/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/jon/.gem/ruby/2.3.0/bin:/home/jon/.cabal/bin:/home/jon/.local/bin:/opt/android-sdk/platform-tools:/usr/bin/core_perl"

# export MANPATH="/usr/local/man:$MANPATH"

export NODE_PATH=/home/jreeve/.nvm/v0.10.33/lib/node_modules:/home/jreeve/.npm-packages/lib/node_modules

export GEM_HOME=$HOME/.gem

# Set default browser for Jupyter etc. 
export BROWSER='firefox'

export WORKON_HOME=~/.virtualenvs

export BULLETTRAIN_VIRTUALENV_PREFIX='venv:'

# If var is not set, set it. 
if [ -z ${DONETHISWEEK+x} ]
then 
	DONETHISWEEK=`donethisweek`
fi

# If var is not set, set it. 
if [ -z ${DONETODAY+x} ]
then 
	DONETODAY=`donetoday`
fi

if [ $DONETHISWEEK -gt 0 ]
then
	export BULLETTRAIN_CUSTOM_MSG="$DONETHISWEEK-$DONETODAY"
	BULLETTRAIN_CUSTOM_BG=green
fi
