export DOTFILES=~/Documents/Settings/dotfiles
source $DOTFILES/scripts/zgen/zgen.zsh 

# -- Plugins -- 
zgen oh-my-zsh
zgen oh-my-zsh plugins/git
zgen oh-my-zsh plugins/common-aliases
zgen oh-my-zsh plugins/colorize
zgen oh-my-zsh plugins/extract
zgen oh-my-zsh plugins/vagrant
zgen oh-my-zsh plugins/vi-mode
zgen load zsh-users/zsh-syntax-highlighting

zgen load zsh-users/zsh-completions src

# -- Theme -- 
zgen oh-my-zsh themes/juanghurtado
#zgen load caiogondim/bullet-train-oh-my-zsh-theme 
zgen load caiogondim/bullet-train-oh-my-zsh-theme 

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

# Aliases for package management in Ubuntu and Fedora 
OS=$(lsb_release -si)
if [[ $OS == "Ubuntu" ]] 
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
	alias update='sudo pacman -Syu'
fi

# -- Shortcuts -- 
export WP=$HOME
export V=~/Documents/commons-playbooks
export W=$WP/app/public/wp-content 
export P=$WP/app/public/wp-content/plugins
export T=$WP/app/public/wp-content/themes
export M=$T/cbox-mla
export B=$T/cbox-mla-blog
export C=$T/cbox-theme
export A=$P/cbox-auth
export CA=$P/cac-advanced-profiles
export MA=$P/mla-admin-bar

# -- Context-Dependent Aliases -- 
# Get sensitive AWS vars from local file
if [[ -f ~/.aws-vars.sh ]] 
then 
	source ~/.aws-vars.sh 
fi 

# Aliases for graphical environments. 
# (Assumes GNOME is installed.)  
if [[ $DISPLAY = ":0" ]] 
then 
	alias edit='gvim'
	alias open='gnome-open' 
fi

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
	export EDITOR='vim'
else
	export EDITOR='gvim'
fi

# -- Aliases --

# Extra Git Aliases
alias gcm='git commit -m'
compdef _git gcm=git-commit

alias gcam='git commit -a -m'
compdef _git gcam=git-commit

# Todo File Shortcut
alias t='$DOTFILES/scripts/todo/todo.sh -d $DOTFILES/todo.cfg'

#opens Journal
alias jnl='vim ~/Dropbox/Personal/.jnl.txt'

#opens password file
alias pw='vim ~/Dropbox/Personal/.p10.txt'

#google calendar alias, requires googlecl
alias caladd='google calendar add'

#python alias
alias py='python3'

#makes find commmand more useful
f() { find . -iname *"$1"* }

#dictionary hack
#d() { wn "$1" -over |tee -a ~/Notes/vocab }

#searches vocab file
#v() { grep "$1" ~/Notes/vocab }

# Opens a note
n() { gvim note:"$*" } 

## New Note: calls vim notes plugin
nn() { gvim -c :Note }

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

# -- PATH -- 
export PATH="/home/jreeve/dotfiles/scripts:/usr/local/heroku/bin:/home/jreeve/.nvm/v0.10.33/bin:/home/jreeve/.npm-packages/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
# export MANPATH="/usr/local/man:$MANPATH"

export NODE_PATH=/home/jreeve/.nvm/v0.10.33/lib/node_modules:/home/jreeve/.npm-packages/lib/node_modules
