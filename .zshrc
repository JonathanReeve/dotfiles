# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="juanghurtado"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git common-aliases debian dirhistory extract fasd python vagrant vi-mode web-search)

# User configuration

export PATH="/home/jreeve/dotfiles/scripts:/usr/local/heroku/bin:/home/jreeve/.nvm/v0.10.33/bin:/home/jreeve/.npm-packages/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#

# Get history search working again
bindkey "^R" history-incremental-search-backward 

#shortcuts for MLA servers
export WP=$HOME
export W=$WP/app/public/wp-content 
export P=$WP/app/public/wp-content/plugins
export T=$WP/app/public/wp-content/themes
export M=$T/cbox-mla
export B=$T/cbox-mla-blog
export C=$T/cbox-theme
export A=$P/cbox-auth
export CA=$P/cac-advanced-profiles
export MA=$P/mla-admin-bar

# Get sensitive AWS vars from local file
[[ -f ~/.aws-vars.sh ]] && source ~/.aws-vars.sh 

# Check whether in graphical environment or not, 
# and if so, open gvim instead of vim
#[[ $DISPLAY = ":0" ]] && alias vim='gvim'

# Todo File Shortcut
alias t='~/Documents/Settings/dotfiles/scripts/todo/todo.sh -d ~/Documents/Settings/dotfiles/todo.cfg'

#opens Journal
alias jnl='vim ~/Dropbox/Personal/.jnl.txt'

#opens password file
alias pw='vim ~/Dropbox/Personal/.p10.txt'

#google calendar alias, requires googlecl
alias gc='google calendar add'

#python alias
alias py='python3'

#makes find commmand more useful
f() { find . -iname *"$1"* }

#dictionary hack
d() { wn "$1" -over |tee -a ~/Notes/vocab }

#searches vocab file
v() { grep "$1" ~/Notes/vocab }

# Opens a note
n() { gvim note:"$*" } 

## New Note: calls vim notes plugin
nn() { gvim -c :Note }

# Searches Notes
nls() { ls -c ~/Notes/ | egrep -i "$*" }
