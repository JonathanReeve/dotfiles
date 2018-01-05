# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
#sets vi mode
set -o vi

#makes ls colorful
#alias ls="ls --color"
alias grep="grep --color" 

# Todo File Shortcut
alias t='~/Documents/Settings/dotfiles/scripts/todo/todo.sh -d ~/Documents/Settings/dotfiles/todo.cfg'

#opens Journal
alias jnl='vim ~/Dropbox/Personal/.jnl.txt'

#opens password file
alias pw='vim ~/Dropbox/Personal/.p10.txt'

#makes find commmand more useful
f() 
{ 
	find . -iname *"$1"*
}

#google calendar alias, requires googlecl
alias gc='google calendar add'

#python alias
alias py='python3'

#dictionary hack

d()
{
	wn "$1" -over |tee -a ~/Notes/vocab
}

#searches vocab file
v()
{
	grep "$1" ~/Notes/vocab
}

# Opens a note
n() {
        gvim note:"$*"
}

## New Note: calls vim notes plugin
nn() { 
	gvim -c :Note
}

# Searches Notes
nls() {
        ls -c ~/Notes/ | egrep -i "$*"
}

# Determine Linux version 
OS=$(lsb_release -si)
if [ $OS == "Ubuntu" ]
then 
	alias install='sudo apt-get install' 
	alias search='apt-cache search' 
	alias update='sudo apt-get update && sudo apt-get upgrade'
fi
if [ $OS == "Fedora" ] 
then 
	alias install='sudo yum install' 
	alias search='yum search' 
	alias update='sudo yum update'
fi

#for mounting dev directories over ssh
#alias dev="sshfs -oworkaround=rename jon:/home4/jonreeve/public_html/dev ~/Web/dev && cd ~/Web/dev && gvim ." 
#alias ccadev="sshfs -oworkaround=rename jon:/home4/jonreeve/public_html/cca ~/Web/dev && cd ~/Web/dev && gvim ." 
#alias wfdev="sshfs -oworkaround=rename wf:/home/jonreeve/webapps/cca/cca_project ~/Web/dev && cd ~/Web/dev && gvim ." 
#alias mladev="sshfs -oworkaround=rename mla:/ ~/Web/dev && cd ~/Web/dev && gvim ." 
#alias nodev="cd && fusermount -u ~/Web/dev" 

#I'm tired of CDing into this directory
#alias btc="cd ~/Dropbox/Apps/bitcoin-arbitrage/arbitrage/" 

#alias syncdb="python3 ~/Apps/cca-django/manage.py syncdb" 
#alias runserver="python3 ~/Apps/cca-django/manage.py runserver" 

#grep things better
g() { 
	grep -RiI --exclude-dir=blogs.dir --exclude-dir=uploads "$1" * 
} 

#adds script for doing that neat line across the terminal
#if [ -f "$HOME/.bash_ps1" ]; then
#. "$HOME/.bash_ps1"
#fi 

#get fancy prompt
#PS1="┌─[\d][\u@\h:\w]\n└─>" 

#my gpg key
export GPGKEY=4C9615CC

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

#use vim for the editor
export EDITOR='vim'

#better tmux/ssh integration
ssh() {
	tmux rename-window "$*"
	command ssh "$@"
	tmux rename-window "bash" 
}

#frees up Ctrl+S and Ctrl+Q from xon/xoff
stty -ixon -ixoff

# Fancy git statusline, stolen from https://github.com/chriszarate/dotfiles/blob/master/bash/bashrc
## Prompt
export PS1='┌─[\d][\u@\h:\w]\[\033[1;33m\]$(parse_git_status)\[\033[0m\]\n└─>'

# Change title of gnome-terminal to reflect current directory, etc. 
PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"' 

# Get git repository status in shorthand form (branch name and cleanliness).
function parse_git_status () {
	git status -b --porcelain 2>/dev/null | awk -F'[#./ ]+' '{print $2}' |
	while read statusline; do
		if [[ -n "$git_has_branch" && -n "$statusline" ]]; then
			printf "\033[35m⌾"
			break
		fi
		printf "⎇ $statusline" && git_has_branch="yes"
	done
}

# Stuff to get local Node.js commands to work 
NPM_PACKAGES="/home/jreeve/.npm-packages"
NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
PATH="$NPM_PACKAGES/bin:$PATH"
# Unset manpath so we can inherit from /etc/manpath via the `manpath`
# command
unset MANPATH  # delete if you already modified MANPATH elsewhere in your config
MANPATH="$NPM_PACKAGES/share/man:$(manpath)"

export NVM_DIR="/home/jreeve/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# add my own scripts 
export PATH="$HOME/dotfiles/scripts:$PATH"
# >>> Added by cnchi installer
BROWSER=/usr/bin/chromium
EDITOR=/usr/bin/nano
