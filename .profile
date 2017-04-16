# Variables and things, since emacs complains that they shouldn't
# be set in .zshrc, but in .profile. 

if [[ -n $SSH_CONNECTION ]]; then
	  #I tend to keep dotfiles here in VMs
	  export DOTFILES=~/dotfiles
else
	  #...and here on my personal machines.
	  export DOTFILES=~/Documents/Settings/dotfiles
fi

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

alias edit='emacsclient -c'
export EDITOR='emacsclient -c'
alias open='gvfs-open'

# -- PATH --
export PATH="$DOTFILES/scripts:/home/jon/Dropbox/Settings/scripts:/usr/local/heroku/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/jon/.gem/ruby/2.3.0/bin:/home/jon/.cabal/bin:/home/jon/.local/bin:/opt/android-sdk/platform-tools:/usr/bin/core_perl"

PATH="$HOME/.node_modules/bin:$PATH"
export npm_config_prefix=~/.node_modules

if which ruby >/dev/null && which gem >/dev/null; then
    PATH="$(ruby -rubygems -e 'puts Gem.user_dir')/bin:$PATH"
fi

export GEM_HOME=$HOME/.gem

# Set default browser for Jupyter etc. 
export BROWSER='qutebrowser'

export WORKON_HOME=~/.virtualenvs

export BULLETTRAIN_VIRTUALENV_PREFIX='venv:'

if [ $DONETHISWEEK -gt 0 ]
then
	  export BULLETTRAIN_CUSTOM_MSG="$DONETHISWEEK-$DONETODAY"
	  BULLETTRAIN_CUSTOM_BG=green
fi
