# Start Debugging Mode
set -x

# Make a temporary backup location. 
mkdir /tmp/bak

for f in tmux.conf inputrc gitconfig zshrc vim vimrc
do 
	# Back up existing files, if any. 
	mv ~/.$f /tmp/bak
	# Symlink to their expected locations
	ln -s $PWD/.$f ~/.$f
done 

# Determine Linux version 
if [ -f "/etc/arch-release" ]; then
	OS="Arch"
else
	OS=$(lsb_release -si)
fi

# Get Linux install commands. 
if [ $OS == "Ubuntu" ] || [ $OS == "Debian" ]
then 
	INSTALL='sudo apt-get install'
	# Ubuntu doesn't have neovim yet, so we have to add it
	# from a PPA. 
	$INSTALL software-properties-common
	$INSTALL python-software-properties
	add-apt-repository ppa:neovim-ppa/unstable
	apt-get update
	$INSTALL neovim
fi
if [ $OS == "Arch" ] 
then
	INSTALL='sudo pacman -S'
	$INSTALL neovim python-neovim python2-neovim
fi

# DOTFILES environment variable needed by .zshrc
export DOTFILES=$PWD

#Install Essential Packages
$INSTALL vim git zsh

# Use zsh instead of BASH
chsh -s /bin/zsh

# Set up vim environment
mkdir -p $HOME/.vim/autoload
ln -s $PWD/scripts/vim-plug/plug.vim $HOME/.vim/autoload/plug.vim

# Set up neovim
mkdir -p ${XDG_CONFIG_HOME:=$HOME/.config}
ln -s ~/.vim $XDG_CONFIG_HOME/nvim
ln -s ~/.vimrc $XDG_CONFIG_HOME/nvim/init.vim
sudo ln -s $HOME/.vimrc /home/root/.vimrc
sudo ln -s /home/root/.vimrc /home/.config/nvim/init.vim

# Get submodules
git submodule update --init --recursive
vim -c PlugInstall

## Personal Stuff

# todo.txt
#mv scripts/todo/todo.cfg bak/
#ln -s $PWD/todo.cfg scripts/todo/todo.cfg 
#chmod +x scripts/todo-plugins/.todo.actions.d/birdseye

# Check for GUI
if [[ $DISPLAY ]]
then
	for f in vimperatorrc i3/config 
	do 
		mv ~/.$f bak/
		ln -s $PWD/.$f ~/.$f
	done
	$INSTALL i3
fi 

set +x # Stop debugging. 
