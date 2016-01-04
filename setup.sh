mkdir -p bak
mv ~/.bashrc bak/
mv ~/.tmux.conf bak/
mv ~/.inputrc bak/
mv ~/.gitconfig bak/
mv ~/.i3/config bak/
mv ~/.zshrc bak/
mv ~/.config/uzbl/config bak/
ln -s $PWD/.bashrc ~/.bashrc
ln -s $PWD/.inputrc ~/.inputrc
ln -s $PWD/.gitconfig ~/.gitconfig
ln -s $PWD/.zshrc ~/.zshrc
ln -s $PWD/.uzbl-config ~/.config/uzbl/config
source ~/.bashrc 

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]
then 
	ln -s $PWD/.tmux.conf-child ~/.tmux.conf
else
	ln -s $PWD/.tmux.conf-parent ~/.tmux.conf
fi

# Determine Linux version 
if [ -f "/etc/arch-release" ]; then
	OS="Arch"
else
	OS=$(lsb_release -si)
fi

# Get Linux install commands. 
if [ $OS == "Ubuntu" ] || [ $OS == "Debian" ]
then 
	export INSTALL='sudo apt-get install' 
fi
if [ $OS == "Fedora" ] 
then 
	export INSTALL='sudo yum install' 
fi
if [ $OS == "Arch" ] 
then
	export INSTALL='sudo pacman -S'
fi

# DOTFILES environment variable needed by .zshrc
export DOTFILES=$PWD

#Install Essential Packages
$INSTALL vim git zsh

chsh -s /bin/zsh

# Install Other Packages
$INSTALL markdown pandoc

# Set up vim environment
mv ~/.vim bak/
mv ~/.vimrc bak/
ln -s $PWD/.vim ~/.vim
ln -s $PWD/.vimrc ~/.vimrc
$INSTALL vim

# Set up neovim
mkdir -p ${XDG_CONFIG_HOME:=$HOME/.config}
ln -s ~/.vim $XDG_CONFIG_HOME/nvim
ln -s ~/.vimrc $XDG_CONFIG_HOME/nvim/init.vim
ln -s ~/.vimrc $XDG_CONFIG_HOME/nvim/init.vim
sudo ln -s $HOME/.vimrc /home/root/.vimrc
sudo ln -s /home/root/.vimrc /home/.config/nvim/init.vim

# Get vundle and other submodules
git submodule update --init --recursive
vim -c PluginInstall

# todo.txt
#mv scripts/todo/todo.cfg bak/
#ln -s $PWD/todo.cfg scripts/todo/todo.cfg 
#chmod +x scripts/todo-plugins/.todo.actions.d/birdseye

# Install ZSH
$INSTALL zsh
chsh -s `which zsh`
 
# Check for GUI
if [[ $DISPLAY ]]
then
	mv ~/.vimperatorrc bak/
	mv ~/.pentadactylrc bak/
	mv ~/.i3/config bak/
	ln -s $PWD/.vimperatorrc ~/.vimperatorrc
	ln -s $PWD/.pentadactylrc ~/.pentadactylrc
	ln -s $PWD/.i3/config ~/.i3/config
	$INSTALL i3
	#neovim stuff
	$INSTALL neovim python-neovim python2-neovim xsel 
fi 
#sudo apt-get install dwb
#ln -s $PWD/.dwb/* ~/.config/dwb/
