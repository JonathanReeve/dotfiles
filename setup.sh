mkdir -p bak
mv ~/.bashrc bak/
mv ~/.tmux.conf bak/
mv ~/.inputrc bak/
mv ~/.gitconfig bak/
mv ~/.i3/config bak/
mv ~/.zshrc bak/
ln -s $PWD/.bashrc ~/.bashrc
ln -s $PWD/.tmux.conf-child ~/.tmux.conf
ln -s $PWD/.inputrc ~/.inputrc
ln -s $PWD/.gitconfig ~/.gitconfig
ln -s $PWD/.zshrc ~/.zshrc
source ~/.bashrc 

# Determine Linux version 
OS=$(lsb_release -si)
if [ $OS == "Ubuntu" ]
then 
	export INSTALL='sudo apt-get install' 
fi
if [ $OS == "Fedora" ] 
then 
	export INSTALL='sudo yum install' 
fi

#Install Essential Packages 
$INSTALL vim git 

# Install Other Packages
$INSTALL markdown pandoc php-codesniffer 

# set up vim environment
mv ~/.vim bak/
mv ~/.vimrc bak/
ln -s $PWD/.vim ~/.vim
ln -s $PWD/.vimrc ~/.vimrc
$INSTALL vim

# get vundle and other submodules
git submodule update --init --recursive
vim -c PluginInstall

# todo.txt
#mv scripts/todo/todo.cfg bak/
#ln -s $PWD/todo.cfg scripts/todo/todo.cfg 
#chmod +x scripts/todo-plugins/.todo.actions.d/birdseye

#Install ZSH
$INSTALL zsh
wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh
chsh -s `which zsh`
 
# Check for GUI
if [[ $DISPLAY ]]
then
	mv ~/.vimperatorrc bak/
	mv ~/.pentadactylrc bak/
	mv ~/.gvimrc bak/
	mv ~/.i3/config bak/
	ln -s $PWD/.vimperatorrc ~/.vimperatorrc
	ln -s $PWD/.pentadactylrc ~/.pentadactylrc
	ln -s $PWD/.gvimrc ~/.gvimrc
	ln -s $PWD/.i3/config ~/.i3/config
	$INSTALL i3 kupfer chromium-browser vim-gnome ttf-anonymous-pro
fi 
#sudo apt-get install dwb
#ln -s $PWD/.dwb/* ~/.config/dwb/
