#mkdir bak
mv ~/.bashrc bak/
mv ~/.tmux.conf bak/
mv ~/.inputrc bak/
mv ~/.gitconfig bak/
mv ~/.i3/config bak/
ln -s $PWD/.bashrc ~/.bashrc
ln -s $PWD/.tmux.conf-child ~/.tmux.conf
ln -s $PWD/.inputrc ~/.inputrc
ln -s $PWD/.gitconfig ~/.gitconfig

source ~/.bashrc 

#sudo yum install vim weechat markdown pandoc php-codesniffer git 
sudo apt-get install weechat markdown pandoc php-codesniffer git 

# set up vim environment
mv ~/.vim bak/
mv ~/.vimrc bak/
ln -s $PWD/.vim ~/.vim
ln -s $PWD/.vimrc ~/.vimrc
sudo apt-get install vim

# get vundle and other submodules
git submodule update --init --recursive
vim -c PluginInstall

# todo.txt
#mv scripts/todo/todo.cfg bak/
#ln -s $PWD/todo.cfg scripts/todo/todo.cfg 

# Uncomment for GUI systems
#mv ~/.vimperatorrc bak/
#mv ~/.pentadactylrc bak/
#ln -s $PWD/.vimperatorrc ~/.vimperatorrc
#ln -s $PWD/.pentadactylrc ~/.pentadactylrc
#mv ~/.i3/config bak/
#ln -s $PWD/.i3/config ~/.i3/config
#sudo yum install i3 kupfer chromium-browser vim-gnome ttf-anonymous-pro

#sudo apt-get install dwb
#ln -s $PWD/.dwb/* ~/.config/dwb/
