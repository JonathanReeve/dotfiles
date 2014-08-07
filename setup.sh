mkdir bak
mv ~/.bashrc bak/
mv ~/.vim bak/
mv ~/.vimrc bak/
mv ~/.tmux.conf bak/
mv ~/.vimperatorrc bak/
mv ~/.pentadactylrc bak/
mv ~/.inputrc bak/
mv ~/.gitconfig bak/
ln -s $PWD/.bashrc ~/.bashrc
ln -s $PWD/.vim ~/.vim
ln -s $PWD/.vimrc ~/.vimrc
ln -s $PWD/.vimperatorrc ~/.vimperatorrc
ln -s $PWD/.pentadactylrc ~/.pentadactylrc
ln -s $PWD/.tmux.conf-child ~/.tmux.conf
ln -s $PWD/.inputrc ~/.inputrc
ln -s $PWD/.gitconfig ~/.gitconfig
sudo apt-get install vim-nox weechat markdown pandoc php-codesniffer
