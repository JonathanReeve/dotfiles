#mkdir bak
mv ~/.bashrc bak/
mv ~/.vim bak/
mv ~/.vimrc bak/
mv ~/.tmux.conf bak/
mv ~/.inputrc bak/
mv ~/.gitconfig bak/
mv ~/.i3/config bak/
ln -s $PWD/.bashrc ~/.bashrc
ln -s $PWD/.vim ~/.vim
ln -s $PWD/.vimrc ~/.vimrc
ln -s $PWD/.tmux.conf-child ~/.tmux.conf
ln -s $PWD/.inputrc ~/.inputrc
ln -s $PWD/.gitconfig ~/.gitconfig
#sudo yum install vim weechat markdown pandoc php-codesniffer git 
#git submodule update --init --recursive

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
