" This is my .vimrc! 
" Jonathan Reeve
" http://jonreeve.com
" http://github.com/JonathanReeve/

" Basic Settings {{{
set nocompatible
syntax on

" Turn off case-sensitive searches and things
set ignorecase

" Sets swap directory to ~/.swap
set directory=~/.vim/swap
set backupdir=~/.vim/backup

" Set location of tags file
set tags=~/.vim/tags/tags

" Makes vim color adapter thing work
set t_Co=256

" Get vim increment to behave normally
set nrformats-=octal

" Sets Color Scheme
" colorscheme desert
colorscheme jellybeans

"Makes Unicode Work
set encoding=utf-8

"Confirm saves rather than give errors
set confirm 

"Makes sure it doesn't add unnecessary line breaks
set textwidth=0
set wrapmargin=0 

" To make plugins work
filetype plugin on
filetype indent on
" }}} 

" Plugins {{{

"Vundle Stuff
set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle' 

" My bundles here: 

" Required by vim-notes
Bundle 'xolox/vim-misc' 
" Essential notes plugin.  
Bundle 'xolox/vim-notes' 
" Allows for opening of URLs and other files. 
Bundle 'xolox/vim-shell'
" For text surrounds like tags and quotes
Bundle 'tpope/vim-surround'
" Git wrapper. 
Bundle 'tpope/vim-fugitive'
" For Easy Commenting
Bundle 'scrooloose/nerdcommenter'
" File browser. 
Bundle 'scrooloose/nerdtree'
" Makes colors work in term. 
"Bundle 'godlygeek/csapprox'
" Pretty Colors
Bundle 'Colour-Sampler-Pack' 
" New Colorscheme
Bundle '29decibel/codeschool-vim-theme'  
" Another Colorscheme
Bundle 'Lokaltog/vim-distinguished' 
" Vim outliner
Bundle 'VOoM' 
"For autocomplete and faster html typing
"Bundle 'garbas/vim-snipmate' 
"For a pretty statusline
"Bundle 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}  
"PHP error checking
"Bundle 'joonty/vim-phpqa.git' 
"For better CSS
Bundle 'JulesWang/css.vim' 
" Autocompletion Magic
"Bundle 'Valloric/YouCompleteMe' 
" For writing in Pandoc markdown
Bundle 'vim-pandoc/vim-pandoc' 

filetype plugin indent on     " required! 

" }}} 

" Options for Vim Notes {{{
" To set notes directory for :Note command
let g:notes_directories = ['~/.vim/notes']

" This ensures that updating vim notes won't overwrite my blank default note. 
let g:notes_shadowdir = '~/.vim/notes-shadow/' 

"set file extension for notes (notes.vim plugin)
let g:notes_suffix = '.txt'

" Turn off notes.vim highlighting for vim command syntax
highlight link notesVimCmd Normal
"}}}

" Better word processing. Copy and paste. {{{
" Stuff to make vim useful as a word processor
setlocal formatoptions=l
set lbr
" Be able to use j and k to move within wrapped lines, too
map  j gj
map  k gk
setlocal smartindent
setlocal spelllang=en_us

" Makes for Regular-Style Copy and Paste to the System Clipboard
map <C-v> "+gp
map <C-c> "+y
map <C-x> "+x

" groovyness in Insert mode (lets you paste and keep on typing)
" This blows away i_CTRL-V though (see :help i_CTRL-V)
imap <C-v> <Esc><C-v>a 
" }}} 

" Useful mappings. {{{
" Show wrapped lines by indenting them three spaces (mostly used for notes)
" set showbreak=\ \ \ 

"Map :w to Ctrl+s to save files so I don't have to type :w all the time
map <C-s> :update<CR>

"Make double click toggle folds
nmap <2-LeftMouse> za 

" Wordnet lookup
map ,d :!wn <cword> -over<CR>

" Wordnet thesaurus
map ,t :!wn <cword> -synsa<CR>

" Type ,b to load current page in browser
map ,b :!firefox "%:p"<CR>

" Allows you to press ,q to surround the line in quotes and press ,' from
" within a quoted passage to change it from double quotes to single quotes,
" and vice versa
map ,q ^i“<ESC>A”
map ,' cs"'
map ," cs'"

"Open nerdtree
map ,n :NERDTreeToggle<CR>

" Mapping for editing vimrc
map ,v :e ~/.vimrc<CR>

" mapping to count the number of words in a fold section
map ,c [zjv]zg<C-g>

"Git add and commit current file (uses Fugitive) 
"map ,g :w<CR>:Git add %<CR><CR>:Gcommit<CR>i

" Vimgrep word under cursor and open quicklist " market
map ,gc :vimgrep /<C-R><C-W>/gj **/*.css<CR>:cw<CR> 
map ,gp :vimgrep /<C-R><C-W>/gj **/*.php<CR>:cw<CR> 

"Php linting
map ,p :!php -l %

"Turn on tag completion for XML and HTML
imap ,/ </<C-X><C-O>    

"Now pressing Tab allows you to switch between open windows
map <Tab> <C-W><C-W>

"Alt-something for navigating split windows
map ∆ <C-W>j
map ˚ <C-W>k
map ˙ <C-W>h
map ¬ <C-W>l

"Open stuff
"nmap <CR> <F6>

"Press F4 to search for tags
map <F4> :execute "vimgrep /" . expand("<cword>") . "/j ~/Notes/*" <Bar> cw<CR> 

" Press F8 to set showbreak
map <F8> :set breakindent<CR>:set showbreak=\ \ <CR>

"Press F7 to start Voom outliner mode
map <F7> :Voom markdown<CR>

"Space does the same thing as Ctrl+F
map <Space> <C-f>

"Yank current filename and line number
map \yy :let @" = expand("%")<CR>
map \yl :let @" = expand("%").":".line(".")<CR>

" }}} 

" Language Specific Stuff {{{ 

" this sets tabstops to four spaces per python guidelines
autocmd Filetype python setlocal expandtab tabstop=4 shiftwidth=4


" }}} 

" Autocomplete and autoreplace {{{
" Autoreplace t4 with the therefore symbol
iabbrev t4 ∴

" Autoreplace 'teh' with 'the'
iabbrev teh the

" Autocorrect spelling mistakes
" I put this in a function because it slowed down the load time too much

" fun! Ac()
"	source /home/jon/.vim/autocorrect.vim
" endfu
" }}} 

" Markdown fixes {{{
"Make vim recognize *.md files as markdown. No idea why this isn't default.  
autocmd BufRead *.mkd      set ai formatoptions=tcroqn2 comments=n:> ft=markdown
autocmd BufRead *.md       set ai formatoptions=tcroqn2 comments=n:> ft=markdown
autocmd BufRead *.markdown set ai formatoptions=tcroqn2 comments=n:> ft=markdown 
" }}}

" Allows for folding in this file. 
" vim:fdm=marker
