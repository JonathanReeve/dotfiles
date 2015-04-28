" This is my .vimrc!  " Jonathan Reeve
" http://jonreeve.com
" http://github.com/JonathanReeve/

" Plugins {{{

" Package Management. {{{2
"Vundle Stuff
set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Plugin 'gmarik/vundle' 
" }}} 

" Notes {{{2
" Required by vim-notes
Plugin 'xolox/vim-misc' 
" Essential notes plugin.  
"Plugin 'xolox/vim-notes' 
" Using my fork instead. 
Plugin 'JonathanReeve/vim-notes'
" To set notes directory for :Note command
let g:notes_directories = ['~/Dropbox/Notes']
" This ensures that updating vim notes won't overwrite my blank default note. 
let g:notes_shadowdir = '~/.vim/notes-shadow/' 
"set file extension for notes (notes.vim plugin)
let g:notes_suffix = '.txt'
" Turn off notes.vim highlighting for vim command syntax
highlight link notesVimCmd Normal

" Vim outliner
Plugin 'VOoM' 
"}}}

" Git {{{2
" Git wrapper. 
Plugin 'tpope/vim-fugitive'

" Github issues!
Plugin 'jaxbot/github-issues.vim'
" Source Oauth Token from private file for Using Github Issues Plugin
if !empty(glob('~/.vim-private')) "if file exists 
	source ~/.vim-private
endif
" }}}

" Browsers {{{2
" File browser. 
Plugin 'scrooloose/nerdtree'
"Open nerdtree
map ,n :NERDTreeToggle<CR>

" Buffer browser. 
Plugin 'techlivezheng/vim-plugin-minibufexpl' 
"Open minibufexplorer
map ,m :MBEToggle<CR>

" Tag browser.
Plugin 'majutsushi/tagbar' 
" Make Tagbar look like NERDTree. 
let g:tagbar_iconchars = ['▸', '▾']
highlight link TagbarFoldIcon Title
"Open Tagbar
map ,t :TagbarToggle<CR>

"Open Quickfix List
map ,c :cw<CR>

"Open debug log
map \d :sp $W/debug.log<CR>

"Open todo file
map \t :sp ~/Notes/todo.txt<CR>

"Open vimrc 
map \v :sp ~/.vimrc<CR>

" }}} 

" Colors {{{2
" Makes colors work in term. 
" Plugin 'godlygeek/csapprox'
" Another script for trying to make colors work better in term.
"Plugin 'vim-scripts/colorsupport.vim' 
Plugin 'vim-scripts/ScrollColors'
" Colors. 
"Plugin 'Colour-Sampler-Pack' 
"Plugin '29decibel/codeschool-vim-theme'  
Plugin 'ryu-blacknd/vim-nucolors' 
"Plugin 'Lokaltog/vim-distinguished' 
"Plugin 'whatyouhide/vim-gotham'
Plugin 'chriskempson/base16-vim' 
"Plugin 'tomasr/molokai'
Plugin 'JonathanReeve/vim-colorschemes'
"Plugin 'xolox/vim-colorscheme-switcher'
Plugin 'ajh17/Spacegray.vim' 
" }}}

" HTML/CSS Plugins Etc {{{2
"For better CSS
Plugin 'JulesWang/css.vim' 
"For Compass/SCSS/Sass
Plugin 'cakebaker/scss-syntax.vim'
" For writing in Pandoc markdown
"Plugin 'vim-pandoc/vim-pandoc' 
" HTML Authoring Autocompletion 
Plugin 'mattn/emmet-vim' 
" Improved matching for html tags
Plugin 'tmhedberg/matchit'
" HTML Tag Matching
" Plugin 'Valloric/MatchTagAlways'
"For autocomplete and faster html typing
"Plugin 'garbas/vim-snipmate' 

" }}}

" IDE Stuff {{{2
" Php folding
"Plugin 'rayburgemeestre/phpfolding.vim' 
"PHP error checking
"Plugin 'joonty/vim-phpqa' 
"PHP xdebug integration
"Plugin 'joonty/vdebug' 
"All kinds of syntax checking
"Plugin 'scrooloose/syntastic'
" Syntastic Options
let g:syntastic_php_checkers = ['php', 'phpcs'] 
let g:syntastic_php_phpcs_args = '--standard=/home/jreeve/Documents/WordPress-Coding-Standards/WordPress/ruleset.xml'
let g:syntastic_javascript_checkers = ['jshint'] 
map ,s :SyntasticToggleMode<CR>

"PHP IDE
"Plugin 'spf13/PIV'

" PHP Manual
"Plugin 'alvan/vim-php-manual'
" }}}

"" Neocomplete {{{2

""Plugin 'Shougo/neocomplete' 
""Plugin 'Shougo/neocomplcache' 

"" Use neocomplcache.
"let g:neocomplcache_enable_at_startup = 1
"" Use smartcase.
"let g:neocomplcache_enable_smart_case = 1
"" Set minimum syntax keyword length.
"let g:neocomplcache_min_syntax_length = 3
"let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

"" Enable heavy features.
"" Use camel case completion.
""let g:neocomplcache_enable_camel_case_completion = 1
"" Use underbar completion.
""let g:neocomplcache_enable_underbar_completion = 1

"" Define dictionary.
"let g:neocomplcache_dictionary_filetype_lists = {
    "\ 'default' : '',
    "\ 'vimshell' : $HOME.'/.vimshell_hist',
    "\ 'scheme' : $HOME.'/.gosh_completions'
	"\ }

"" Define keyword.
"if !exists('g:neocomplcache_keyword_patterns')
    "let g:neocomplcache_keyword_patterns = {}
"endif
"let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

"" Plugin key-mappings.
"inoremap <expr><C-g>     neocomplcache#undo_completion()
"inoremap <expr><C-l>     neocomplcache#complete_common_string()

"" Recommended key-mappings.
"" <CR>: close popup and save indent.
"inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
"function! s:my_cr_function()
  "return neocomplcache#smart_close_popup() . "\<CR>"
  "" For no inserting <CR> key.
  ""return pumvisible() ? neocomplcache#close_popup() : "\<CR>"
"endfunction
"" <TAB>: completion.
"inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
"" <C-h>, <BS>: close popup and delete backword char.
"inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
"inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
"inoremap <expr><C-y>  neocomplcache#close_popup()
"inoremap <expr><C-e>  neocomplcache#cancel_popup()
"" Close popup by <Space>.
""inoremap <expr><Space> pumvisible() ? neocomplcache#close_popup() : "\<Space>"

"" For cursor moving in insert mode(Not recommended)
""inoremap <expr><Left>  neocomplcache#close_popup() . "\<Left>"
""inoremap <expr><Right> neocomplcache#close_popup() . "\<Right>"
""inoremap <expr><Up>    neocomplcache#close_popup() . "\<Up>"
""inoremap <expr><Down>  neocomplcache#close_popup() . "\<Down>"
"" Or set this.
""let g:neocomplcache_enable_cursor_hold_i = 1
"" Or set this.
""let g:neocomplcache_enable_insert_char_pre = 1

"" AutoComplPop like behavior.
""let g:neocomplcache_enable_auto_select = 1

"" Shell like behavior(not recommended).
""set completeopt+=longest
""let g:neocomplcache_enable_auto_select = 1
""let g:neocomplcache_disable_auto_complete = 1
""inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"

"" Enable omni completion.
"autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
"autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
"autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
"autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
"autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

"" Enable heavy omni completion.
"if !exists('g:neocomplcache_force_omni_patterns')
  "let g:neocomplcache_force_omni_patterns = {}
"endif
"let g:neocomplcache_force_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
"let g:neocomplcache_force_omni_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
"let g:neocomplcache_force_omni_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

""  }}} 

" Misc {{{2
" Allows for opening of URLs and other files. 
Plugin 'xolox/vim-shell'
" For text surrounds like tags and quotes
Plugin 'tpope/vim-surround'
" For Easy Commenting
Plugin 'scrooloose/nerdcommenter'
" tail -f Emulation for Watching Files
"Plugin 'vim-scripts/Tail-Bundle' 

"A bunch of mappings that do cool stuff
Plugin 'tpope/vim-unimpaired' 



"Grepping Stuff
Bundle 'mileszs/ack.vim' 
" Better grepping with Ack (grep-ack)
map ,a :Ack <C-R><C-W><CR>

" }}}

"" Fancy Statusline {{{ 

""A lighter-weight statusline
"Plugin 'bling/vim-airline'
"" Next config setting needed for airline
"set laststatus=2

"if !exists('g:airline_symbols')
	"let g:airline_symbols = {}
"endif

"let g:airline_left_sep = '»'
"let g:airline_right_sep = '«'
"let g:airline_symbols.branch = '⎇'

"" Tmuxline
""Bundle 'edkolev/tmuxline.vim' 
""let g:tmuxline_powerline_separators = 0

"" End Airline }}} 

filetype plugin indent on     " required! 

" End Plugins }}} 

" Basic Settings {{{
set nocompatible
syntax on

" Turn off case-sensitive searches and things
set ignorecase

" Sets swap directory to ~/.swap
set directory=~/.vim/swap
"set backup
"set backupdir=~/.vim/backup

" Enable persistent undo
"set undofile
"set undodir=$HOME/.vim/undo

" Set location of tags file
set tags=~/app/tags

" Get vim increment to behave normally
set nrformats-=octal

" Sets Color Scheme
" Makes colors work in terminal
"set t_Co=256
"colorscheme gotham
"colorscheme base16-ocean
colorscheme nucolors
set background=dark 
highlight Normal ctermbg=NONE
highlight nonText ctermbg=NONE

" Highlight cursor depending on mode
au InsertLeave * hi Cursor guibg=red
au InsertEnter * hi Cursor guibg=green

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

" Password File and Journal File {{{
" Use Strong Encryption
"set cm=blowfish
" Makes password file quit automagically after five minutes 
autocmd BufReadPost,FileReadPost   .p10.txt set updatetime=300000
autocmd CursorHold                 .p10.txt wq

"Makes journal file quit automagically after five minutes
autocmd BufReadPost,FileReadPost   .jnl.txt set updatetime=300000
autocmd CursorHold                 .jnl.txt wq 
autocmd CursorHoldI                .jnl.txt wq
"}}}

" Syntax higlighting. {{{ 
" syntax highlighting for wordpress debug.log  
au BufRead,BufNewFile debug.log setfiletype debuglog
" }}} 

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
" map ,d :!wn <cword> -over<CR>

" Wordnet thesaurus
" map ,t :!wn <cword> -synsa<CR>

" Type ,b to load current page in browser
map ,b :!firefox "%:p"<CR>

" For todo items that look like [ ], fill in x and insert datestamp
map ,x 0f[lrxll"=strftime("%m%d %H:%M")." "<CR>p

" Allows you to press ,q to surround the line in quotes and press ,' from
" within a quoted passage to change it from double quotes to single quotes,
" and vice versa
map ,q ^i“<ESC>A”
map ,' cs"'
map ," cs'"

" Mapping for editing vimrc
"map ,v :sp ~/.vimrc<CR>

" mapping to count the number of words in a fold section
map ,c [zjv]zg<C-g>

"Git add and commit current file (uses Fugitive) 
"map ,g :w<CR>:Git add %<CR><CR>:Gcommit<CR>i

" Vimgrep word under cursor and open quicklist " market
map ,gc :vimgrep /<C-R><C-W>/gj **/*.css<CR>:cw<CR> 
map ,gp :vimgrep /<C-R><C-W>/gj **/*.php<CR>:cw<CR> 

"Press F4 to search for tags
map <F4> :execute "vimgrep /" . expand("<cword>") . "/j ~/Notes/*" <Bar> cw<CR> 

"Php linting
map ,p :!php -l %<CR>

"Turn on tag completion for XML and HTML
imap ,/ </<C-X><C-O>    

"Now pressing Tab allows you to switch between open windows
"map <Tab> <C-W><C-W>

"Ctrl-something for navigating split windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

"Open stuff
"nmap <CR> <F6>

" Press F8 to set showbreak
map <F8> :set breakindent<CR>:set showbreak=\ \ <CR>

"Press F7 to start Voom outliner mode
map <F7> :Voom markdown<CR>

"Space does the same thing as Ctrl+F
map <Space> <C-f>

"Yank current filename and line number
map \yy :let @" = expand("%")<CR>
map \yl :let @" = expand("%").":".line(".")<CR>
" open filename mentioned under cursor and navigate to line number mentioned
map \o Byt:f:l"1yw:e <C-r>0<CR>:<C-r>1<CR>

" open filename mentioned in debug.log and navigate to line number mentioned
" example error: 
" [11-Aug-2014 13:49:41 UTC] PHP Parse error:  syntax error, unexpected $end in 
" /vagrant/app/public/wp-content/plugins/buddypress-docs/includes/templates/docs/docs-loop.php on line 151
map \do 0/ in <CR>/app\/public<CR>"1yWW/\d<CR>"2yw:e ~/<C-r>1<CR>:<C-r>2<CR>

" loggy loggy: create log expression for variable under cursor
map \ll yiwo_log( '<C-o>p is:', $<C-o>p );<Esc>
" ss = 'spacey spacey.' Add spaces to parentheses to update code to WP coding
" standards. 
map \ss :s/(\([^ )]\)/( \1/g<CR>:s/\([^ (]\))/\1 )/g<CR>

" }}} 

" Language Specific Stuff {{{ 

" this sets tabstops to four spaces per python guidelines
autocmd Filetype python setlocal expandtab tabstop=4 shiftwidth=4
autocmd Filetype php setlocal noexpandtab
" }}} 

" Autocomplete and autoreplace {{{
" Autoreplace t4 with the therefore symbol
iabbrev t4 ∴

" Autoreplace 'teh' with 'the'
iabbrev teh the
iabbrev haev have
iabbrev liek like
iabbrev langauges languages
iabbrev langauge language

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

" Misc {{{
" }}}

" Allows for folding in this file. 
" vim:fdm=marker
