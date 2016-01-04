" This is my .vimrc!  " Jonathan Reeve

" http://github.com/JonathanReeve/

" Very Basics {{{ 

" Vim-only, not nvim. 
if !has('nvim')
	set nocompatible   " be iMproved
	set cm=blowfish2   " use strong encryption
	set encoding=utf-8 " unicode is not standard on vim
endif

" Nvim-only, not vim 
if has('nvim')
	" 24-bit true color!
	let $NVIM_TUI_ENABLE_TRUE_COLOR=1
	" Hack to get C-h working in neovim
	nmap <BS> <C-W>h
	" Use escape to get out of terminal mode.
	tnoremap <Esc> <C-\><C-n>
	tnoremap <C-h> <C-\><C-n><C-w>h
endif
" }}} 

" Plugins {{{

" Package Management. {{{2
"Vundle Stuff
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Plugin 'gmarik/vundle'
" }}}

" Prose Writing {{{2
" Required by vim-notes
Plugin 'xolox/vim-misc'
" Essential notes plugin.
Plugin 'xolox/vim-notes'
" Use my fork instead.
"Plugin 'JonathanReeve/vim-notes'
" To set notes directory for :Note command
let g:notes_directories = ['~/Dropbox/Notes']
" This ensures that updating vim notes won't overwrite my blank default note.
let g:notes_shadowdir = '~/.vim/notes-shadow/'
"set file extension for notes (notes.vim plugin)
let g:notes_suffix = '.txt'
" Turn off notes.vim highlighting for vim command syntax
highlight link notesVimCmd Normal
" Temporarily don't use curly quotes
" let g:notes_smart_quotes = 0

" Vim outliner
"Plugin 'VOoM'

" Distraction-Free Writing Mode
Plugin 'junegunn/goyo.vim' 
nnoremap <Leader>g :Goyo<CR>

" Dictionary
Plugin 'szw/vim-dict'

"}}}

" Git {{{2
" Git wrapper.
Plugin 'tpope/vim-fugitive'

" Github issues!
"Plugin 'jaxbot/github-issues.vim'
"" Source Oauth Token from private file for Using Github Issues Plugin
"if !empty(glob('~/.vim-private')) "if file exists
	"source ~/.vim-private
"endif
" }}}

" Browsers {{{2
" File browser.
Plugin 'scrooloose/nerdtree'
"Open nerdtree
map ,n :NERDTreeToggle<CR>

" Buffer browser.
"Plugin 'techlivezheng/vim-plugin-minibufexpl'
"Open minibufexplorer
"map ,m :MBEToggle<CR>

"" Tag browser.
"Plugin 'majutsushi/tagbar'
"" Make Tagbar look like NERDTree.
"let g:tagbar_iconchars = ['▸', '▾']
"highlight link TagbarFoldIcon Title
""Open Tagbar
"map ,t :TagbarToggle<CR>

"Open Quickfix List
map ,c :cw<CR>

"Open debug log
map \d :sp $W/debug.log<CR>

"Open vimrc
map \v :sp ~/.vimrc<CR>

" }}}

" Colors {{{2
Plugin 'vim-scripts/ScrollColors'
" Colors.
"Plugin '29decibel/codeschool-vim-theme'
"Plugin 'ryu-blacknd/vim-nucolors'
"Plugin 'Lokaltog/vim-distinguished'
Plugin 'whatyouhide/vim-gotham'
Plugin 'mhartington/oceanic-next'
Plugin 'zenorocha/dracula-theme', {'rtp': 'vim/'}
Plugin 'chriskempson/base16-vim'
"Plugin 'tomasr/molokai'
Plugin 'JonathanReeve/vim-colorschemes'
"Plugin 'xolox/vim-colorscheme-switcher'
"Plugin 'ajh17/Spacegray.vim'
" }}}

" Language-Specific Plugins {{{2
"For better CSS
"Plugin 'hail2u/vim-css3-syntax'
"For Compass/SCSS/Sass. Old.
"Plugin 'cakebaker/scss-syntax.vim'
" For real Sass
"Plugin 'tpope/vim-haml'
"For coffeescript
"Plugin 'kchmck/vim-coffee-script'

" For writing in Pandoc markdown
Plugin 'vim-pandoc/vim-pandoc'
Plugin 'vim-pandoc/vim-pandoc-syntax'

" HTML Authoring Autocompletion
Plugin 'mattn/emmet-vim'
" Improved matching for html tags
Plugin 'tmhedberg/matchit'
" HTML Tag Matching
Plugin 'Valloric/MatchTagAlways'
"For autocomplete and faster html typing
"Plugin 'garbas/vim-snipmate'
Plugin 'ivanov/vim-ipython'
" }}}

" IDE Stuff {{{2
"All kinds of syntax checking
Plugin 'scrooloose/syntastic'
" Syntastic Options
let g:syntastic_php_checkers = ['php', 'phpcs']
let g:syntastic_php_phpcs_args = '--standard=/home/jreeve/Documents/WordPress-Coding-Standards/WordPress/ruleset.xml'
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_xml_checkers = ['xmllint']
let g:syntastic_xml_xmlling_args = '--dtdvalid tei_all.dtd' 
map ,s :SyntasticToggleMode<CR>
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
"A bunch of mappings that do cool stuff
Plugin 'tpope/vim-unimpaired'
"Grepping Stuff
Plugin 'rking/ag.vim'

" Timer adapted from this StackOverflow answer: http://superuser.com/a/982728/83457 
function! s:Start()
    if exists('b:CountMinutesStart')
        echohl ERROR
        echomsg "Already counting."
        echohl NONE
        return
    endif

    echohl TODO
    echomsg "Counting started."
    echohl NONE
    let b:CountMinutesStart = localtime()
endfunction

function! s:Stop()
    if !exists('b:CountMinutesStart')
        echohl ERROR
        echomsg "Not counting."
        echohl NONE
        return -1
    endif

    let l:start = b:CountMinutesStart
    let l:end = localtime()
    unlet b:CountMinutesStart
    let l:elapsed = l:end - l:start

    echohl TODO
    echomsg "Elapsed time since start: " . s:Format(l:elapsed)
    echohl NONE

    return l:elapsed
endfunction

function! s:Format(seconds)
    let l:minutes = a:seconds / 60
    let l:seconds = a:seconds % 60
    return printf('time:%02d:%02d', l:minutes, l:seconds)
endfunction

function! s:InsertTime()
    let l:seconds = s:Stop()
    if l:seconds == -1
        return
    endif
    let l:line = getline('.')
    if l:line =~ 'time:\d\{2}:\d\{2}'
        let l:tmp = split(substitute(l:line, '.*time:\(\d\{2}\):\(\d\{2}\).*', '\1 \2', ''), ' ')
        let l:seconds = l:seconds + (l:tmp[0] * 60 + l:tmp[1])
        call setline('.', substitute(l:line, 'time:\d\{2}:\d\{2}', s:Format(l:seconds), ''))
    else
        exe 'normal A' . ' ' . s:Format(l:seconds)
    endif
endfunction

command! StartCounting call s:Start()
command! StopCounting call s:InsertTime()

nmap <silent> <leader>sc :StartCounting<cr>
nmap <silent> <leader>ec :StopCounting<cr>

" }}}

" Fancy Statusline {{{

""A lighter-weight statusline
Plugin 'bling/vim-airline'
set laststatus=2
"" Next config setting needed for airline

let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

"if !exists('g:airline_symbols')
	"let g:airline_symbols = {}
"endif

"let g:airline_left_sep = '»'
"let g:airline_right_sep = '«'
"let g:airline_symbols.branch = '⎇'

"" End Airline }}}

" Language Specific Stuff {{{

" this sets tabstops to four spaces per python guidelines
autocmd Filetype python setlocal expandtab tabstop=4 shiftwidth=4
autocmd Filetype php setlocal noexpandtab

"Plugin 'editorconfig/editorconfig-vim'

let g:EditorConfig_exclude_patterns = ['fugitive://.*']

" }}}

filetype plugin indent on     " required!

" End Plugins }}}

" Basic Settings {{{
syntax on

" Stop vim from making annoying error sounds
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set vb t_vb=

" Turn off case-sensitive searches and things
set ignorecase

" Sets swap directory to ~/.swap
set directory=~/.vim/swap
"set backup
"set backupdir=~/.vim/backup

" Enable persistent undo
set undofile
set undodir=$HOME/.vim/undo

" Get vim increment to behave normally
set nrformats-=octal

" Sets Color Scheme
" Makes colors work in terminal
"set t_Co=256
colorscheme OceanicNext
set background=dark
let g:airline_theme='oceanicnext'
"highlight Normal ctermbg=NONE
"highlight nonText ctermbg=NONE

" Highlight cursor depending on mode
au InsertLeave * hi Cursor guibg=red
au InsertEnter * hi Cursor guibg=green

"Confirm saves rather than give errors
set confirm

"Makes sure it doesn't add unnecessary line breaks
set textwidth=0
set wrapmargin=0

" To make plugins work
filetype plugin on
filetype indent on

"Keeps the cursor in the center of the screen.
set scrolloff=5
" }}}

" Personal Stuff {{{
" Use Strong Encryption. Only works on vim, not nvim.
if !has('nvim')
endif

" Makes password file quit automagically after five minutes
autocmd BufReadPost,FileReadPost   .p10.txt set updatetime=300000
autocmd CursorHold                 .p10.txt wq

"Makes journal file quit automagically after five minutes
autocmd BufReadPost,FileReadPost   .jnl.txt set updatetime=300000
autocmd CursorHold                 .jnl.txt wq
autocmd CursorHoldI                .jnl.txt wq

" Todo List
Plugin 'freitass/todo.txt-vim'
au BufRead,BufNewFile todo.txt setfiletype=todo
command! Todo edit ~/Dropbox/Personal/Todo/todo.txt

" Toggl Experiments
" Plugin 'termoshtt/toggl.vim'
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
set clipboard+=unnamedplus 
map <C-v> "+gp
map <C-c> "+y
map <C-x> "+x

" groovyness in Insert mode (lets you paste and keep on typing)
" This blows away i_CTRL-V though (see :help i_CTRL-V)
imap <C-v> <Esc><C-v>a
" }}}

" Useful mappings. {{{

"Map :w to Ctrl+s to save files so I don't have to type :w all the time
map <C-s> :update<CR>

"Make double click toggle folds
nmap <2-LeftMouse> za

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

"Ctrl-something for navigating split windows
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l

" Press F8 to set showbreak
map <F8> :set breakindent<CR>:set showbreak=\ \ <CR>

"Press F7 to start Voom outliner mode
map <F7> :Voom markdown<CR>

"Space does the same thing as Ctrl+F
nmap <Space> <C-f>

"Yank current filename and line number
map \yy :let @" = expand("%")<CR>
map \yl :let @" = expand("%").":".line(".")<CR>

" open filename mentioned under cursor and navigate to line number mentioned
map \o Byt:f:l"1yw:e <C-r>0<CR>:<C-r>1<CR>

map <Leader>w :Gwrite<CR>:Gcommit -m "

map <Leader>s :!sass-convert -F sass -T scss<CR>
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

" Enable this for profiling stuff, in case vim is being slow.
" Put it at the top of the file to enable profiling startup
" profile start profile.log
" profile func *
" profile file *

" Allows for folding in this file.
" vim:fdm=marker
