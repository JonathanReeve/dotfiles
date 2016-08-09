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

call plug#begin('~/.vim/plugged')

" Prose Writing {{{2

Plug 'kana/vim-textobj-user'    " Needed by vim-textobj-quote
Plug 'reedes/vim-textobj-quote' " For curly quotes. 
let g:textobj#quote#matchit = 1 " Turn on matching of curly quotes. Requires matchit. 

map <silent> <leader>qc <Plug>ReplaceWithCurly
map <silent> <leader>qs <Plug>ReplaceWithStraight

augroup textobj_quote
  autocmd!
  autocmd FileType markdown call textobj#quote#init()
  autocmd FileType textile call textobj#quote#init()
  autocmd FileType text call textobj#quote#init({'educate': 0})
  autocmd FileType notes call textobj#quote#init({'educate': 1})
augroup END

Plug 'xolox/vim-misc'           " Required by vim-notes
Plug 'xolox/vim-notes'          " Essential notes plugin.
let g:notes_directories = ['~/Dropbox/Notes']
" This ensures that updating vim notes won't overwrite my blank default note.
let g:notes_shadowdir = '~/.vim/notes-shadow/' 
" Set file extension for notes.
let g:notes_suffix = '.txt'
" Turn off notes.vim highlighting for vim command syntax.
highlight link notesVimCmd Normal
" Don't use curly quotes. 
"let g:notes_smart_quotes = 0

Plug 'VOoM'                    " Vim outliner

Plug 'junegunn/goyo.vim'       " Distraction-Free Writing Mode
nnoremap <Leader>g :Goyo<CR>

Plug 'szw/vim-dict'            " Dictionary
Plug 'vim-pandoc/vim-pandoc'   " For writing in Pandoc markdown
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'mattn/emmet-vim'         " HTML Authoring Autocompletion

Plug 'msprev/unite-bibtex'
let g:unite_bibtex_bib_files=['/home/jon/Dropbox/Papers/library.bib']
let g:unite_bibtex_bib_files=['/home/jon/Dropbox/Papers/library.bib']
let g:unite_bibtex_cache_dir='/tmp'

"}}}

" Git {{{2

Plug 'tpope/vim-fugitive'          " Git wrapper.

"Plugin 'jaxbot/github-issues.vim' " Github issues!
"" Source Oauth Token from private file for Using Github Issues Plugin
"if !empty(glob('~/.vim-private')) "if file exists
	"source ~/.vim-private
"endif

" }}}

" Browsers {{{2

Plug 'scrooloose/nerdtree' " File browser.

" Open nerdtree
map ,n :NERDTreeToggle<CR>  

"Open vimrc
map \v :sp ~/.vimrc<CR>

" }}}

" Colors {{{2

Plug 'vim-scripts/ScrollColors' " Easily switch colorschemes.
"Plug '29decibel/codeschool-vim-theme'
"Plug 'ryu-blacknd/vim-nucolors'
"Plug 'Lokaltog/vim-distinguished'
Plug 'whatyouhide/vim-gotham'
Plug 'mhartington/oceanic-next'
Plug 'zenorocha/dracula-theme', {'rtp': 'vim/'}
Plug 'chriskempson/base16-vim'
Plug 'JonathanReeve/vim-colorschemes'
" }}}


Plug 'tmhedberg/matchit'       " Improved matching for html tags
Plug 'Valloric/MatchTagAlways' " HTML Tag Matching

"Plug 'ivanov/vim-ipython'      " IPython
Plug 'bfredl/nvim-ipy'

" }}}

" IDE Stuff {{{2

"All kinds of syntax checking
Plug 'scrooloose/syntastic'
" Syntastic Options
let g:syntastic_php_checkers = ['php', 'phpcs']
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_xml_checkers = ['xmllint']
let g:syntastic_xml_xmlling_args = '--dtdvalid tei_all.dtd' 
map ,s :SyntasticToggleMode<CR>

" }}}

" Misc {{{2

Plug 'xolox/vim-shell'          " Allows for opening of URLs and other files. 
Plug 'tpope/vim-surround'       " For text surrounds like tags and quotes
Plug 'scrooloose/nerdcommenter' " For Easy Commenting
Plug 'tpope/vim-unimpaired'     " A bunch of mappings that do cool stuff
Plug 'rking/ag.vim'             " Grepping Stuff

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

Plug 'bling/vim-airline'     " A lighter-weight statusline
set laststatus=2
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

"" End Airline }}}

" Language Specific Stuff {{{

" this sets tabstops to four spaces per python guidelines
autocmd Filetype python setlocal expandtab tabstop=4 shiftwidth=4
autocmd Filetype php setlocal noexpandtab

"Plugin 'editorconfig/editorconfig-vim'

let g:EditorConfig_exclude_patterns = ['fugitive://.*']

" }}}

filetype plugin indent on     " required!


Plug 'freitass/todo.txt-vim'    " Todo List
au BufRead,BufNewFile todo.txt setfiletype=todo
command! Todo edit ~/Dropbox/Personal/Todo/todo.txt


Plug 'JonathanReeve/toggl.vim'  " Toggl Experiments
Plug 'vim-jp/vital.vim'         " Required by Toggl
Plug 'Shougo/unite.vim'         " Also required by Toggl
source ~/Dropbox/Personal/.toggl-api-key

call plug#end()

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
"colorscheme OceanicNext
colorscheme apprentice
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
autocmd BufRead *.mkd      set ft=markdown
autocmd BufRead *.md       set ft=markdown
autocmd BufRead *.markdown set ft=markdown
" }}}

" Enable this for profiling stuff, in case vim is being slow.
" Put it at the top of the file to enable profiling startup
" profile start profile.log
" profile func *
" profile file *

" Allows for folding in this file.
" vim:fdm=marker
