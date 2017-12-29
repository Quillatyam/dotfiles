" Robert den Harink .vimrc
" Most aimed at Haskell / Golang / C / LaTeX
"
" Feel free to copy ;-)

" General {{{
" Syntax highlighting
syntax on

" enable loading indent filt for filetype
filetype plugin indent on

" Use indentation for folds
set foldmethod=indent
set foldnestmax=5
set foldlevelstart=99
set foldcolumn=0

augroup vimrcFold
  " fold vimrc itself by categories
  autocmd!
  autocmd FileType vim set foldmethod=marker
  autocmd FileType vim set foldlevel=0
augroup END

" Set to auto read when a file is changed from the outside
set autoread

" Use par for prettier line formatting
set formatprg=par
let $PARINIT = 'rTbgqR B=.,?_A_a Q=_s>|'

" Set a leader key
let mapleader = ","
  
" Leader key timeout
set tm=2000 "Leader key timeout

" Allow the normal use of "," by pressing it twice
noremap ,, ,

" Kill the damned Ex mode.
nnoremap Q <nop>

" Make <c-h> work like <c-h> again (this is a problem with libterm)
nnoremap <BS> <C-w>h

" Use Unix as the standard file type
set ffs=unix,dos,mac

" 80 Column mark
set colorcolumn=80

" }}}

" Plug Manager {{{
call plug#begin()
Plug 'jgdavey/tslime.vim'
Plug 'ervandew/supertab'
Plug 'moll/vim-bbye'
Plug 'vim-scripts/gitignore'
Plug 'neomake/neomake'

" Git
Plug 'tpope/vim-fugitive'
Plug 'int3/vim-extradite'

" Bars, panels, and files
Plug 'scrooloose/nerdtree'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'majutsushi/tagbar'

" Themes
Plug 'joshdick/onedark.vim'

" Text manipulation
Plug 'vim-scripts/Align'
Plug 'simnalamburt/vim-mundo'
Plug 'tpope/vim-commentary'
Plug 'godlygeek/tabular'
Plug 'michaeljsmith/vim-indent-object'
Plug 'easymotion/vim-easymotion'
Plug 'ConradIrwin/vim-bracketed-paste'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'terryma/vim-multiple-cursors'

" Allow pane movement to jump out of vim into tmux
Plug 'christoomey/vim-tmux-navigator'

" Haskell
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'enomsg/vim-haskellConcealPlus', { 'for': 'haskell' }
Plug 'eagletmt/ghcmod-vim', { 'for': 'haskell' }
Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }
Plug 'Twinside/vim-hoogle', { 'for': 'haskell' }
Plug 'mpickering/hlint-refactor-vim', { 'for': 'haskell' }
Plug 'nbouscal/vim-stylish-haskell'

Plug 'tpope/vim-sensible'
Plug 'reedes/vim-lexical'
Plug 'niklasl/vim-rdf'
Plug 'tpope/vim-surround'
Plug 'tomtom/tlib_vim'
Plug 'vim-scripts/vim-addon-mw-utils'
Plug 'dag/vim-fish'
Plug 'b4b4r07/vim-hcl'
Plug 'honza/vim-snippets'
Plug 'Shougo/deoplete.nvim'
Plug 'vim-syntastic/syntastic'
Plug 'shougo/vimproc.vim', {'do' : 'make'}
call plug#end()
" }}}

" Vim UI {{{
" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

" Turn on the WiLd menu
set wildmenu

" Tab-complete files up to longest unambiguous prefix
set wildmode=list:longest,full

" Always show current position
set ruler
set number

" Show trailing whitespace
set list

" But only interesting whitespace
if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

" Height of the command bar
set cmdheight=1

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch

" How many tenths of a second to blink when matching brackets
set mat=2

" No annoying sound on errors
set noerrorbells
set vb t_vb=

if &term =~ '256color'
  " disable Background Color Erase (BCE) so that color schemes
  " render properly when inside 256-color tmux and GNU screen.
  " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif

" Force redraw
map <silent> <leader>r :redraw!<CR>

" Turn mouse mode on
nnoremap <leader>ma :set mouse=a<cr>

" Turn mouse mode off
nnoremap <leader>mo :set mouse=<cr>

" Default to mouse mode on
set mouse=a

" Change cursor shape between insert and normal mode in iTerm2.app
if $TERM_PROGRAM =~ "iTerm"
  let &t_SI = "\<Esc>]50;CursorShape=1\x7" " Vertical bar in insert mode
  let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
endif

" }}}

" Colors and Fonts {{{
" 16-color mode is the preferred option, since its colors are more accurate than those of 256-color mode. 
let g:onedark_termcolors=16
let g:onedark_terminal_italics=1

"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if (has("termguicolors"))
  set termguicolors
endif

" Colorscheme
colorscheme onedark

" Highlight colors
if (has("autocmd"))
  augroup colorextend
    autocmd!
    " Make `Function`s bold in GUI mode
    autocmd ColorScheme * call onedark#extend_highlight("Function", { "gui": "bold" })
    " Override the `Statement` foreground color in 256-color mode
    autocmd ColorScheme * call onedark#extend_highlight("Statement", { "fg": { "cterm": 128 } })
    " Override the `Identifier` background color in GUI mode
    autocmd ColorScheme * call onedark#extend_highlight("Identifier", { "bg": { "gui": "#333333" } })
  augroup END
endif

" }}}

" Files, backups and undo {{{
" Turn backup off, since most stuff is in Git anyway...
set nobackup
set nowb
set noswapfile

" Source the vimrc file after saving it
augroup sourcing
  autocmd!
  if has('nvim')
    autocmd bufwritepost init.vim source $MYVIMRC
  else
    autocmd bufwritepost .vimrc source $MYVIMRC
  endif
augroup END

" Open file prompt with current path
nmap <leader>e :e <C-R>=expand("%:p:h") . '/'<CR>

" Show undo tree
nmap <silent> <leader>u :MundoToggle<CR>

" Fuzzy find files
nnoremap <silent> <Leader><space> :CtrlP<CR>
let g:ctrlp_max_files=0
let g:ctrlp_show_hidden=1
let g:ctrlp_custom_ignore = { 'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$' }
"}}}

" Text, tab and indent related {{{
" Use spaces instead of tabs
set expandtab

" 1 tab == 2 spaces, unless the file is already
" using tabs, in which case tabs will be inserted.
set shiftwidth=2
set softtabstop=2
set tabstop=2

" Linebreak on 500 characters
set lbr
set tw=500

set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines

" Copy and paste to os clipboard
nmap <leader>y "*y
vmap <leader>y "*y
nmap <leader>d "*d
vmap <leader>d "*d
nmap <leader>p "*p
vmap <leader>p "*p
" }}}

" Visual mode related {{{
" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :call VisualSelection('f', '')<CR>
vnoremap <silent> # :call VisualSelection('b', '')<CR>
" }}}

" Moving around, tabs, windows and buffers {{{
" Treat long lines as break lines (useful when moving around in them)
nnoremap j gj
nnoremap k gk

noremap <c-h> <c-w>h
noremap <c-k> <c-w>k
noremap <c-j> <c-w>j
noremap <c-l> <c-w>l

" Disable highlight when <leader><cr> is pressed
" but preserve cursor coloring
nmap <silent> <leader><cr> :noh\|hi Cursor guibg=red<cr>

" Return to last edit position when opening files (You want this!)
augroup last_edit
  autocmd!
  autocmd BufReadPost *
       \ if line("'\"") > 0 && line("'\"") <= line("$") |
       \   exe "normal! g`\"" |
       \ endif
augroup END

" Remember info about open buffers on close
set viminfo^=%

" Open window splits in various places
nmap <leader>sh :leftabove  vnew<CR>
nmap <leader>sl :rightbelow vnew<CR>
nmap <leader>sk :leftabove  new<CR>
nmap <leader>sj :rightbelow new<CR>

" Manually create key mappings (to avoid rebinding C-\)
let g:tmux_navigator_no_mappings = 1

" don't close buffers when you aren't displaying them
set hidden

" previous buffer, next buffer
nnoremap <leader>bp :bp<cr>
nnoremap <leader>bn :bn<cr>

" close every window in current tabview but the current
nnoremap <leader>bo <c-w>o

" delete buffer without closing pane
noremap <leader>bd :Bd<cr>

" fuzzy find buffers
noremap <leader>b<space> :CtrlPBuffer<cr>

" Neovim terminal configurations
" Use <Esc> to escape terminal insert mode
"
if has('nvim')
  " Use <Esc> to escape terminal insert mode
  tnoremap <Esc> <C-\><C-n>
  " Make terminal split moving behave like normal neovim
  tnoremap <c-h> <C-\><C-n><C-w>h
  tnoremap <c-j> <C-\><C-n><C-w>j
  tnoremap <c-k> <C-\><C-n><C-w>k
  tnoremap <c-l> <C-\><C-n><C-w>l
  " Make terminal buffers start in insertmode
  autocmd BufWinEnter,WinEnter term://* startinsert
  autocmd BufLeave term://* stopinsert
endif
" }}}

" Status line {{{
" Always show the status line
set laststatus=2
" }}}

" Editing mappings {{{
" Utility function to delete trailing white space
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc

" Search selected text
vnoremap // y/\V<C-R>"<CR>
" }}}

" NERDTree {{{
" Close nerdtree after a file is selected
let NERDTreeQuitOnOpen = 0

function! IsNERDTreeOpen()
  return exists("t:NERDTreeBufName") && (bufwinnr(t:NERDTreeBufName) != -1)
endfunction

function! ToggleFindNerd()
  if IsNERDTreeOpen()
    exec ':NERDTreeToggle'
  else
    exec ':NERDTreeFind'
  endif
endfunction

" If nerd tree is closed, find current file, if open, close it
nmap <silent> <leader>f <ESC>:call ToggleFindNerd()<CR>
nmap <silent> <leader>F <ESC>:NERDTreeToggle<CR>
" }}}

" Alignment {{{
" Stop Align plugin from forcing its mappings on us
let g:loaded_AlignMapsPlugin=1
" Align on equal signs
map <Leader>a= :Align =<CR>
" Align on commas
map <Leader>a, :Align ,<CR>
" Align on pipes
map <Leader>a<bar> :Align <bar><CR>
" Prompt for align character
map <leader>ap :Align
" }}}

" Tags {{{
map <leader>tt :TagbarToggle<CR>

set tags=tags;/
set cst
set csverb
" }}}

" Git {{{
let g:extradite_width = 60
" Hide messy Ggrep output and copen automatically
function! NonintrusiveGitGrep(term)
  execute "copen"
  " Map 't' to open selected item in new tab
  execute "nnoremap <silent> <buffer> t <C-W><CR><C-W>T"
  execute "silent! Ggrep " . a:term
  execute "redraw!"
endfunction

command! -nargs=1 GGrep call NonintrusiveGitGrep(<q-args>)
nmap <leader>gs :Gstatus<CR>
nmap <leader>gg :copen<CR>:GGrep 
nmap <leader>gl :Extradite!<CR>
nmap <leader>gd :Gdiff<CR>
nmap <leader>gb :Gblame<CR>

function! CommittedFiles()
  " Clear quickfix list
  let qf_list = []
  " Find files committed in HEAD
  let git_output = system("git diff-tree --no-commit-id --name-only -r HEAD\n")
  for committed_file in split(git_output, "\n")
    let qf_item = {'filename': committed_file}
    call add(qf_list, qf_item)
  endfor
  " Fill quickfix list with them
  call setqflist(qf_list)
endfunction

" Show list of last-committed files
nnoremap <silent> <leader>g? :call CommittedFiles()<CR>:copen<CR>
" }}}

" Completion {{{
set completeopt+=longest

" Use buffer words as default tab completion
let g:SuperTabDefaultCompletionType = '<c-x><c-p>'
if has("gui_running")
  imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
else " no gui
  if has("unix")
    inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
  endif
endif
" }}}

" Syntastic {{{
map <Leader>s :SyntasticToggleMode<CR>

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

"let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['haskell'] }
" }}}

" Autocorect {{{
ab teh the
ab sefl self
ab equivelant equivalent
" }}}

" Other Languages {{{
autocmd BufNewFile,BufRead *.ocaml set filetype=ocaml
autocmd BufNewFile,BufRead *.go set filetype=go
autocmd BufNewFile,BufRead *.pure set filetype=pure
autocmd BufNewFile,BufRead *.js set filetype=javascript
autocmd BufNewFile,BufRead *.md set filetype=markdown
autocmd BufNewFile,BufRead *.ll set filetype=llvm
autocmd BufNewFile,BufRead *.scala set filetype=scala
autocmd BufNewFile,BufRead *.c set filetype=c
" }}}

" Fast Navigation {{{
" Go to last edited line one keystroke 
map ` g;
map gl <C-^>
" }}}

" Writing Mode {{{
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
" autocmd Filetype markdown call SetMarkdownOptions()

:hi SpellBad cterm=underline ctermfg=red

" Lexical settings
let g:lexical#spelllang = ["en_gb","nl",]
let g:lexical#thesaurus = ['~/.config/nvim/thesaurus/mthesaur.txt',]
let g:lexical#dictionary_key = '<leader>k'
" }}}

" Motion {{{
" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1
" }}}

" Airline {{{
let g:airline_powerline_fonts = 1
let g:airline_theme='onedark'
" }}}

" Deoplete {{{
let g:deoplete#enable_at_startup = 1
" }}}

" Slime {{{
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane": ":.1"}

vmap <silent> <Leader>rs <Plug>SendSelectionToTmux
nmap <silent> <Leader>rs <Plug>NormalModeSendToTmux
nmap <silent> <Leader>rv <Plug>SetTmuxVars
" }}}

" Haskell {{{
" haskell config path
let h_config_haskell = expand(resolve($HOME . "/.config/nvim/vimrc.haskell"))
execute 'source '. h_config_haskell
"let $PATH = $PATH . ':' . expand('~/.cabal/bin')

let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
" }}}

"{{{ Multicursor
" Called once right before you start selecting multiple cursors
function! Multiple_cursors_before()
  if exists(':NeoCompleteLock')==2
    exe 'NeoCompleteLock'
  endif
endfunction

" Called once only when the multiple selection is canceled (default <Esc>)
function! Multiple_cursors_after()
  if exists(':NeoCompleteUnlock')==2
    exe 'NeoCompleteUnlock'
  endif
endfunction

" Default highlighting (see help :highlight and help :highlight-link)
highlight multiple_cursors_cursor term=reverse cterm=reverse gui=reverse
highlight link multiple_cursors_visual Visual

"}}}
