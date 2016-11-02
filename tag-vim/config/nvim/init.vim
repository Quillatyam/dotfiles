"""""""""""""""""""""""""""""""""""""""""""""""""""
" Maintainer:
"       Robert den Harink
"       http://robhar.com robert@robhar.com
" Version:
"       1.1 - 28-10-2016
"
"""""""""""""""""""""""""""""""""""""""""""""""""""


"Pathogen
"""""""""""""""""""""""""""""""""""""""""""""""""""
execute pathogen#infect()

"General
"""""""""""""""""""""""""""""""""""""""""""""""""""
colorscheme solarized
syntax enable
let mapleader = ","
let g:mapleader = ","
set history=1000
set encoding=utf8
set ffs=unix,mac,dos
set autoread
set wildmenu
set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=longest,list,full
set nocompatible
set nowrap
set showmode
set tw=80
set so=7
set ruler
set number
set cmdheight=1
set ignorecase
set smartcase
set smarttab
set smartindent
set autoindent
set softtabstop=2
set expandtab
set incsearch
set mouse=a
set hlsearch
set lazyredraw
set magic
set showmatch
set mat=2
set completeopt+=noselect "deoplete recommend
"set completeopt=menuone,menu,longest
"set completeopt+=longest
set t_CO=256
set guifont=Menlo\ for\ Powerline
set background=dark
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set lbr
set tw=500
set colorcolumn=80
let &colorcolumn=join(range(81,999), ',')
highlight ColorColumn guibg=Black
set nobackup
set nowb
set noswapfile

"Use deoplete.
"""""""""""""""""""""""""""""""""""""""""""""""""""
let g:deoplete#enable_at_startup = 1

"FileTypePlugin
"""""""""""""""""""""""""""""""""""""""""""""""""""
filetype plugin on

"NerdCommenter
"""""""""""""""""""""""""""""""""""""""""""""""""""
"Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

"Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1

"Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'

"Set a language to use its alternate delimiters by default
let g:NERDAltDelims_java = 1

"Add your own custom formats or override the defaults
let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }

"Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1

"Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1

"Splits
"""""""""""""""""""""""""""""""""""""""""""""""""""
set splitbelow
set splitright
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
map <Leader>% :vsp<CR>
map <Leader>" :sp<CR>

"Tabs
""""""""""""""""""""""""""""""""""""""""""""""""""
map <Leader>t :tabnew<CR>


"Terminal session open
map <Leader>~ :e term://fish<CR>

"Syntastic
"""""""""""""""""""""""""""""""""""""""""""""""""""
map <Leader>s :SyntasticToggleMode<CR>
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1


"NERDTree
"""""""""""""""""""""""""""""""""""""""""""""""""""
"Open NERDTREE when no file is opened
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
map <C-n> :NERDTreeToggle<CR>


"Tabularize
"""""""""""""""""""""""""""""""""""""""""""""""""
vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a- :Tabularize /-><CR>


"Haskell
""""""""""""""""""""""""""""""""""""""""""""""""""
let g:haskell_tabular = 1
let g:haskellmode_completion_ghc = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc


"Golang
"""""""""""""""""""""""""""""""""""""""""""""""""""
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)

"By default syntax-highlighting for Functions, Methods and Structs is disabled.
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

"By default new terminals are opended in vertical split, make it horizontal
let g:go_term_mode = "split"

"Run go-test in a new terminal
let g:go_term_enabled = 1

"Use with syntastic
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }
let g:go_list_type = "quickfix"

"Enable goimports to automatically insert import paths instead of gofmt
let g:go_fmt_command = "goimports"

"vim-go shows errors for the fmt command
let g:go_fmt_fail_silently = 0

"Format on save
let g:go_fmt_autosave = 1

"Syntastic fixes
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }

"Disable updating dependencies when installing/updating binaries
let g:go_get_update = 0

"Deoplete-go settings
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
let g:deoplete#sources#go#use_cache = 1
let g:deoplete#sources#go#json_directory = '~/.cache/deoplete/go/$GOOS_GOARCH'

"Elm
"""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <leader>el :ElmEvalLine<CR>
vnoremap <leader>es :<C-u>ElmEvalSelection<CR>
nnoremap <leader>em :ElmMakeCurrentFile<CR>

"GHC-Mod
"""""""""""""""""""""""""""""""""""""""""""""""""""
filetype plugin indent on
map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GHCModTypeClear<CR>

"Disabling the directional keys
"""""""""""""""""""""""""""""""""""""""""""""""""""
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>
"imap <up> <nop>
"imap <down> <nop>
"imap <left> <nop>
"imap <right> <nop>

"Easier escape key 
""""""""""""""""""""""""""""""""""""""""""""""""""
:imap jj <Esc>

"SuperTab
"""""""""""""""""""""""""""""""""""""""""""""""""""
let g:SuperTabDefaultCompletionType = '<c-x><c-o>'

if has("gui_running")
  imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
else " no gui
  if has("unix")
    inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
  endif
endif


"ctrl+p
""""""""""""""""""""""""""""""""""""""""""""""""""
"map <silent> <Leader>t :CtrlP()<CR>
noremap <leader>b<space> :CtrlPBuffer<cr>
let g:ctrlp_custom_ignore = '\v[\/]dist$'


"copy and paste
""""""""""""""""""""""""""""""""""""""""""""""""""
vmap <C-c> "+yi
vmap <C-x> "+c
vmap <C-v> c<ESC>"+p
imap <C-v> <ESC>"+pa

"Tagbar
"""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <F6> :TagbarToggle<CR>


"Moving around, Tabs windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""
"Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

"Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

"Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove

"Clear last search highlight by esaping twice
nnoremap <esc><esc> :noh<return>

"Return to last edit position when opening files (You want this!)
autocmd BufReadPost *Ignore
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif
"Remember info about open buffers on close
set viminfo^=%


"Status line
"""""""""""""""""""""""""""""""""""""""""""""""""""
"Always show the status line
set laststatus=2
let g:airline_powerline_fonts = 1

"Format the status line
"set statusline=%t       "tail of the filename
"set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
"set statusline+=%{&ff}] "file format
"set statusline+=%h      "help file flag
"set statusline+=%m      "modified flag
"set statusline+=%r      "read only flag
"set statusline+=%y      "filetype
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*
"set statusline+=%=      "left/right separator
"set statusline+=%c,     "cursor column
"set statusline+=%l/%L   "cursor line/total lines
"set statusline+=\ %P    "percent through file

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

"Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""
"Move a line of text using ALT+[jk] or Comamnd+[jk] on mac
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z<Paste>

if has("mac") || has("macunix")
  nmap <D-j> <M-j>
  nmap <D-k> <M-k>
  vmap <D-j> <M-j>
  vmap <D-k> <M-k>
endif

"Fast saving
nmap <leader>w :w!<cr>

"Fast close
nmap <leader>q :q<cr>

"Go to tab by number
noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt
noremap <leader>0 :tablast<cr>


"Helper functions
"""""""""""""""""""""""""""""""""""""""""""""""""""
"Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    en
    return ''
endfunction
