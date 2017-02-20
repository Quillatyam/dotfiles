""""""""""""""""""""""""""""""""""""""""""""""""""
" Maintainer:
"       Robert den Harink
"       http://robhar.com robert@robhar.com
" Version:
"       1.1 - 28-10-2016
"
"""""""""""""""""""""""""""""""""""""""""""""""""""

"General
"""""""""""""""""""""""""""""""""""""""""""""""""""
syntax on
filetype plugin indent on
                       
set nocompatible
set hidden
set number
set ruler
set showmode
set lazyredraw
set mouse=a
set encoding=utf8
set clipboard=unnamed
set pastetoggle=<F11>
set ffs=unix,mac,dos

set wrap
set lbr
set tw=79

set smarttab
set expandtab
set smartindent
set autoindent
set tabstop=4
set softtabstop=4
set shiftwidth=4

set so=999
set mat=5

set incsearch
set smartcase
set wildmenu
set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=longest,list,full
set completeopt=menuone,menu,longest

set magic
set showmatch
set autoread
set cmdheight=1
set shiftwidth=4
set hlsearch

colorscheme base16-eighties
set background=dark
set colorcolumn=80
let &colorcolumn=join(range(81,999), ',')
highlight ColorColumn guibg=Yellow

" Pathogen Infect
"""""""""""""""""""""""""""""""""""""""""""""""""""
execute pathogen#infect()

" Leader
"""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader = ","
let g:mapleader = ","

" Git Gutter
""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gitgutter_realtime=1                                                           
let g:gitgutter_eager=1 

" Deoplete.
"""""""""""""""""""""""""""""""""""""""""""""""""""
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1
" Golang
let g:deoplete#sources#go#use_cache = 1
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#json_directory = '~/.cache/deoplete/go/$GOOS_GOARCH'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']

" EasyMotion
"""""""""""""""""""""""""""""""""""""""""""""""""""
let g:EasyMotion_smartcase = 1
" <Leader>f{char} to move to {char}
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)
" s{char}{char} to move to {char}{char}
nmap s <Plug>(easymotion-overwin-f2)
" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)
" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)
" Gif config
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)
" These `n` & `N` mappings are options. You do not have to map `n` & `N` to 
" EasyMotion. Without these mappings, `n` & `N` works fine. 
" (These mappings just provide different highlight method and have some other 
" features )
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)

" NerdCommenter
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

" Syntastic
"""""""""""""""""""""""""""""""""""""""""""""""""""
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }

" NERDTree
"""""""""""""""""""""""""""""""""""""""""""""""""""
"Open NERDTREE when no file is opened
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
map <C-n> :NERDTreeToggle<CR>

" Haskell
""""""""""""""""""""""""""""""""""""""""""""""""""
let g:haskellmode_completion_ghc = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>

map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>

let g:haskell_tabular = 1

" Tabularize
""""""""""""""""""""""""""""""""""""""""""""""""""
vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a- :Tabularize /-><CR>
vmap a, :Tabularize /<-<CR>
vmap al :Tabularize /[\[\\|,]<CR>

" React
""""""""""""""""""""""""""""""""""""""""""""""""""
let g:jsx_ext_required = 0

" Golang
"""""""""""""""""""""""""""""""""""""""""""""""""""
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)

"By default syntax-highlighting for Functions,
"Methods and Structs is disabled.
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

"By default new terminals are opended in vertical split, 
"make it horizontal
let g:go_term_mode = "split"
"Run go-test in a new terminal
let g:go_term_enabled = 1
"Use with syntastic
let g:go_list_type = "quickfix"
"Enable goimports to automatically insert import paths 
"instead of gofmt
let g:go_fmt_command = "goimports"
"vim-go shows errors for the fmt command
let g:go_fmt_fail_silently = 0
"Format on save
let g:go_fmt_autosave = 1
"Disable updating dependencies when installing/updating binaries
let g:go_get_update = 0

" PHP
"""""""""""""""""""""""""""""""""""""""""""""""""""
" php.vim
let g:syntastic_php_checkers=['php', 'phpcs']
let g:syntastic_php_phpcs_args='--standard=PSR2 -n'

" Format to psr2
let g:php_cs_fixer_rules = "@PSR2"
let g:php_cs_fixer_php_path = "php"
let g:php_cs_fixer_enable_default_mapping = 1 "default (<leader>pcd, <leader>pcf)
let g:php_cs_fixer_dry_run = 0                    
let g:php_cs_fixer_verbose = 0

" Ctags
let g:tagbar_phpctags_memory_limit = '256M'

" VDebug
let g:vdebug_options= {
    \    "port" : 9001,
    \    "server" : '',
    \    "timeout" : 20,
    \    "on_close" : 'detach',
    \    "break_on_open" : 0,
    \    "ide_key" : '',
    \    "path_maps": {'/opt/www/kedo-current': '/Users/robert/src/github.com/Rauwekost/kedo-dev-env/kedo-current'},
    \    "debug_window_level" : 2,
    \    "debug_file_level" : 0,
    \    "debug_file" : "",
    \    "watch_window_style" : 'compact',
    \    "marker_default" : '⬦',
    \    "marker_closed_tree" : '▸',
    \    "marker_open_tree" : '▾'
    \}
let g:vdebug_keymap = {
\    "run_to_cursor" : "<Down>",
\    "step_over" : "<Up>",
\    "step_into" : "<Left>",
\    "step_out" : "<Right>",
\    "close" : "q",
\    "detach" : "x",
\}

" Disabling the directional keys / Hardtime config
"""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <leader>h :HardTimeToggle<CR>
let g:hardtime_default_on = 0
let g:list_of_normal_keys = ["h", "j", "k", "l", "-", "+","<UP>", "<DOWN>", "<LEFT>", "<RIGHT>"]
let g:list_of_visual_keys = ["h", "j", "k", "l", "-", "+"]
let g:list_of_insert_keys = ["<UP>", "<DOWN>", "<LEFT>", "<RIGHT>"]
let g:hardtime_showmsg = 1
let g:hardtime_timeout = 100
let g:hardtime_ignore_buffer_patterns = [ "NERD.*" ]

" SuperTab
"""""""""""""""""""""""""""""""""""""""""""""""""""
let g:SuperTabDefaultCompletionType = "<c-x><c-o>"

" Ctrl+P
""""""""""""""""""""""""""""""""""""""""""""""""""
map <silent> <Leader>t :CtrlP()<CR>
noremap <leader>b<space> :CtrlPBuffer<cr>
let g:ctrlp_custom_ignore = '\v[\/]dist$'

" Tagbar
"""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <F6> :TagbarToggle<CR>

" Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""
"Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk
"Move a line of text using SHIFT+[jk]
nnoremap <S-Up> :m-2<CR>
nnoremap <S-Down> :m+<CR>
inoremap <S-Up> <Esc>:m-2<CR>
inoremap <S-Down> <Esc>:m+<CR>
"Clear last search highlight by esaping twice
nnoremap <esc><esc> :noh<return>
"Fast saving
nmap <leader>w :w!<cr>
"Fast close
nmap <leader>q :q<cr>

"Status line
"""""""""""""""""""""""""""""""""""""""""""""""""""
"set laststatus=2
let g:airline#extensions#tabline#show_buffers = 0
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_theme='base16_eighties'

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
