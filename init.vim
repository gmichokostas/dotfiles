nnoremap <space> <nop>
let mapleader = "\<Space>"

filetype indent plugin on " enable file type detection
syntax enable             " enable syntax highlight

set number      " show line numbers
set relativenumber
set mouse=a     " enable mouse
set cursorline  " highlight the current line
set scrolloff=3 " Minimal number of screen lines to keep above and below the cursor.
set colorcolumn=80
set noswapfile " disable swap files
set showcmd    " show command in the last line of the screen
set wildmenu
set wildmode=longest,full,full
set cpoptions+=$
set hlsearch " When there is a previous search pattern, highlight all its matches.
set incsearch
set ignorecase
set smartcase
set expandtab
set autoindent
set autochdir
set hidden
set encoding=utf-8
set timeoutlen=300
set lazyredraw   " redraw only when needed<Paste>
set autoread     " Reload unchanged files automatically
set secure       " Limit what modelines and autocmds can do
set smartindent  " Do smart auto-indenting when starting a new line
set title
set nojoinspaces " Do not insert two spaces after '.' when using J
set softtabstop=2
set tabstop=2
set shiftwidth=2
set foldmethod=syntax " Fold config
set nofoldenable
set clipboard^=unnamed " Share clipboard with system

set spelllang=en_us                         " set en_us as the default spell checking language
set spellfile=$HOME/.vim-spell-en.utf-8.add " location to save the 'good' words
autocmd BufRead,BufNewFile *.md set filetype=markdown
autocmd FileType markdown setlocal spell    " Spell-check Markdown files
autocmd FileType gitcommit setlocal spell   " Spell-check Git messages

" Show hidden characters
set nolist
set listchars=nbsp:¬,extends:»,precedes:«,trail:•

call plug#begin('~/.local/share/nvim/plugged')

" Color settings
Plug 'itchyny/lightline.vim'
Plug 'chriskempson/base16-vim'
Plug 'sonph/onehalf', {'rtp': 'vim/'}

Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Utils
Plug 'w0rp/ale'
Plug 'junegunn/goyo.vim'
Plug 'machakann/vim-highlightedyank'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-vinegar'
Plug 'qpkorr/vim-bufkill'
Plug 'janko-m/vim-test'

" Languages support
Plug 'rust-lang/rust.vim'
Plug 'plasticboy/vim-markdown'
Plug 'vim-ruby/vim-ruby',    { 'for': 'ruby' }
Plug 'tpope/vim-fireplace',  { 'for': 'clojure' }
Plug 'tpope/vim-rails'
Plug 'posva/vim-vue'
Plug 'pangloss/vim-javascript',  { 'for': 'javascript' }
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'fatih/vim-go',             { 'for': 'go', 'do': ':GoUpdateBinaries' }
Plug 'othree/html5.vim'

call plug#end()

" Colors
set background=dark
colorscheme onehalfdark
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

" if filereadable(expand("~/.vimrc_background"))
"   let base16colorspace=256
"   source ~/.vimrc_background
" endif
" set termguicolors

highlight VertSplit ctermbg=NONE guibg=NONE
highlight Comment gui=italic
highlight ExtraWhitespace ctermbg=grey guibg=grey
" Show trailing whitespace and spaces before a tab"
match ExtraWhitespace /\s\+$\| \+\ze\t/

" Lightline config
let g:lightline = {
      \ 'colorscheme': 'onehalfdark',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'fugitive#head'
      \ },
      \ }

" Ale settings
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
let g:ale_sign_warning = '▲'
let g:ale_sign_error   = '✗'
let g:ale_linters = {
      \ 'c': ['clang'],
      \ 'cpp': ['clang'],
      \ 'ruby': ['ruby'],
      \ 'javascript': ['eslint'],
      \}

" Rust
let g:rustfmt_command = "rustfmt +nightly"
let g:racer_experimental_completer = 1
let g:rustfmt_autosave = 1

" Ruby conf
let g:ruby_indent_block_style = 'do'
let g:ruby_indent_assignment_style = 'variable'

" goyo config
let g:goyo_width = 120

if has("persistent_undo")
  set undodir=~/.undodir/
  set undofile
endif

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
endif

if executable('rg')
  set grepprg=rg\ --no-heading\ --vimgrep
  set grepformat=%f:%l:%c:%m
endif

command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
      \   <bang>0 ? fzf#vim#with_preview('up:60%')
      \           : fzf#vim#with_preview('right:50%:hidden', '?'),
      \   <bang>0)

" Command for git grep
command! -bang -nargs=* GGrep
      \ call fzf#vim#grep(
      \   'git grep --line-number '.shellescape(<q-args>), 0,
      \   { 'dir': systemlist('git rev-parse --show-toplevel')[0] }, <bang>0)

" Command for Ag
command! -bang -nargs=* Ag
      \ call fzf#vim#ag(<q-args>,
      \                 <bang>0 ? fzf#vim#with_preview('up:60%')
      \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
      \                 <bang>0)

" Files command with preview window
command! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

" vim-test strategy
let test#strategy = "neovim"
let test#neovim#term_position = "vsplit"

" Key mappings
"
" leader+f open fzf files
noremap <leader>f :Files<cr>

" Ripgrep
noremap <leader>s :Rg<space>

" Delete current line in insert mode
inoremap <c-d> <esc>ddi

" Upcase current word in insert mode
inoremap <c-u> <esc>viwUea

" Upcase current word in normal mode
nnoremap <leader>u viwU<esc>

" Add blank line below cursor
nnoremap <leader>o o<esc>

" Add blank line below cursor
nnoremap <leader>b O<esc>

" Quick edit vim settings file
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

" Source vim settings file
nnoremap <leader>r :source $MYVIMRC<cr>

" Easy access to the begining of line
nnoremap 0 ^

" esc insert mode
inoremap jk <esc>

" Easily navigate in wrapped lines
nnoremap j gj
nnoremap k gk

" Quick save
noremap <leader>w :w<cr>

" Quick quit
noremap <leader>k :q<cr>

" No highlight
nnoremap <leader>h :noh<cr>

" Show buffers
noremap <leader>; :Buffers<cr>

" Left and right can switch buffers
nnoremap <left> :bp<cr>
nnoremap <right> :bn<cr>

" <leader><leader> toggles between buffers
nnoremap <leader><leader> <c-^>

" <leader>, shows/hides hidden characters
nnoremap <leader>, :set invlist<cr>

" <leader>i indent the entire file
nnoremap <leader>i magg=G`azz

" Copy to clipboard
vnoremap  <leader>y  "+y
nnoremap  <leader>y  "+y

" Paste from clipboard
nnoremap <leader>p "+p
vnoremap <leader>p "+p

" Open the definition in a vertical split
nnoremap <C-\> :vsp <cr>:exec("tag ".expand("<cword>"))<cr>

" vim-test mappings
nnoremap <C-n> :TestNearest<cr>
nnoremap <C-f> :TestFile<cr>
nnoremap <C-s> :TestSuite<cr>
nnoremap <C-g> :TestVisit<cr>

" No arrow keys
nnoremap <up>    <nop>
nnoremap <down>  <nop>
inoremap <up>    <nop>
inoremap <down>  <nop>
inoremap <left>  <nop>
inoremap <right> <nop>

" Move around splits with <c-hjkl>
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" Align selected lines
vnoremap <leader>i ma=`a
" Close all other splits
nnoremap <leader>a :only<cr>

" vim-commentary
nnoremap <leader>/ :Commentary<cr>
vnoremap <leader>/ :Commentary<cr>

" Strip trailing spaces on save
function! StripTrailingWhitespaces()
  let l:save = winsaveview()
  %s/\\\@<!\s\+$//e
  call winrestview(l:save)
endfunction

autocmd BufWritePre * :call StripTrailingWhitespaces()
autocmd Filetype help nnoremap <buffer> q :q<cr>
autocmd FocusGained * silent! checktime
autocmd BufWritePre <buffer> :%s/\($\n\s*\)\+\%$//e
