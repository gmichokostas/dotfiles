let mapleader = "\<Space>"

filetype indent plugin on " enable file type detection
syntax enable             " enable syntax highlight

set number      " show line numbers
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
set hidden
set encoding=utf-8
set timeoutlen=300
set lazyredraw " redraw onlw when needed<Paste>

" Show hidden characters
" Verbose: set listchars=nbsp:¬,eol:¶,extends:»,precedes:«,trail:•
set nolist
set listchars=nbsp:¬,extends:»,precedes:«,trail:•

call plug#begin('~/.local/share/nvim/plugged')

" Color settings
Plug 'chriskempson/base16-vim'
Plug 'joshdick/onedark.vim'
Plug 'itchyny/lightline.vim'

" Fuzzy finder
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Utils
Plug 'w0rp/ale'
Plug 'machakann/vim-highlightedyank'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'junegunn/vim-easy-align'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'

" Languages support
Plug 'rust-lang/rust.vim'
Plug 'fatih/vim-go'
Plug 'plasticboy/vim-markdown'
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'tpope/vim-rails'
Plug 'posva/vim-vue'

call plug#end()

" Colors
set background=dark
let g:onedark_terminal_italics=1
colorscheme onedark

if (has("termguicolors"))
	set termguicolors
endif

" Lightline config
let g:lightline = {
      \ 'component_function': {
      \   'filename': 'LightlineFilename',
      \ },
\ }
function! LightlineFilename()
        return expand('%:t') !=# '' ? @% : '[No Name]'
endfunction

" No arrow keys
nnoremap <up>    <nop>
nnoremap <down>  <nop>
inoremap <up>    <nop>
inoremap <down>  <nop>
inoremap <left>  <nop>
inoremap <right> <nop>

" Ale settings
let g:ale_lint_on_save = 1
let g:ale_sign_warning = '▲'
let g:ale_sign_error   = '✗'
let g:ale_linters = {
      \ 'c': ['clang'],
      \ 'cpp': ['clang'],
      \ 'ruby': ['ruby'],
      \}

" Rust
let g:rustfmt_command = "rustfmt +nightly"
let g:racer_experimental_completer = 1
let g:rustfmt_autosave = 1

" go section

let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_command = "goimports"
let g:go_fmt_autosave = 1

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


" Key mappings
"
" ctrl+p open fzf files
noremap <c-p> :Files<CR>

" Ripgrep
noremap <leader>s :Rg

" Start interactive EasyAlign in visual mode
vnoremap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object
nnoremap ga <Plug>(EasyAlign)

" Delete current line in insert mode
inoremap <c-d> <esc>ddi

" Upcase current word in insert mode
inoremap <c-u> <esc>viwUi

" Upcase current word in normal mode
nnoremap <c-u> viwU<esc>

" Add blank line below cursor
nnoremap <leader>o o<esc>

" Add blank line below cursor
nnoremap <leader>b O<esc>

" Quick edit vim settings file
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

" Source vim settings file
nnoremap <leader>sv :source $MYVIMRC<cr>

" esc insert mode
inoremap jk <esc>

" Quick save
noremap <leader>w :w<cr>

" Show buffers
noremap <leader><space> :Buffers<cr>

" Left and right can switch buffers
nnoremap <left> :bp<cr>
nnoremap <right> :bn<cr>

" <leader><leader> toggles between buffers
nnoremap <leader><leader> <c-^>

" <leader>, shows/hides hidden characters
noremap <leader>, :set invlist<cr>

" Copy to clipboard
vnoremap  <leader>y  "+y
nnoremap  <leader>y  "+y
nnoremap  <leader>yy  "+yy

" Paste from clipboard
nnoremap <leader>p "+p
vnoremap <leader>p "+p
vnoremap <leader>P "+P

" Strip trailing spaces on save
function! StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun

autocmd BufWritePre * :call StripTrailingWhitespaces()

