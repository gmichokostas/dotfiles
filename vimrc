filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'kchmck/vim-coffee-script'
Plugin 'jiangmiao/auto-pairs'
Plugin 'vim-ruby/vim-ruby'
Plugin 'shougo/neocomplete'
Plugin 'plasticboy/vim-markdown'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-haml'
Plugin 'tpope/vim-fugitive.git'
Plugin 'tpope/vim-surround'
Plugin 'altercation/vim-colors-solarized'
Plugin 'mattn/emmet-vim'
Plugin 'tpope/vim-repeat'
Plugin 'guns/vim-sexp'
Plugin 'airblade/vim-gitgutter'
Plugin 'scrooloose/NERDTree'
Plugin 'scrooloose/NERDCommenter'
Plugin 'scrooloose/syntastic'
Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'slim-template/vim-slim.git'
Plugin 'derekwyatt/vim-scala'
Plugin 'kien/ctrlp.vim'
Plugin 'msanders/snipmate.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-endwise'
Plugin 'elixir-lang/vim-elixir'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'keith/swift.vim'
Plugin 'fatih/vim-go'
Plugin 'joshdick/onedark.vim'
Plugin 'sheerun/vim-polyglot'
" Plugin 'yggdroot/indentline'
call vundle#end()

set noswapfile
set ruler
set cursorline
set scrolloff=3
syntax enable

" one dark config
let g:onedark_terminal_italics=1
let g:airline_theme='onedark'

set background=dark
colorscheme onedark "solarized

set encoding=utf-8
set mouse=a
set showcmd
set wildmenu
set wildmode=longest,full,full
set ttyfast
set title
set number
filetype plugin indent on

let blacklist = ['go']
autocmd BufWritePre * if index(blacklist, &ft) < 0 | set listchars=tab:..,trail:.,extends:#,nbsp:.
autocmd BufWritePre * if index(blacklist, &ft) < 0 | set list

set nowrap
set hlsearch
set incsearch
set ignorecase
set smartcase
set laststatus=2
set expandtab
set tabstop=2 shiftwidth=2 softtabstop=2
set backspace=indent,eol,start
set autoindent
set modeline
set tags=./tags;
set grepprg=ack

autocmd BufWritePre * :%s/\s\+$//e

" Enable copying to clipboard using `CTRL + c`
map <C-c> y:e ~/clipsongzboard<CR>P:w !pbcopy<CR><CR>:bdelete!<CR>

" check check for markdown files
autocmd BufRead,BufNewFile *.md,*.markdown setlocal spell

" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#sources#syntax#min_keyword_length = 3

" Python
let python_highlight_all = 1
autocmd Filetype python setlocal expandtab tabstop=8 shiftwidth=4 softtabstop=4

"set statusline=%F%m%r%h%w\
"set statusline+=%{fugitive#statusline()}\
"set statusline+=[%{strlen(&fenc)?&fenc:&enc}]
"set statusline+=\ [line\ %l\/%L]

"hi StatusLine ctermfg=Black ctermbg=White
" Change colour of statusline in insert mode
"au InsertEnter * hi StatusLine ctermbg=DarkBlue
"au InsertLeave * hi StatusLine ctermfg=Black ctermbg=White

nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" Whitespace fixes
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" navigate between buffers
map gn :bn<cr>
map gp :bp<cr>
map gd :bd<cr>

" AirLine
let g:airline#extensions#tabline#enabled = 1
"let g:airline#extensions#tabline#left_sep = '>'
let g:airline_symbols = {}
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

let $PATH = $PATH . ':' . expand('~/.local/bin')

" let g:indentLine_char = '¦'
" let g:indentLine_leadingSpaceEnabled = 1
" let g:indentLine_leadingSpaceChar = '·'

" Go section
autocmd Filetype go setlocal ts=4 sts=4 sw=4
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_command = "goimports"
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }

"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
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
endif

" let g:polyglot_disabled = ['css']
