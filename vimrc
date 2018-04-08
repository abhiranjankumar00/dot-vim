" Set 'nocompatible' to ward off unexpected things that your distro might
" have made, as well as sanely reset options when re-sourcing .vimrc
set nocompatible

" Uncomment the following to have Vim jump to the last position when reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Attempt to determine the type of a file based on its name and possibly its
" contents. Use this to allow intelligent auto-indenting for each filetype,
" and for plugins that are filetype specific.
filetype indent plugin on

syntax on 		"Enable syntax highlighting

set number		" Always show line number
set wrap                " Always wrap
set mouse=a 		" Enable use of the mouse for all modes
set scrolloff=100	" Keep cursor at the centre of screen
set ruler		" Show cursor position in status line

set wildmenu 		" Better command-line completion
set showcmd 		" Show partial commands in the last line of the screen
set history=1000	" Remember last 1000 commands

set clipboard=unnamedplus,autoselect " y and d put stuff into system clipboard (so that other apps can see it)
set pastetoggle=<F12> 	" <F12> toggles paste mode

set autoread  		" Automatically re-read file changed outside vim
set autowrite		" Automatically save before commands like :next and :make
" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %



" Searching
set hlsearch 		" Highlight all matching string
" Map <C-L> (redraw screen) to also turn off search highlighting until the  next search
nnoremap <C-L> :nohl<CR><C-L>
set ignorecase 		" Use case insensitive search
set smartcase 		" Override the 'ignorecase' option if the search pattern contains upper case characters
set incsearch		" Search as you type

" Allow backspacing over autoindent, line breaks and start of insert action
set backspace=indent,eol,start

" Indentation settings for using 4 spaces instead of tabs.
" Do not change 'tabstop' from its default value of 8 with this setup.
set shiftwidth=4
set softtabstop=4
set expandtab

" When opening a new line and no filetype-specific indenting is enabled, keep
" the same indent as the line you're currently on. Useful for READMEs, etc.
set autoindent

" Use default wrap option in diffmode `wrap<` uses default wrap setting
" autocmd FilterWritePre * if &diff | setlocal wrap< | endif
autocmd FilterWritePre * if &diff | setlocal wrap | endif

"Adding template for cpp, java and haskell and rest
autocmd! BufNewFile * silent! 0r $HOME/.vim/skel/skel.%:e

function! ShowTralingSpace()
    " Highligting trailing whitespaces
    highlight ExtraWhitespace ctermbg=red guibg=red
    match ExtraWhitespace /\s\+$/
endfunction

" call `ShowTrailingSpace` in each read/write
autocmd BufRead *.* call ShowTralingSpace()
autocmd BufWrite *.* call ShowTralingSpace()

function TrimTrailingSpaces()
  if !&binary && &filetype != 'diff'
    normal mz
    normal Hmy
    %s/\s\+$//e
    normal 'yz<CR>
    normal `z
  endif
endfunction
