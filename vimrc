" Set 'nocompatible' to ward off unexpected things that your distro might
" have made, as well as sanely reset options when re-sourcing .vimrc
set nocompatible

" Attempt to determine the type of a file based on its name and possibly its
" contents. Use this to allow intelligent auto-indenting for each filetype,
" and for plugins that are filetype specific.
filetype indent plugin on

syntax on 		"Enable syntax highlighting

set number		" Always show line number
set wildmenu 		" Better command-line completion
set showcmd 		" Show partial commands in the last line of the screen
set ruler		" Show cursor position in status line

set clipboard=unnamed,unnamedplus,autoselect " y and d put stuff into system clipboard (so that other apps can see it)


set autoread  		" Automatically re-read file changed outside vim
set autowrite		" Automatically save before commands like :next and :make

set scrolloff=100	" Keep cursor at the centre of screen

" Searching
set hlsearch 		" Highlight all matching string
" Map <C-L> (redraw screen) to also turn off search highlighting until the  next search
nnoremap <C-L> :nohl<CR><C-L> 
set ignorecase 		" Use case insensitive search
set smartcase 		" Override the 'ignorecase' option if the search pattern contains upper case characters
set incsearch		" Search as you type
 
" Allow backspacing over autoindent, line breaks and start of insert action
set backspace=indent,eol,start
 
" When opening a new line and no filetype-specific indenting is enabled, keep
" the same indent as the line you're currently on. Useful for READMEs, etc.
set autoindent

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
