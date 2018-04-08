" Set 'nocompatible' to ward off unexpected things that your distro might
" have made, as well as sanely reset options when re-sourcing .vimrc
set nocompatible

" Attempt to determine the type of a file based on its name and possibly its
" contents. Use this to allow intelligent auto-indenting for each filetype,
" and for plugins that are filetype specific.
filetype indent plugin on
 
syntax on 		"Enable syntax highlighting

set number		" Always show line number
set hlsearch 		" Highlight searches
set autoread  		" Automatically re-read file changed outside vim
set autowrite		" Automatically save before commands like :next and :make
set scrolloff=100	" Keep cursor at the centre of screen

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
