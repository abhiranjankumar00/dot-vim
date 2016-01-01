set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin() "{{{
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Full path fuzzy file, buffer, mru, tag, ... finder for Vim.
Plugin 'git@github.com:kien/ctrlp.vim.git'

" A tree explorer plugin for vim.
Plugin 'git@github.com:scrooloose/nerdtree.git'

" NERDTree and tabs together in Vim, painlessly
Plugin 'git@github.com:jistr/vim-nerdtree-tabs.git'

" Vim plugin, provides insert mode auto-completion for quotes, parens, brackets, etc.
Plugin 'git@github.com:Raimondi/delimitMate.git'

" Vim motions on speed!
Plugin 'git@github.com:easymotion/vim-easymotion.git'

" Vim plugin for intensely orgasmic commenting
Plugin 'git@github.com:scrooloose/nerdcommenter.git'

" Vim script for automatically detecting indent settings
Plugin 'git@github.com:ciaranm/detectindent.git'

" one colorscheme pack to rule them all!
Plugin 'git@github.com:flazz/vim-colorschemes.git'

" Syntax checking hacks for vim
Plugin 'git@github.com:scrooloose/syntastic.git'

" Vim plugin that displays tags in a window, ordered
" by scope http://majutsushi.github.com/tagbar/
Plugin 'git://github.com/majutsushi/tagbar'

" Happy Haskell programming on Vim, powered by ghc-mod
" http://www.vim.org/scripts/script.php?script_id=4473
Plugin 'git@github.com:eagletmt/ghcmod-vim.git'

" Interactive command execution in Vim.
Plugin 'git@github.com:Shougo/vimproc.vim.git'

" Vim script for text filtering and alignment
" Syntax highlighting, matching rules and mappings for the original Markdown and extensions.
" Depends on tabular
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'

" F# bindings for vim
Plugin 'git@github.com:fsharp/vim-fsharp.git'

" A code-completion engine for Vim
Plugin 'git@github.com:Valloric/YouCompleteMe.git'

" A vim plugin to give you some slime. (Emacs) http://technotales.wordpress.com/2007/10/03/like-slime-for-vim/
Plugin 'git@github.com:jpalardy/vim-slime.git'

" vim bundle for Racket, for use with Pathogen
Plugin 'git@github.com:wlangstroth/vim-racket.git'

" rainbow parentheses improved, shorter code, no level limit, smooth and fast, powerful configuration.
Plugin 'git@github.com:luochen1990/rainbow.git'

" All of your Plugins must be added before the following line
call vundle#end()            " required "}}}

filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

" Enable powerline #Method
set rtp+=/usr/local/lib/python2.7/dist-packages/powerline/bindings/vim/
set laststatus=2
set t_Co=256

" tell the folds to fold on file open
set foldmethod=marker

function! ShowTralingSpace()
    " Highligting trailing whitespaces
    highlight ExtraWhitespace ctermbg=red guibg=red
    match ExtraWhitespace /\s\+$/
endfunction
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

"let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll|o|d|7z|swp)$',
  \ }

"Adding template for cpp, java and haskell and rest
autocmd! BufNewFile * silent! 0r $HOME/.vim/skel/skel.%:e

" Vim5 and later versions support syntax highlighting. Uncommenting the next
" line enables syntax highlighting by default.
if has("syntax")
  syntax on
endif

" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Uncomment the following to have Vim load indentation rules and plugins
" according to the detected filetype.
if has("autocmd")
  filetype plugin indent on
endif

set number		    " Set line numbers
set wildmenu		" show all options on pressing tab
set autoread  		"automaticall re-read file changed outside vim
set mouse=a		" Enable mouse usage (all modes)
set scrolloff=100
"set smartindent
set autowrite		" Automatically save before commands like :next and :make
set backspace=indent,eol,start  " set backspace

set ignorecase		" Do case insensitive matching
set incsearch		" Incremental search
set smartcase		" Do smart case matching
set wrap

set shiftwidth=4
set expandtab          " Expand tabs as spaces
set shiftround  " when shifting a non-aligned set of lines, alignt them to the next tabstop
set softtabstop=4

set clipboard=unnamed,unnamedplus " y and d put stuff into system clipboard (so that other apps can see it)

"Solarized settings
syntax enable

set pastetoggle=<F12> " <F12> toggles paste mode

" Disbale folding in markdown mode
let g:vim_markdown_folding_disabled=1

map <Leader>n <plug>NERDTreeTabsToggle<CR>
let g:nerdtree_tabs_open_on_console_startup = 1

" Disbale it for gvimdiff
let g:nerdtree_tabs_no_startup_for_diff = 0
let g:nerdtree_tabs_open_on_gui_startup = 0

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

"nmap <F10> :TagbarToggle<CR>
map <Leader>t :TagbarToggle<CR>

" fugitive statusline usage (http://stackoverflow.com/questions/5983906/vim-conditionally-use-fugitivestatusline-function-in-vimrc)
set statusline+=%{exists('g:loaded_fugitive')?fugitive#statusline():''}

" Syntastic settings (https://github.com/scrooloose/syntastic#installation)
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Disbale on-the-fly syntax checking for fsharp
let g:fsharp_only_check_errors_on_write = 1

" Setting tmux as default for slime
let g:slime_target = "tmux"

" Enable rainbow parenthesis
let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle
let lightcolors =  ['lightblue', 'lightyellow', 'red', 'darkgreen', 'darkyellow', 'lightred', 'yellow', 'cyan', 'magenta', 'white']
let darkcolors = ['DarkBlue', 'Magenta', 'Black', 'Red', 'DarkGray', 'DarkGreen', 'DarkYellow']
let g:rainbow_conf = {
   \   'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick'],
   \   'ctermfgs': lightcolors,
   \   'operators': '_,_',
   \   'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
   \   'separately': {
   \       '*': {},
   \       'tex': {
   \           'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/'],
   \       },
   \       'lisp': {
   \           'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick', 'darkorchid3'],
   \       },
   \       'vim': {
   \           'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold', 'start=/(/ end=/)/ containedin=vimFuncBody', 'start=/\[/ end=/\]/ containedin=vimFuncBody', 'start=/{/ end=/}/ fold containedin=vimFuncBody'],
   \       },
   \       'html': {
   \           'parentheses': ['start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold'],
   \       },
   \       'css': 0,
   \   }
   \}

" To toggle between active or passive type checking we can enable the following key bindings:
map <silent> <Leader>e :Errors<CR>
map <Leader>s :SyntasticToggleMode<CR>

" LaTeX"{{{
function! TEXSET()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ pdfcslatex\ -file-line-error-style\ %;fi;fi
  set errorformat=%f:%l:\ %m
endfunction

" C/C++:
function! CSET()
  "set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ gcc\ -o%.bin\ %;fi;fi
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ gcc\ -Wno-unused-result\ -Wreturn-type\ -Wmain\ -Werror=return-type\ -Werror=main\ -pipe\ -O3\ -std=c99\ %;fi;fi
"  set errorformat=%f:%l:\ %m
  set cindent
  set tw=0
"  set nowrap
endfunction

function! CPPSET()
"  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ g++\ -O2\ -g\ -Wall\ -std=c++0x\ -W\ -o%.bin\ %;fi;fi
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ g++\ -O2\ -g\ -std=c++0x\ -Wno-unused-result\ -D_GLIBCXX_DEBUG\ -DDEBUG\ -Wall\ -Wshadow\ %;fi;fi
  "set cindent
  "set tw=0
"  set nowrap
endfunction

" Haskell
function! HSKSET()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ ghc\ -O2\ -fforce-recomp\ -rtsopts\ -fwarn-name-shadowing\ -fwarn-incomplete-patterns\ -auto-all\ -Wall\ -with-rtsopts=\"-K512m\ -A8m\"\ %;fi;fi
  set background=dark
  colorscheme solarized
"  set nowrap
endfunction

"  COQ
function! COQSET()
  "set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ ghc\ -O2\ -fforce-recomp\ -rtsopts\ -fwarn-name-shadowing\ -prof\ -auto-all\ -with-rtsopts=\"-K512m\ -A8m\"\ %;fi;fi
  set ts=2
  set shiftwidth=2
  set expandtab          " Expand tabs as spaces
"  set nowrap
endfunction

"SML
function! SMLSET()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ sml\ %;fi;fi
  set ts=2
  set shiftwidth=2
  set expandtab          " Expand tabs as spaces
"  set nowrap
endfunction

" XQuery
function! XQUERYSET()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ basex\ %;fi;fi
  set ts=2
  set shiftwidth=2
  set expandtab          " Expand tabs as spaces
"  set nowrap
endfunction

function! OCAMLSET()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ ocamlc\ str.cma\ %;fi;fi
  set ts=2
  set shiftwidth=2
  set expandtab          " Expand tabs as spaces
"  set nowrap
endfunction

" JSON
function! JSON()
  set autoindent
  set formatoptions=tcq2l
  set textwidth=78 shiftwidth=2
  set softtabstop=4
  set expandtab
  set foldmethod=syntax
endfunction

" Ruby
function! RUBYSET()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ ruby\ %;fi;fi
  set ts=4
  set shiftwidth=4
  set expandtab          " Expand tabs as spaces
"  set nowrap
endfunction

" AWK
function! AWKSET()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ awk\ -f\ %;fi;fi
  set cindent
  set tw=0
endfunction

function! CLJSET()
  "set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ java\ -Xss128m\ -cp\ /usr/share/java/clojure-1.4.0.jar:/ clojure.main\ %;fi;fi
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ clojure\ %;fi;fi
  set ts=4
  set shiftwidth=4
  set expandtab          " Expand tabs as spaces
"  set nowrap
endfunction

" Java
function! JAVASET()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ javac\ -cp\ \"/usr/share/java/json-simple-1.1.1.jar:/usr/share/java/junixsocket.jar\"\ -g\ %;fi;fi
  set errorformat=%f:%l:\ %m
  set cindent
  set tw=0
  set expandtab          " Expand tabs as spaces
"  set nowrap
endfunction

function! SCALASET()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ scalac\ -g\ %;fi;fi
  set errorformat=%f:%l:\ %m
  set cindent
  set tw=0
  set expandtab          " Expand tabs as spaces
"  set nowrap
endfunction

function! CSHARPSET()
  "set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ gmcs\ %;fi;fi
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ dmcs\ -unsafe\ -r:System.Numerics\ -r:System.Data\ -r:Mono.Data.Sqlite\ -r:/usr/lib/cli/Newtonsoft.Json-5.0/Newtonsoft.Json.dll\ %;fi;fi
  "
  set cindent
endfunction

function! FSHARPSET()
  "set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ gmcs\ %;fi;fi
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ fsharpc\ -r:/usr/lib/cli/FSharp.Core-4.3/FSharp.Core.dll\ --noframework\ %;fi;fi

  "set smartindent
  set cindent 
  set shiftwidth=4
  set expandtab          " Expand tabs as spaces
  set shiftround  " when shifting a non-aligned set of lines, alignt them to the next tabstop
endfunction

function! MARKDOWNSET()
  "set shiftwidth=4
  set expandtab          " Expand tabs as spaces
  "set softtabstop=4
  set wrap
endfunction

function! JAVASCRIPT()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ node\ %;fi;fi
  set cindent
endfunction

function! PYTHONSET()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ python\ %;fi;fi
  set cindent
  set shiftwidth=4
  set expandtab          " Expand tabs as spaces
endfunction

function! CLISPSCRIPT()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ clisp\ %;fi;fi
  set cindent
  set shiftwidth=4
  set expandtab          " Expand tabs as spaces
endfunction

function! SCHEMESCRIPT()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ racket\ %;fi;fi
  "set cindent
  set shiftwidth=2
  set expandtab          " Expand tabs as spaces
endfunction

" Pascal
function! PPSET()
  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ fpc\ -g\ -O2\ -o\%.bin\ %;fi;fi
  set errorformat=%f:%l:\ %m
  set tw=0
"  set nowrap
endfunction

" vim scripts
function! VIMSET()
  set tw=0
"  set nowrap
  set comments+=b:\"
endfunction

" Makefile
function! MAKEFILESET()
  set tw=0
"  set nowrap
  " in a Makefile we need to use <Tab> to actually produce tabs
  set noet
  set sts=4
endfunction

" HTML/PHP
function! HTMLSET()
  set tw=0
"  set nowrap
endfunction

" Asymptote
function! ASYSET()
  runtime asy.vim " find this somewhere and place it into ~/.vim/ for syntax hl to work
  set tw=0
"  set nowrap
  set makeprg=asy\ -noV\ -fpdf\ %\ -o\ %.pdf
  set errorformat=%f:\ %l.%c:\ %m
endfunction

" Python
function! PYSET()
  set tw=0
"  set nowrap
endfunction
"}}}

"FileType settings "{{{
" Asymptote does not get recognized by default, fix it
augroup filetypedetect
	autocmd BufNewFile,BufRead *.asy setfiletype asy
	autocmd BufNewFile,BufRead *.scala setfiletype scala
	autocmd BufNewFile,BufRead *.hs setfiletype haskell
	autocmd BufNewFile,BufRead *.idr setfiletype haskell
	autocmd BufNewFile,BufRead *.lhs setfiletype haskell
	autocmd BufNewFile,BufRead *.json setfiletype json
        autocmd BufNewFile,BufRead *.agda setfiletype agda
        autocmd BufNewFile,BufRead *.md setfiletype markdown
        "autocmd BufRead,BufNewFile *.fs set filetype=fs
        "autocmd BufRead,BufNewFile *.fsx set filetype=fs
augroup END


filetype plugin on

" Autocommands for all languages:
autocmd Filetype gitcommit setlocal spell textwidth=72
autocmd FileType vim    call VIMSET()
autocmd FileType c      call CSET()
autocmd FileType C      call CPPSET()
autocmd FileType cc     call CPPSET()
autocmd FileType cpp    call CPPSET()
autocmd FileType java   call JAVASET()
autocmd FileType tex    call TEXSET()
autocmd FileType pascal call PPSET()
autocmd FileType make   call MAKEFILESET()
autocmd FileType html   call HTMLSET()
autocmd FileType php    call HTMLSET()
autocmd FileType asy    call ASYSET()
autocmd FileType python call PYSET()
autocmd FileType cs     call CSHARPSET()
autocmd FileType python call PYTHONSET()
autocmd FileType ruby   call RUBYSET()
autocmd FileType lisp   call CLISPSCRIPT()
autocmd FileType scheme call SCHEMESCRIPT()

autocmd FileType haskell    call HSKSET()
autocmd FileType clojure    call CLJSET()
autocmd FileType forth      call FSHARPSET()
autocmd FileType fsharp     call FSHARPSET()
autocmd FileType scala      call SCALASET()
autocmd FileType ocaml      call OCAMLSET()
autocmd FileType markdown   call MARKDOWNSET()
autocmd FileType javascript call JAVASCRIPT()
autocmd FileType json       call JSON()
autocmd FileType xquery     call XQUERYSET()
autocmd FileType coq        call COQSET()
autocmd FileType sml        call SMLSET()
autocmd FileType awk        call AWKSET()
"}}}

command! Status echo "All systems are go!"
