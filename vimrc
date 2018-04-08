set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Vim script for text filtering and alignment
" Syntax highlighting, matching rules and mappings for the original Markdown and extensions.
" Depends on tabular
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'

Plugin 'git@github.com:vim-airline/vim-airline.git'

" Interactive command execution in Vim.
Plugin 'git@github.com:Shougo/vimproc.vim.git'

" Happy Haskell programming on Vim, powered by ghc-mod
Plugin 'git@github.com:eagletmt/ghcmod-vim.git'

" A completion plugin for Haskell, using ghc-mod
Plugin 'git@github.com:eagletmt/neco-ghc.git'

" snipMate.vim aims to be a concise vim script that implements
" some of TextMate's snippets features in Vim.
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'garbas/vim-snipmate'
Plugin 'honza/vim-snippets'

" Syntax checking hacks for vim
" Plugin 'git@github.com:scrooloose/syntastic.git'

" Next generation completion framework after neocomplcache
Plugin 'git@github.com:Shougo/neocomplete.vim.git'

" Fuzzy file, buffer, mru, tag, etc finder
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

" One colorscheme pack to rule them all!
Plugin 'git@github.com:flazz/vim-colorschemes.git'

" Vim plugin that displays tags in a window, ordered
" by scope http://majutsushi.github.com/tagbar/
Plugin 'git@github.com:majutsushi/tagbar.git'

" Perform all your vim insert mode completions with Tab
Plugin 'git@github.com:ervandew/supertab.git'

" A vim plugin to give you some slime. (Emacs)
" http://technotales.wordpress.com/2007/10/03/like-slime-for-vim/
Plugin 'git@github.com:jpalardy/vim-slime.git'

" Rainbow parentheses improved, shorter code, no level limit,
" smooth and fast, powerful configuration.
Plugin 'git@github.com:luochen1990/rainbow.git'

" vim-erlang-omnicomplete is an Erlang autocompletion plugin for Vim.
"Plugin 'https://github.com/vim-erlang/vim-erlang-omnicomplete'

" Collection of heuristics to help quickly detect modifications in vim buffers
Plugin 'git@github.com:let-def/vimbufsync.git'

" Interactive theorem proving with Coq in vim.
Plugin 'git@github.com:the-lambda-church/coquille.git'

" Manage your 'runtimepath' with ease. In practical terms, pathogen.vim makes it
" super easy to install plugins and runtime files in their own private
" directories.
Plugin 'git@github.com:tpope/vim-pathogen.git'

" Syntax checking hacks for vim
Plugin 'git@github.com:vim-syntastic/syntastic.git'

" F# bindings for vim
Plugin 'git@github.com:fsharp/vim-fsharp.git'

" All of your Plugins must be added before the following line
call vundle#end()            " required
execute pathogen#infect()
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on

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

" Enable powerline #Method
"set rtp+=/usr/local/lib/python2.7/dist-packages/powerline/bindings/vim/


set autoread  		"automaticall re-read file changed outside vim
set scrolloff=100
set autowrite		" Automatically save before commands like :next and :make
"set backspace=indent,eol,start  " set backspace
set ignorecase		" Do case insensitive matching
set incsearch		" Incremental search
set smartcase		" Do smart case matching
"set shiftround  " when shifting a non-aligned set of lines, alignt them to the next tabstop
set number
set showmode
set textwidth=80
set smarttab
set smartindent
set autoindent
set softtabstop=4
set tabstop=4
set shiftwidth=4
set expandtab
"set nowrap
set wrap
set mouse=a
set history=1000
set completeopt=menuone,menu,longest

set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=longest,list,full
set wildmenu
set completeopt+=longest

set pastetoggle=<F12> " <F12> toggles paste mode
set t_Co=256
set cmdheight=1
set laststatus=2
set clipboard=unnamed,unnamedplus,autoselect " y and d put stuff into system clipboard (so that other apps can see it)
" tell the folds to fold on file open
set foldmethod=marker

"Adding template for cpp, java and haskell and rest
autocmd! BufNewFile * silent! 0r $HOME/.vim/skel/skel.%:e

colorscheme molokai
colorscheme solarized

" Use default wrap option in diffmode `wrap<` uses default wrap setting
" autocmd FilterWritePre * if &diff | setlocal wrap< | endif
autocmd FilterWritePre * if &diff | setlocal wrap | endif

" Disbale folding in markdown mode
let g:vim_markdown_folding_disabled=1

map <Leader>n <plug>NERDTreeTabsToggle<CR>
let g:nerdtree_tabs_open_on_console_startup = 1
let g:nerdtree_tabs_open_on_new_tab = 1

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

" Setting tmux as default shell
let g:slime_target = "tmux"
" Set default
let g:slime_default_config = {"socket_name": "default", "target_pane": "1"}
" Don't ask for congif
"let g:slime_dont_ask_default = 1

" Syntastic related settings
map <Leader>s :SyntasticToggleMode<CR>
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Enable C++ 0x mode
let g:syntastic_cpp_compiler = 'clang++'
"let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'
"let g:syntastic_cpp_compiler = 'g++'
let g:syntastic_cpp_compiler_options = ' -std=c++14'

" ghc-mod completion ability
map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>

let g:SuperTabDefaultCompletionType = '<c-x><c-o>'

" Supertab related settings
if has("gui_running")
  imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
else " no gui
  if has("unix")
    inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
  endif
endif

" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 2
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
let g:necoghc_enable_detailed_browse = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

"autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
"autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
"autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
"autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
"autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Tabularize settings for haskell
" For example typing a + - on the case arm branches will align on the right
" arrow.
let g:haskell_tabular = 1
vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a- :Tabularize /-><CR>

"nmap <F8> :TagbarToggle<CR>
nmap <Leader>t :TagbarToggle<CR>

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

let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
\ }

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
"  set makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ g++\ -O2\ -g\ -Wall\ -std=c++14\ -W\ -o%.bin\ %;fi;fi
  setlocal makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ clang++\ -O2\ -g\ -std=c++14\ -Wno-unused-result\ -D_GLIBCXX_DEBUG\ -DDEBUG\ -Wall\ -Wshadow\ %;fi;fi
  "set cindent
  "set tw=0
"  set nowrap
endfunction

" Haskell
function! HSKSET()
  setlocal makeprg=if\ \[\ -f\ \"Makefile\"\ \];then\ make\ $*;else\ if\ \[\ -f\ \"makefile\"\ \];then\ make\ $*;else\ ghc\ -O2\ -fforce-recomp\ -rtsopts\ -fwarn-name-shadowing\ -fwarn-incomplete-patterns\ -auto-all\ -Wall\ -with-rtsopts=\"-K512m\ -A8m\"\ %;fi;fi
  setlocal background=dark
  colorscheme solarized
  setlocal ts=2
  setlocal shiftwidth=2
  setlocal expandtab          " Expand tabs as spaces
  setlocal smartindent
"  set nowrap
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

" Makefile
function! MAKEFILESET()
  set tw=0
"  set nowrap
  " in a Makefile we need to use <Tab> to actually produce tabs
  set noet
  set sts=4
endfunction

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

        autocmd BufNewFile,BufRead *.coq setfiletype coq
augroup END

filetype plugin on

" Autocommands for all languages:
autocmd Filetype gitcommit setlocal spell textwidth=72
autocmd FileType c      call CSET()
autocmd FileType C      call CPPSET()
autocmd FileType cc     call CPPSET()
autocmd FileType cpp    call CPPSET()
autocmd FileType make   call MAKEFILESET()
autocmd FileType haskell    call HSKSET()
autocmd FileType fsharp     call FSHARPSET()

" Maps Coquille commands to CoqIDE default key bindings
au FileType coq call coquille#CoqideMapping()
" Maps Coquille commands to <F2> (Undo), <F3> (Next), <F4> (ToCursor)
au FileType coq call coquille#FNMapping()
au FileType coq CoqLaunch
"}}}

call ShowTralingSpace()
