# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Languages
alias g++='g++ -O2 -std=c++0x -Wno-unused-result -D_GLIBCXX_DEBUG -DDEBUG -Wall -Wshadow'
alias ghc='ghc -O2 -fforce-recomp -rtsopts -fwarn-name-shadowing -auto-all -with-rtsopts="-A8m -K512m -p"'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias apt-get='apt-fast'
alias cp='cp -v'
alias mv='mv -v'
alias rm='rm -v'
alias rmdir='rmdir -v'

alias lss='ls -t --color=always | head'
alias pingcheck='ping www.google.com'
alias lastfile='ls --sort=t -c --color=yes --file-type *.cpp -r | head -n 1 | xargs  vim'
alias readJson='python -mjson.tool < '

# Subversioning
alias gs='git status'
alias hgdiff='hg extdiff -p vimdiff'
alias hglog='hg log | less'

# export PATH=~/.cabal/bin:/opt/cabal/1.18/bin:/opt/ghc/7.8.4/bin:$PATH
export PATH=~/.cabal/bin:/opt/ghc/7.8.4/bin:/opt/idea-IC-141.1532.4/bin:$PATH


