alias gs='git status'

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

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias apt-get='apt-fast'
alias hgdiff='hg extdiff -p vimdiff'

# export PATH=~/.cabal/bin:/opt/cabal/1.18/bin:/opt/ghc/7.8.4/bin:$PATH
export PATH=~/.cabal/bin:/opt/ghc/7.8.4/bin:/opt/idea-IC-141.1532.4/bin:$PATH
