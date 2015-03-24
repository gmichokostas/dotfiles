if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi

#if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
#    source "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh"
#fi

# parse_git_branch() {
#
#     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
#
# }

#source $(brew --prefix)/etc/bash_completion.d/git-prompt.sh
#PS1="\h:\W \u\$(__git_ps1 \" (%s) \")\$ "
# \[\033[01;32m\]\u\[\033[01;34m\]::\[\033[01;31m\]\h
#PS1="\[\033[00;34m\]{ \[\033[01;34m\]\w \[\033[00;34m\]}\[\033[01;32m\]$ \[\033[00m\]\$(__git_ps1 \"(%s) \")"
#export PS1="\u@\h \W\[\033[32m\]\$(parse_git_branch)\[\033[00m\] $ "

alias ls='ls -G'
alias l='ls -lh'
alias la='l -a'
alias c='clear'
alias rm='rm -v'
alias mv='mv -v'
alias grep='grep --color=always'
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs "$@"'
alias updb='sudo /usr/libexec/locate.updatedb'
alias pg_start="pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start"
alias pg_stop="pg_ctl -D /usr/local/var/postgres stop -s -m fast"
alias code="cd Code"
alias gl="git log --graph --pretty=format:'%Cred%h%Creset %s%Cgreen%d %C(yellow)(%cr) %Cblue[%an]%Creset' --abbrev-commit --date=relative --all"

source ~/.profile

export GOPATH=$HOME/Code/Go
export PATH="$GOPATH/bin:$PATH"
export PATH="$PATH:/usr/local/opt/go/libexec/bin"

export PATH=$PATH:/usr/local/share/python:/usr/local/opt/coreutils/libexec/gnubin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/george/android-sdk/platform-tools:/Users/george/android-sdk/tools
export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"



[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# source ~/.git-completion.bash
# source ~/.git-prompt.sh
#
# MAGENTA="\[\033[0;35m\]"
# YELLOW="\[\033[0;33m\]"
# BLUE="\[\033[34m\]"
# LIGHT_GRAY="\[\033[0;37m\]"
# CYAN="\[\033[0;36m\]"
# GREEN="\[\033[0;32m\]"
# GIT_PS1_SHOWDIRTYSTATE=true
# export LS_OPTIONS='--color=auto'
# export CLICOLOR='Yes'
# export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
#
# export PS1=$LIGHT_GRAY"\u@\h"'$(
#     if [[ $(__git_ps1) =~ \*\)$ ]]
#     # a file has been modified but not added
#     then echo "'$YELLOW'"$(__git_ps1 " (%s)")
#     elif [[ $(__git_ps1) =~ \+\)$ ]]
#     # a file has been added, but not commited
#     then echo "'$MAGENTA'"$(__git_ps1 " (%s)")
#     # the state is clean, changes are commited
#     else echo "'$CYAN'"$(__git_ps1 " (%s)")
#     fi)'$BLUE" \w"$GREEN": "

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
