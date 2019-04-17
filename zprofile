export PATH="/usr/local/opt/grep/libexec/gnubin:$PATH"

export PATH="/usr/local/Cellar/postgresql@9.6/9.6.10/bin:$PATH"
export PATH="$HOME/.rbenv/bin:$PATH"

# RUST
export PATH="$HOME/.cargo/bin:$PATH"

# postgres
export PATH="/usr/local/opt/postgresql@9.6/bin:$PATH"

# SDL
export LIBRARY_PATH="$LIBRARY_PATH:/usr/local/lib"

# homebrew don't spy
export HOMEBREW_NO_ANALYTICS=1

eval "$(rbenv init -)"

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

# Setting fd as the default source for fzf
export FZF_DEFAULT_COMMAND='fd --type f'

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/utopia/.sdkman"
[[ -s "/Users/utopia/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/utopia/.sdkman/bin/sdkman-init.sh"
