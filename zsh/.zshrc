# Check if zplug is installed
if [[ ! -d ~/.zplug ]]; then
  git clone https://github.com/zplug/zplug ~/.zplug
  source ~/.zplug/init.zsh && zplug update --self
fi

# Source zplug/zplug
source ~/.zplug/init.zsh

# prezto
zplug "modules/environment", from:prezto
zplug "modules/terminal", from:prezto
zplug "modules/editor", from:prezto
zplug "modules/history", from:prezto
zplug "modules/directory", from:prezto
zplug "modules/spectrum", from:prezto
zplug "modules/utility", from:prezto
zplug "modules/completion", from:prezto
zplug "modules/ssh", from:prezto, nice:5
zplug "modules/tmux", from:prezto
zplug "modules/git", from:prezto
zplug "modules/ruby", from:prezto
zplug "modules/rails", from:prezto
zplug "modules/python", from:prezto

# zsh-users
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting", nice:17
# using the keybindings from prezto, however loads the newer
# functions from zsh-users
zplug "modules/history-substring-search", from:prezto, nice:18
zplug "zsh-users/zsh-history-substring-search", nice:19

# misc
zplug "rupa/z", use:z.sh
zplug "junegunn/fzf", use:"shell/*.zsh", nice:10
zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:fzf
zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme

# source plugin/shell configuration
source ~/.zconfig

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load
