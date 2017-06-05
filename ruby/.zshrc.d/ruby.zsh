export RBENV_ROOT="$HOME/.rbenv"
export PATH="$RBENV_ROOT/bin:$PATH"

if [ ! -d "$RBENV_ROOT" ]; then
  git clone https://github.com/rbenv/rbenv.git "$RBENV_ROOT"
  git clone https://github.com/rbenv/ruby-build.git "$RBENV_ROOT/plugins/ruby-build"
fi

eval "$(rbenv init --no-rehash - zsh)"

alias be="bundle exec"
alias bl="bundle list"
alias bp="bundle package"
alias bo="bundle open"
alias bout="bundle outdated"
alias bu="bundle update"
alias bi="bundle install"
alias bcn="bundle clean"
alias coverage="COVERAGE=true bundle exec rspec"
