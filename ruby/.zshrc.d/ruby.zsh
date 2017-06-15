export RBENV_ROOT="${ZIT_MODULES_PATH}/rbenv"

path=(
  ${RBENV_ROOT}/bin
  ${path}
)

zit-in "https://github.com/rbenv/rbenv.git" "rbenv"
zit-in "https://github.com/rbenv/ruby-build" "rbenv/plugins/ruby-build"

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
