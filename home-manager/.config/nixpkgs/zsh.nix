{ ... }:

{
  programs.zsh = {
    enable = true;
    autocd = true;
    defaultKeymap = "viins";
    enableCompletion = true;
    enableAutosuggestions = true;

    history = {
      ignoreDups = true;
      ignoreSpace = true;
      expireDuplicatesFirst = true;
      share = true;
    };

    sessionVariables = {
      DOTFILES_PATH = "$HOME/.dotfiles";
      # Reduce time to wait for multi-key sequences
      KEYTIMEOUT = 1;
      # Set right prompt to show time
      RPROMPT = "%F{8}%*";
      # zsh-users config
      ZSH_AUTOSUGGEST_USE_ASYNC = 1;
      ZSH_HIGHLIGHT_HIGHLIGHTERS = ["main" "brackets" "cursor"];
    };

    shellAliases = {
      "reload!" = "source $HOME/.zshrc";
      dotfiles = "cd $DOTFILES_PATH";
      dotfiles-pull = "git -C $DOTFILES_PATH pull";
    };

    initExtraBeforeCompInit = ''
      # zimfw config
      zstyle ':zim:input' double-dot-expand yes
      zstyle ':zim:ssh' ids /dev/null
    '';

    initExtra = ''
      # helpers
      close-fd() { "$@" </dev/null &>/dev/null }
      run-bg() { "$@" </dev/null &>/dev/null &! }
      open() { run-bg xdg-open "$@" }
      restart() { pkill "$1"; run-bg "$@" }
      get-ip() { curl -Ss "https://ifconfig.me" }
      get-ip!() { curl -Ss "https://ipapi.co/$(get-ip)/yaml" }

      # try to correct the spelling of commands
      setopt correct
      # Disable C-S/C-Q
      setopt noflowcontrol

      # Edit in vim
      autoload -U edit-command-line
      zle -N edit-command-line
      bindkey -M vicmd v edit-command-line

      # zsh-history-substring-search
      bindkey "$terminfo[kcuu1]" history-substring-search-up
      bindkey "$terminfo[kcud1]" history-substring-search-down

      # source contents from ~/.zshrc.d/*.zsh
      for file in $HOME/.zshrc.d/*.zsh; do
        [[ -f "$file" ]] && source "$file"
      done

      # load after ~/.zshrc.d files to make sure that ~/.local/bin is the first in $PATH
      export PATH="$HOME/.local/bin:$PATH"
    '';

    plugins = [
      # TODO: Remove after migration
      {
        name = "zit";
        src = fetchGit "https://github.com/thiagokokada/zit";
        file = "zit.zsh";
      }
      {
        name = "zim-completions";
        src = fetchGit "https://github.com/zimfw/completion";
        file = "init.zsh";
      }
      {
        name = "zim-environment";
        src = fetchGit "https://github.com/zimfw/environment";
        file = "init.zsh";
      }
      {
        name = "zim-input";
        src = fetchGit "https://github.com/zimfw/input";
        file = "init.zsh";
      }
      {
        name = "zim-git";
        src = fetchGit "https://github.com/zimfw/git";
        file = "init.zsh";
      }
      {
        name = "zim-ssh";
        src = fetchGit "https://github.com/zimfw/ssh";
        file = "init.zsh";
      }
      {
        name = "zim-utility";
        src = fetchGit "https://github.com/zimfw/utility";
        file = "init.zsh";
      }
      {
        name = "pure";
        src = fetchGit "https://github.com/sindresorhus/pure";
      }
      {
        name = "autopair";
        src = fetchGit "https://github.com/hlissner/zsh-autopair";
      }
      {
        name = "zsh-completions";
        src = fetchGit "https://github.com/zsh-users/zsh-completions";
      }
      {
        name = "zsh-syntax-highlighting";
        src = fetchGit "https://github.com/zsh-users/zsh-syntax-highlighting";
      }
      {
        name = "zsh-history-substring-search";
        src = fetchGit "https://github.com/zsh-users/zsh-history-substring-search";
      }
    ];
  };

  programs.autojump.enable = true;
  programs.dircolors.enable = true;
  programs.fzf = {
    enable = true;
    fileWidgetOptions = [ "--preview 'head {}'" ];
    historyWidgetOptions = [ "--sort" ];
  };
}
