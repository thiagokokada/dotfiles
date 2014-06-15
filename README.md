# dotfiles
 
Misc configuration files from my $HOME directory ;) ...

## Introduction

This is my repository containing some configuration files that I use on my *nix systems. Since this is to be used by myself, I sometimes hardcode strings (like my name/e-mail on my .gitconfig file), so this repo probably shouldn't be used by you without modifications.
 
## Dependencies

This repository includes configuration files for the following programs (so you need them installed to use this repo). You can, of course, install only the things you want/need, but you will need to copy/link the files/folders in "()" to make it work:

  * mpv (.mpv)
  * Sublime Text 3 (.config/sublime-text-3)
  * vim/gvim (.vim/.vimrc/.gvimrc)
  * zsh (.zshrc/.zsh/{oh-my-zsh/zsh-syntax-highlighting/zsh-completions})

Of course you need Git too ;) . If you do want to use my .gitconfig, don't forget to change the e-mail and name, unless you want to commit things with my name.
 
## Instalation

Thanks to Dotbot it's very easy to use this repository. Simple clone this repo and execute the install script (you need Python 2/3 installed in your system):

```
  $ ./install
```

Of course you can simple copy/symlink which config file that you want. But if you do that, you're on your own (e.g. managing updates).

After instalation you need to run 

```
  :VundleInstall
```

on Vim command mode to install all plugins. There is no need to do something similar to Oh-My-Zsh or Package Control since they're self contained.

