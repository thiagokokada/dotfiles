# dotfiles
 
A repository with my personal configuration files. Managed using GNU [stow][stow].

## How to use

Clone this repository somewhere in your home, e.g.:

    $ cd ~
    $ git clone https://github.com/m45t3r/dotfiles .dotfiles

Use `stow` to manage symlinks, e.g.:

    $ cd ~/.dotfiles
    $ stow zsh

`vim` and its dependencies is managed using [Vundle][vundle], so you need to use:

    :PluginInstall

inside `vim` command mode to install all plugins.

[stow]: https://www.gnu.org/software/stow/
[vundle]: https://github.com/VundleVim/Vundle.vim
