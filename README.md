# dotfiles

A repository with my personal configuration files. Managed using GNU
[stow][stow].

## How to use

Clone this repository (including submodules) in `~/.dotfiles` directory, e.g.:

    $ git clone --recursive https://github.com/thiagokokada/dotfiles ~/.dotfiles

Use `stow` to manage symlinks, e.g.:

    $ cd ~/.dotfiles
    $ stow zsh

`(n)vim` and its dependencies is managed using [vim-plug][plug], so you need
to use:

    :PlugInstall

inside `(n)vim` command mode to install all plugins.

[stow]: https://www.gnu.org/software/stow/
[plug]: https://github.com/junegunn/vim-plug
[pctrl]: https://packagecontrol.io/installation
