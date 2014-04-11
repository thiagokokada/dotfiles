dotfiles
========
 
Misc configuration files from my $HOME directory ;) ...
 
This is my repository containing some configuration files that I use on my *nix
systems. Since this is to be used by myself, I sometimes hardcode strings (like
my name/e-mail on my .gitconfig file), so this repo probably shouldn't be used
by you without some modifications.
 
Why the README.md file is hidden? This is because this repo is supposed to be
clonned on $HOME directory directly, instead to clone to some folder and use
symlinks/shell voodoo to put the files on the correct place. This has some
advantages (it's easier to manage files, especially if you use Zsh+Git
integration like me), but this also means I can't put a README.md/LICENSE/etc
files on this repo or these files would appear on my $HOME directory. And well,
this is a dotfile repo, so it makes sense to README.md to be a dotfile too.
 
To install, simple clone this repository on your $HOME directory. After that,
you need to do a:

```
  $ git submodule update --init --remote
```

on your $HOME directory since I use git submodules to all plugins that can be
found on a Git repository, making the update process less painful (I can just
use "git submodule update --remote" to update everything). 

As a alternative, you can clone this repo anywhere you want and create
symlinks or copy anything you think you want to use. This is probably easier,
but you lose the ability to auto-track your modifications with Git. This is
why I recommend you to fork this repo and make your changes on your own
repository

After
instalation you need to run 

```
  :PluginInstall
```

on Vim command mode to install all plugins. There is no need to do something
similar to Oh-My-Zsh or Package Control since they're self contained.

