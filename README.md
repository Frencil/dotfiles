Repository for config files.

Designed for easy deployment to new installations.

INSTALLATION
------------

1. Clone repo to a directory inside the home directory (e.g. ~/dotfiles)
2. Run setup

Some config files simply have to live in the top-level home directory to get initialized.

In order to keep this repo separate from the other types of things that inhabit one's
home directory I included the setup bash script to parse the contents of the repo directory
and create a symbolic link in the home directory to each dotfile.
