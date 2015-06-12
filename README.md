**nix-package.el** is for those of us who use [Nix](http://nixos.org/nix/) and enjoy package.el. It provides an Emacs interface to `nix-env`.

# User Guide

## Execution

`M-x list-nix-packages` will bring up a full list.

If you do `C-u M-x …` then the list will only include packages matching `nix-package-system` (customizable).

## Key Bindings

The key bindings should match those of package.el when appropriate. `C-h m` will provide a full listing.

* `i` to install
* `d` to delete
* `U` to upgrade all updated packages
* `~` to delete obsolete packages
* `RET` to see the details of a package

## Fields

### Version

Actually having a version number in a Nix package is rare. This tries to fall back on parsing the version out of the name when necessary. `nix-env` certainly does some work in this vein, so it might be possible to leverage that as well.

### Status

The status code is two characters. The first is taken from the nix-env `--status` option (in decreasing order of availablity):

* `I`: for a package that is _i_nstalled for the current user,
* `P`: for a package that is _p_resent on the system,
* `S`: for a package that has a _s_ubstitute available for download,
* ␠: for a package that can be built, or
* `D`: for a package that is _d_isabled.

The second character indicates if the package is older or newer than the one installed (if any):

* `<`: if the package is older than an installed version (IE, obsolete),
* ␠: if the package is the same as the installed version (or not installed), or
* `>`: if the package is newer than an installed version (IE, upgradable).
