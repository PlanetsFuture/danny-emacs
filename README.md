
# Danny Emacs

Danny Emacs configuration.


## Features
- Clean
- Quick fuzzy search
- Better Org/Markdown support
- Support multiple programming languages
  - C/C++/Object-C/Java
  - Python/JavaScript/Typescript/JSON/YAML
  - HTML/CSS/XML
  - Rust/Dart/Golang/Swift

- Auto completion
- Fly syntax chenk
- Fly spell chenk
- Git integration

## Prerequisite

### OS

- GNU Linux
- macOS
- Windows (Cygwin/MSYS)

### GNU Emacs

Please refer to [GNU Emacs](https://www.gnu.org/software/emacs/emacs.html)

### Install

In case you already have your own configuration and want to try this one, first backup your .emacs.d folder, open your terminal and type:

``` shell
mv ~/.emacs.d ~/.emacs.d.bak
```

Assuming you already have git installed on your computer, clone this git repository with the following command

``` shell

git clone --depth 1 https://github.com/PlanetsFuture/danny-emacs.git ~/.emacs.d
```

Start Emacs and wait until all packages are installed.

### Update

``` emacs-lisp
# Update Danny Emacs, configurations and packages
M-x danny-update

# Update Emacs configurations only
M-x danny-update-config

# Update packages only
M-x danny-update-packages
```

## Hydra
| Name                  | Scope                  | Keybinding        | Description           |
|-----------------------|------------------------|-------------------|-----------------------|
| `toggles-hydra`       | global                 | `<f6>`            | Global option toggles |
| `window-hydra`        | global                 | `C-c w`/`C-x o w` | Window management     |
| `doom-modeline-hydra` | doom-modeline-modeline | `C-<f6>`          | Mode-options          |
|                       |                        |                   |                       |
