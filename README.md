# cppinsights.el

An Emacs package that integrates with [C++ Insights](https://cppinsights.io/), a tool that transforms C++ source code into its expanded form, revealing the details the compiler sees after applying language features like templates, operator overloading, and lambda functions.

## Description

This package allows you to run C++ Insights directly from within Emacs, displaying the transformed code in a separate buffer. It helps C++ developers understand how the compiler interprets their code, making it easier to debug complex C++ features and learn about the language's inner workings.

## Installation

### Prerequisites

Before using this package, you need to install the C++ Insights command-line tool:

#### Windows
1. Install [WSL](https://docs.microsoft.com/en-us/windows/wsl/install) (Windows Subsystem for Linux)
2. Follow the Ubuntu instructions below within WSL

#### macOS
Using Homebrew:
```bash
brew install cmake llvm
git clone https://github.com/andreasfertig/cppinsights.git
cd cppinsights
mkdir build && cd build
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_CXX_COMPILER=clang++ ..
make
sudo make install
```

#### Ubuntu/Debian
```bash
sudo apt-get install cmake clang libclang-dev llvm
git clone https://github.com/andreasfertig/cppinsights.git
cd cppinsights
mkdir build && cd build
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
make
sudo make install
```

#### Arch Linux
Using AUR:
```bash
yay -S cpp-insights
```
Or:
```bash
git clone https://aur.archlinux.org/cpp-insights.git
cd cpp-insights
makepkg -si
```

### Package Installation

1. Clone this repository:
```bash
git clone https://github.com/yourusername/cppinsights.el.git ~/.emacs.d/cppinsights.el
```

2. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "~/.emacs.d/cppinsights.el")
(require 'cppinsights)
```

## Usage

1. Open a C++ file in Emacs
2. Run `M-x cppinsights-run` to process the current file
3. A new buffer will open showing the transformed code

You can customize the package by:
- `M-x customize-group RET cppinsights RET`

## Key Bindings

You can add a key binding to your Emacs configuration:

```elisp
(global-set-key (kbd "C-c i") 'cppinsights-run)
```
