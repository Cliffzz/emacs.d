.emacs.d
========

[![CircleCI](https://circleci.com/gh/Cliffzz/.emacs.d.svg?style=shield)](https://circleci.com/gh/Cliffzz/.emacs.d) [![emacs-version](https://img.shields.io/badge/emacs-26.1-brightgreen.svg)](https://www.gnu.org/software/emacs/) [![node-version](https://img.shields.io/badge/node-10.2.1-brightgreen.svg)](https://github.com/nodejs/node) [![npm-version](https://img.shields.io/badge/npm-5.6.0-brightgreen.svg)](https://github.com/npm/npm) [![license](https://img.shields.io/badge/license-GPL%20v3-blue.svg)](https://github.com/Cliffzz/.emacs.d/blob/master/LICENSE)

Personal emacs configuration.

<img width="1680" src="https://user-images.githubusercontent.com/2283434/38770578-5bdb5d2c-4015-11e8-905a-caeff3df31c1.png">

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [.emacs.d](#emacsd)
    - [Installation](#installation)
        - [Prerequisites](#prerequisites)
        - [macOS](#macos)
        - [Windows](#windows)

<!-- markdown-toc end -->

## Installation
### Prerequisites
- [aspell](https://github.com/GNUAspell/aspell)
- [ripgrep](https://github.com/BurntSushi/ripgrep)
- [node](https://github.com/nodejs/node)
- [npm](https://github.com/npm/npm)
- [omnisharp-roslyn](https://github.com/OmniSharp/omnisharp-roslyn)

### macOS
- [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)
```
brew install emacs-plus --devel --without-spacemacs-icon
npm install
```

### Windows
- [emacs](https://github.com/m-parashar/emax64)
```
npm install
```

### Byte compile
To improve performance run:
```
C-c c compile-files
```
