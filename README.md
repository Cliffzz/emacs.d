# .emacs.d

[![CircleCI](https://img.shields.io/circleci/project/github/Cliffzz/.emacs.d.svg?style=flat-square)](https://circleci.com/gh/Cliffzz/.emacs.d) [![Release](https://img.shields.io/badge/release-0.3.1-blue.svg?style=flat-square)](https://github.com/Cliffzz/.emacs.d/releases) [![Emacs](https://img.shields.io/badge/emacs-26.1-%23c065db.svg?style=flat-square)](https://www.gnu.org/software/emacs/) [![Node](https://img.shields.io/badge/node->=6-026e00.svg?style=flat-square)](https://nodejs.org/en/) [![ConventionalCommits](https://img.shields.io/badge/Conventional%20Commits-1.0.0-yellow.svg?style=flat-square)](https://conventionalcommits.org) [![License](https://img.shields.io/github/license/Cliffzz/.emacs.d.svg?style=flat-square)](https://github.com/Cliffzz/.emacs.d/blob/master/LICENSE)

Cliffzz's emacs configuration.
<img width="1920" src="https://user-images.githubusercontent.com/2283434/51805106-aa903a00-2269-11e9-986b-ddce2600f892.png">

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

-   [.emacs.d](#emacsd)
    -   [Installation](#installation)
        -   [Prerequisites](#prerequisites)
        -   [macOS](#macos)
        -   [Windows](#windows)

<!-- markdown-toc end -->

## Installation

### Prerequisites

-   [hunspell](https://github.com/hunspell)
-   [dictionary](http://wordlist.aspell.net/dicts/)
-   [ripgrep](https://github.com/BurntSushi/ripgrep)
-   [node](https://github.com/nodejs/node)
-   [Iosevka](https://github.com/be5invis/Iosevka)
-   [luacheck](https://github.com/mpeterv/luacheck)

### macOS

-   [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)

```
brew install emacs-plus --devel --without-spacemacs-icon
npm install
```

### Windows

-   [emacs](https://github.com/m-parashar/emax64)

```
npm install
```

### Byte compile

To improve performance run:

```
M-x compile-files
```
