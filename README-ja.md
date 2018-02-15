<!-- -*- coding: utf-8 -*- -->
![logo1](https://raw.githubusercontent.com/wiki/gfngfn/SATySFi/img/satysfi-logo.png)

[![Build Status](https://travis-ci.org/gfngfn/SATySFi.svg?branch=master)](https://travis-ci.org/gfngfn/SATySFi)

## 概要

*SATySFi*（英単語の “satisfy” と同様に発音します）は，新しい組版処理システムとその言語です。構文は主にテキスト部分とプログラム部分からなり，前者はLaTeX風の構文で文書を執筆するために，後者はコマンドを定義するために使われます。いわゆる函数型プログラミングの要領でコマンドが定義でき，かつ静的に型がつけられるため，柔軟な記述とわかりやすいエラー報告が実現されています。

本ソフトウェアは2017年度IPA未踏事業の1プロジェクトとして支援のもと開発されました。

## OPAM を使ったインストール方法

### 事前に必要なもの

ビルド前に最低限，以下のソフトウェアが必要です。

* bzip2
* cc
* git
* m4
* make
* unzip
* wget
* [opam](https://opam.ocaml.org/) 1.2 （インストール手順は[こちら](https://opam.ocaml.org/doc/Install.html)。）
* ocaml 4.05.0 （OPAM からインストールします）

#### 準備例（Ubuntu）

```sh
sudo apt-get update
sudo apt-get install build-essential git m4 unzip wget

# 以下のコマンドは OPAM がファイルに追記してもよいか聞いてきます。
# 必ず説明を読み，環境変数を適切に設定してください。
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin

opam switch 4.05.0
eval `opam config env`
opam update
```

#### 準備例（OS X Mavericks 以降）

```sh
# このスクリプトを実行する前に，GCC や Make などの基本的なソフトウェアをインストールしておいてください。これらは Xcode Command Line Tools からインストールできます。
# また，Homebrew もインストールしてください。

brew update
brew install unzip wget git opam

# 以下のコマンドは OPAM が（~/.bash_profile などの）ファイルに環境変数に関する設定を追記してもよいか聞いてきます。
# 必ず説明を読み，環境変数を適切に設定してください。
opam init

opam switch 4.05.0
eval `opam config env`
opam update
```

### ビルド

まず，このレポジトリとサブモジュールを clone します。その後 OPAM を使って SATySFi をビルドします。

```sh
# clone
git clone https://github.com/gfngfn/SATySFi.git
cd SATySFi
git submodule update --init --recursive

# build
opam pin add satysfi .
opam install satysfi
```

* 再インストール： `opam reinstall satysfi` を実行
* アンインストール： `opam uninstall satysfi` を実行

## 用法

    satysfi <input files> -o <output file>

で `<input files>` から `<output file>` を出力します。例えばソースファイル `doc.saty` から `output.pdf` を出力したい場合，次のようにします：

    satysfi doc.saty -o output.pdf

## コマンドラインオプション

* `-v`, `--version`: ヴァージョンを表示します。
* `-o`, `--output`: 出力ファイル名を指定します。省略された場合，入力ファイル名の拡張子を `.pdf` に変えた名前を出力ファイル名とします。
* `--full-path`: 標準出力に書き込むログに於いて，ファイル名をすべて絶対パスで表示します。
* `--type-check-only`: 型検査だけをして終了します。