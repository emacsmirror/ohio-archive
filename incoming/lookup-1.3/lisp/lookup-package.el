;;; lookup-package.el --- management of supplement package
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: lookup-package.el,v 1.2 1999/05/23 17:27:20 knishida Exp $

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; 補助パッケージとは、Lookup で利用する辞書の補足的な設定を、特定の書式
;; に従って定義した設定ファイル集のことを言う。このファイルでは、補助パッ
;; ケージを構築・利用するために必要な関数を用意している。
;; 
;; 以下、バージョン 1.1 の補助パッケージの書式について説明する。

;; Package Files:
;; 
;; 補助パッケージは、単一の設定ファイルとして、あるいは複数の設定ファイ
;; ルを含むディレクトリとして構築される。いずれの場合にも、パッケージに
;; は固有のパッケージ名を付ける必要がある。例えば "space" という名のパッ
;; ケージを作る場合には、構成は次のようになる。
;; 
;; o 単一ファイルの場合
;; 
;;   space.el       設定ファイル
;; 
;; o 複数ファイルの場合
;; 
;;   space/         ディレクトリ
;;     space.el     マスターファイル
;;     sun.el       設定ファイル1
;;     earth.el     設定ファイル2
;;     ...          (いくつあってもよい)
;; 
;; 設定ファイルは "パッケージ名.el" とする。複数ファイルを用いる場合には、
;; パッケージ名のディレクトリを作って、その中に必要なだけのファイルを用意
;; する。この場合、メインとなる設定ファイル(マスターファイル)から、実際の
;; 設定ファイルを読み込むように記述する。

;; Package Contents:
;; 
;; 設定は基本的に、以下の二つの変数をセットすることによって行なう。
;; 
;; lookup-package-agent-options - 検索エージェントのオプションを設定する。
;; lookup-package-dictionary-options-alist - 辞書のオプションを設定する。
;; 
;; 例えば次のようにファイルに書けば、それで補助パッケージとして成り立つ。
;; 
;;   (setq lookup-package-agent-options
;;         '((title . "The Immeasurable Space")))
;; 
;;   (setq lookup-package-dictionary-options-alist
;;         '(("sun" . ((title . "The Scorching Sun")))
;;           ("earth" . ((title . "The Earth as Mother")))))
;; 
;; 設定の仕方は、他のオプション設定と同様である。ただし、ここで "sun" とか
;; "earch" とかなっているのは辞書名であり、辞書ID ではないことに注意。後か
;; らユーザによりエージェントID が指定され、オプションとして完成する。
;; 
;; 設定は辞書毎に行なう方が整理が付くので、辞書オプションは個別のファイル
;; で定義して、それをロードし結果をまとめるという方法が考えられる。それを
;; サポートするための方法が用意されている。

;; Supplement Files:
;; 
;; 辞書のオプションを実際に定義している個々のファイルを、特に補助ファイル
;; と呼ぶ。補助ファイルでは、次の変数をセットする。
;; 
;; lookup-package-dictionary-options - 辞書のオプションを設定する
;; 
;; この変数には(辞書名抜きの)オプションのみをセットする。補助ファイルは
;; 関数 `lookup-package-load' によって読み込まれ、セットされた値が返され
;; る。例えば次のようにして利用する。
;; 
;; -- sun.el --
;; 
;;   (setq lookup-package-dictionary-options
;;         '((title . "The Scorching Sun")
;;           ...))
;; 
;; -- space.el --
;; 
;;   (require 'lookup-package)
;; 
;;   (setq lookup-package-dictionary-options-alist
;;         (list (cons "sun" (lookup-package-load "sun"))
;;               ...))
;; 
;; `lookup-package-load' は、このファイルで定義しているので、あらかじめ
;; require しておく必要がある。典型的なパッケージはこれで完成する。

;; Useful Variables:
;; 
;; ある種の辞書は、複数の検索エージェントにより利用されることがある。その
;; ため補助パッケージも、それぞれのエージェントに対応させた方が都合がいい。
;; もし設定をエージェントにより区別したい場合、補助ファイルでは変数 
;; `lookup-package-agent' を参照することが出来る。この変数には、パッケージ
;; が読み込まれるとき、それが適用しようとするエージェントのクラスがセット
;; される。これにより、例えば次のように利用できる。
;; 
;;   (setq lookup-package-dictionary-options
;;         (cond
;;          ((eq lookup-package-agent 'ndic))
;;           '((title . "The Sun by ndic")))
;;          ((eq lookup-package-agent 'ndict))
;;           '((title . "The Sun by ndict")))
;;          ...))
;; 
;; また、パッケージに含まれるその他のファイルを利用する等の目的のために、
;; `lookup-package-file-directory' を参照できる。これには補助ファイルの
;; あるディレクトリと同じディレクトリが示される。

;; Package Example:
;; 
;; 実際的な補助パッケージ構築の実例については、既存のパッケージを参照して
;; ほしい。いくつかのパッケージは Lookup のホームページから得ることが出来る。
;; 
;;   http://www.ring.gr.jp/openlab/lookup/packages/

;;; Code:

(require 'lookup)

(defconst lookup-package-version "1.1"
  "補助パッケージの書式バージョン。")

;;;
;:: Customizable variables
;;;

(defcustom lookup-package-directory
  (expand-file-name "packages" lookup-data-directory)
  "*補助パッケージが収められるディレクトリ。"
  :type 'directory
  :group 'lookup-setup)

;;;
;:: Package variables
;;;

(defvar lookup-package-agent nil
  "パッケージを利用しようとするエージェントのクラスが設定される。")

(defvar lookup-package-file-directory nil
  "補助ファイルのあるディレクトリが設定される。")

(defvar lookup-package-agent-options nil
  "パッケージが提供するエージェント・オプションを設定する。")

(defvar lookup-package-dictionary-options nil
  "補助ファイルが提供する辞書オプションを設定する。")

(defvar lookup-package-dictionary-options-alist nil
  "パッケージが提供する辞書オプションの連想リストを設定する。")

;;;
;:: Construct functions
;;;

(defun lookup-package-load (name)
  "補助ファイルを読み込み、そこで設定されている辞書オプションを返す。
NAME には補助ファイル名を指定する。拡張子は必要ない。
補助ファイルでは、変数 `lookup-package-dictionary-options' を指定する
必要がある。その値がこの関数の返却値となる。"
  (load (expand-file-name name lookup-package-file-directory) nil t)
  lookup-package-dictionary-options)

;;;
;:: Setup functions
;;;

(defun lookup-use-package (agent-id package-name)
  "補助パッケージをロードする。
AGENT-ID は、パッケージを適用するエージェントID。
PACKAGE-NAME は、パッケージ名、あるいはパッケージの絶対パス。
絶対パスでない場合、パッケージは変数 `lookup-package-directory' が
示すディレクトリに展開しておく必要がある。
この関数は `lookup-agent-options-alist' 及び
`lookup-dictionary-options-alist' の値を書き換える。"
  (let ((path (expand-file-name package-name lookup-package-directory))
	(lookup-package-agent (progn (string-match "^[^+]+" agent-id)
				     (intern (match-string 0 agent-id))))
	(lookup-package-file-directory lookup-package-directory)
	lookup-package-agent-options lookup-package-dictionary-options-alist)
    (message "Loading package %s..." package-name)
    ;; load package
    (unless (load path t t)
      (setq lookup-package-file-directory path)
      (unless (load (expand-file-name package-name path) t t)
	(error "Cannot find supplement package: %s" package-name)))
    ;; set agent options
    (lookup-foreach (lambda (option)
		      (lookup-set-agent-option agent-id
					       (car option) (cdr option)))
		    lookup-package-agent-options)
    ;; set dicitonary options
    (lookup-foreach (lambda (pair)
		      (let ((id (concat agent-id ":" (car pair))))
			(lookup-foreach (lambda (option)
					  (lookup-set-dictionary-option
					   id (car option) (cdr option)))
					(cdr pair))))
		    lookup-package-dictionary-options-alist)
    (message "Loading package %s...done" package-name)
    t))

(provide 'lookup-package)

;;; lookup-package.el ends here
