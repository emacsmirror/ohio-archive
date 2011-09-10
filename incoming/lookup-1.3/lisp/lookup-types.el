;;; lookup-types.el --- Lookup various data types
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: lookup-types.el,v 1.4 1999/07/27 13:06:48 tsuchiya Exp $

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

;;; Code:

(require 'evi)
(require 'lookup-utils)
(require 'lookup-vars)

(put 'lookup-defstruct 'lisp-indent-function 2)
(defmacro lookup-defstruct (name slots &rest args)
  ;; Lookup で用いるデータ構造を定義するためのマクロ。
  ;; NAME はデータの名前。SLOTS はデータが保持する情報のリスト。
  ;; ARGS は追加の設定。下記の実例を参照。
  ;; これにより定義されたデータタイプは、以下の関数を持つことになる。
  ;;
  ;; lookup-make-NAME     - データを生成する
  ;; lookup-NAME-p        - データタイプをチェックする
  ;; lookup-NAME-SLOT     - SLOT のデータを参照する
  ;; lookup-NAME-set-SLOT - SLOT にデータをセットする
  ;;
  ;; ARGS に :with-properties オプションを付けた場合、更に次の関数が作られる。
  ;;
  ;; lookup-NAME-get-property - オブジェクトの属性を得る
  ;; lookup-NAME-put-property - オブジェクトに属性を設定する
  ;; lookup-NAME-PROP         - PROP のデータを参照する
  ;; lookup-NAME-set-PROP     - PROP にデータをセットする
  ;;
  (let* ((str (symbol-name name)) (n 0)
	 (prefix (concat "lookup-" str "-"))
	 (tag (list 'quote (intern (concat ":" str))))
	 (with-properties (memq ':with-properties args))
	 (properties (eval (plist-get args ':with-properties)))
	 ;; function names
	 (f-make (intern (concat "lookup-make-" str)))
	 (f-p (intern (concat prefix "p")))
	 (f-plist (intern (concat prefix "plist")))
	 (f-get-prop (intern (concat prefix "get-property")))
	 (f-put-prop (intern (concat prefix "put-property"))))
    (nconc (list 'progn
		 ;; Constructor:
		 ;;
		 ;; (defsubst lookup-make-NAME (SLOT...)
		 ;;   (vector :NAME SLOT... nil))
		 (let ((plist (if lookup-debug-mode '(lookup-new-plist))))
		   (list 'defsubst f-make slots
			 (cons 'vector
			       (cons tag (if with-properties
					     (append slots (list plist))
					   slots)))))
		 ;; Predicate:
		 ;;
		 ;; (defun lookup-NAME-p (NAME)
		 ;;   (and (vectorp NAME) (eq (aref NAME 0) :NAME)))
		 (list 'defun f-p (list name)
		       (list 'and (list 'vectorp name)
			     (list 'eq (list 'aref name 0) tag))))
	   ;; Accessors:
	   ;;
	   (apply 'nconc
		  (mapcar (lambda (slot)
			    (let* ((str (symbol-name slot))
				   (f-ref (intern (concat prefix str)))
				   (f-set (intern (concat prefix "set-" str))))
			      (list
			       ;; (defsubst lookup-NAME-SLOT (NAME)
			       ;;   (aref NAME n))
			       (list 'defsubst f-ref (list name)
				     (list 'aref name (setq n (1+ n))))
			       ;; (defsubst lookup-NAME-set-SLOT (NAME SLOT)
			       ;;   (aset NAME n SLOT))
			       (list 'defsubst f-set (list name slot)
				     (list 'aset name n slot)))))
			  slots))
	   ;; Properties:
	   ;;
	   (when with-properties
	     (setq n (1+ n))
	     (list
	      ;; (defmacro lookup-NAME-plist (NAME &optional plist)
	      ;;   (if plist (list 'aset NAME n plist) (list 'aref NAME n)))
	      (list 'defmacro f-plist (list name '&optional 'plist)
		    (list 'if 'plist
			  (if lookup-debug-mode
			      (` (list 'set (list 'aref (, name) (, n)) plist))
			    (list 'list ''aset name n 'plist))
			  (if lookup-debug-mode
			      (` (list 'symbol-value
				       (list 'aref (, name) (, n))))
			    (list 'list ''aref name n))))
	      ;; (defun lookup-NAME-get-property (NAME prop)
	      ;;   (plist-get (lookup-NAME-plist NAME) prop))
	      (list 'defun f-get-prop (list name 'prop)
		    (list 'plist-get (list f-plist name) 'prop))
	      ;; (defun lookup-NAME-put-property (NAME prop value)
	      ;;   (lookup-NAME-plist NAME
	      ;;     (plist-put (lookup-NAME-plist NAME) prop value)))
	      (list 'defun f-put-prop (list name 'prop 'value)
		    (list f-plist name
			  (list 'plist-put (list f-plist name)
				'prop 'value)))))
	   (when with-properties
	     (apply 'nconc
		    (mapcar (lambda (prop)
			      (let* ((str (symbol-name prop))
				     (ref (intern (concat prefix str)))
				     (set (intern (concat prefix "set-" str))))
				(list
				 ;; (defsubst lookup-NAME-PROP (NAME)
				 ;;   (lookup-NAME-get-property NAME 'PROP))
				 (list 'defsubst ref (list name)
				       (list f-get-prop name
					     (list 'quote prop)))
				 ;;(defsubst lookup-NAME-set-PROP (NAME PROP)
				 ;; (lookup-NAME-put-property NAME 'PROP PROP))
				 (list 'defsubst set (list name prop)
				       (list f-put-prop name
					     (list 'quote prop) prop)))))
			    properties))))))

(defun lookup-new-plist ()
  ;; plist を保持するためのシンボルを生成する。
  ;; plist をそのままデータ構造に組み込むと、デバッグの際に出力が大きく
  ;; なり過ぎて見づらくなる。そのため、変数 `lookup-debug-mode' が non-nil
  ;; の場合には、オブジェクトにはこのシンボルを放り込んでおき、その値として
  ;; plist を処理する。
  (let ((plist (make-symbol "plist")))
    (set plist nil)
    plist))


;;;;;;;;;;;;;;;;;;;;
;: Search Method
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; 各種検索方式。それぞれの検索方式は単一のシンボルによって表現される。
;; 現在、次のものが有効。
;;
;; exact     - 完全一致検索
;; prefix    - 前方一致検索
;; suffix    - 後方一致検索
;; substring - 部分一致検索
;; regexp    - 正規表現検索
;; keyword   - キーワード検索
;; text      - 全文検索
;; reference - リファレンス
;;
;; 与えられた検索方式から実際にどのような検索を行なうかは各 agent に
;; 任されており、厳密な規定はない。それぞれの検索方式についての一般的な
;; 定義は、マニュアルの "Search Methods" を参照。

(defconst lookup-word-search-methods
  '(exact prefix suffix substring regexp keyword stemming expansion text))

(defconst lookup-search-methods
  (cons 'default (cons 'reference lookup-word-search-methods)))

(defconst lookup-method-key-alist
  '((exact . "=") (prefix . "<") (suffix . ">") (substring . "-")
    (regexp . "r") (keyword . "@") (stemming . "^") (text . "/")
    (default . "*") (reference . "%") (expansion . "#")))

(defun lookup-method-key (method)
  (or (lookup-assq-ref lookup-method-key-alist method)
      (error "Unknown search method: %s" method)))


;;;;;;;;;;;;;;;;;;;;
;: Search Query
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; 検索するパターンを格納するためのデータタイプ。Lookup はユーザの入力を
;; 解析し、検索方式に応じてこのタイプのオブジェクトを生成する。生成された
;; データは最終的に検索エージェントに渡され、エージェント毎にその内容を
;; 判断して検索が実行される。

;; Structure:
;;
;;   [:query METHOD STRING]
;;
;; METHOD は検索方式。変数 `lookup-search-methods' にあるシンボルが有効。
;; STRING は検索文字列。METHOD と対にして、検索するパターンが決定される。
;;   この文字列はあらかじめ、不要な文字を取り除く等、ある程度の正規化を
;;   しておくことが望ましい。

(lookup-defstruct query (method string))

;; Functions:
;;
;; lookup-parse-pattern   - 引数の文字列を解析し、query を生成する。
;; lookup-query-to-regexp - query を regexp パターンに変換する。

(defun lookup-parse-pattern (pattern)
  ;; 入力文字列 PATTERN を解析し、query を生成して返す。
  (let (method)
    (cond
     ;; 'word' -> match exactly
     ((string-match "^'\\(.*\\)'$" pattern)
      (setq method 'exact pattern (match-string 1 pattern)))
     ;; /word/ -> match regexp
     ((string-match "^/\\(.*\\)/$" pattern)
      (setq method 'regexp pattern (match-string 1 pattern)))
     ;; /word  -> search text
     ((string-match "^/" pattern)
      (setq method 'text pattern (substring pattern 1)))
     ;; @word  -> match keyword
     ((string-match "^@" pattern)
      (setq method 'keyword pattern (substring pattern 1)))
     ;; *word* -> match substring
     ((string-match "^\\*\\([^*?]*\\)\\*$" pattern)
      (setq method 'substring pattern (match-string 1 pattern)))
     ;; word*  -> match prefixes
     ((string-match "^\\([^*?]*\\)\\*$" pattern)
      (setq method 'prefix pattern (match-string 1 pattern)))
     ;; *word  -> match suffixes
     ((string-match "^\\*\\([^*?]*\\)$" pattern)
      (setq method 'suffix pattern (substring pattern 1)))
     ;; w*o?d  -> "w.*o.d"
     ((string-match "[*?]" pattern)
      (setq pattern (if (string-match "^\\*" pattern)
			(substring pattern 1)
		      (concat "^" pattern)))
      (setq pattern (if (string-match "\\*$" pattern)
			(substring pattern 0 (match-beginning 0))
		      (concat pattern "$")))
      (let ((start 0))
	(while (string-match "*" pattern start)
	  (setq pattern (replace-match ".*" t t pattern)
		start (+ (match-end 0) 1))))
      (while (string-match "?" pattern)
	(setq pattern (replace-match "." t t pattern)))
      (setq method 'regexp pattern pattern))
     ;; default
     (t (setq method 'default)))
    (lookup-make-query method pattern)))

(defun lookup-query-to-regexp (query)
  ;; QUERY を regexp パターンに変換する。
  (let* ((method (lookup-query-method query))
	 (string (lookup-query-string query))
	 (quote (regexp-quote string)))
    (cond ((eq method 'keyword) (concat "\\<" quote "\\>"))
	  ((eq method 'prefix) (concat "^" quote))
	  ((eq method 'suffix) (concat quote "$"))
	  ((eq method 'exact) (concat "^" quote "$"))
	  ((eq method 'substring) quote)
	  ((eq method 'text) string)
	  ((eq method 'regexp) string)
	  (t (error "Illegal search method for regexp: %s" method)))))


;;;;;;;;;;;;;;;;;;;;
;: Search Module
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; 検索モジュールを表わすデータタイプ。各モジュールはこのタイプの
;; オブジェクトとして生成される。
;;
;; 検索モジュールとは、一回の検索の単位となる辞書のまとまりのこと。
;; 複数のモジュールを用意することで、対象の異なる複数の検索コマンドを
;; 定義することが出来る。下記の `Setup' の項を参照。

;; Structure:
;;
;;   [:module NAME DICTIONARIES PLIST]
;;
;; NAME は、モジュールの名称。
;; DICTIONARIES は、モジュールに含まれる辞書のリスト。
;; PLIST は、属性リストの保持に使われる。

;; Properties:
;;
;; history     - モジュールの履歴。
;; agents      - モジュールの辞書が属するエージェントのリスト。
;; id-list     - モジュールの定義に含まれる辞書ID のリスト。
;; initialized - モジュールが初期化されていれば `t'。

(lookup-defstruct module (name dictionaries)
  :with-properties '(history))

;; Initialize:
;;
;; モジュールの初期化は二つの段階で行なわれる。
;;
;; 1) オブジェクトの生成
;;    モジュール・オブジェクトを生成する。これはデータを生成するだけで、
;;    一切の初期化は行なわない。これを行なう関数は `lookup-new-module'。
;; 2) エージェントの初期化
;;    設定に従って検索エージェントを生成し、初期化を行なう。これは
;;    関数 `lookup-module-init' で行なわれる。
;;
;; これらを分けるのは、ユーザの使い勝手をよくするためである。モジュールの
;; 生成は Lookup の起動後すぐに行なわれるが、M-x lookup-pattern などで
;; Lookup を起動したとき、検索語を入力する前にエージェントのセットアップ
;; まで行なわれたのでは入力まで時間が掛かり、ややストレスとなる。そこで
;; 初期化を後回しにすることで少しでも起動を早め、入力が行なわれてから
;; 初期化を始めるようにしている。

(defun lookup-new-module (spec)
  (let ((name (car spec)) (id-list (cdr spec)) module agents)
    ;; get agent list
    (lookup-foreach (lambda (id)
		      ;; get the list of agents matched with ID
		      (let ((match (concat "^" (regexp-quote id)))
			    (list (lookup-agent-list)) (start agents))
			(while list
			  (if (string-match match (lookup-agent-id (car list)))
			      (setq agents (cons (car list) agents)))
			  (setq list (cdr list)))
			(if (eq start agents)
			    (error "No match agent: %s" id))))
		    ;; get a list of agent-IDs
		    (lookup-nunique (mapcar (lambda (id)
					      (string-match "^[^:]*" id)
					      (substring id 0 (match-end 0)))
					    id-list)))
    (setq agents (nreverse (lookup-nunique agents 'eq)))
    ;; construct module
    (setq module (lookup-make-module name nil))
    (lookup-module-put-property module 'agents agents)
    (lookup-module-put-property module 'id-list id-list)
    (lookup-module-init module)))

(defun lookup-module-init (module)
  (lookup-module-set-history module (lookup-new-history))
  module)

(defun lookup-module-setup (module)
  (unless (lookup-module-get-property module 'setup)
    (let ((agents (lookup-module-get-property module 'agents))
	  dicts dictionary-list)
      ;; setup agents
      (lookup-foreach 'lookup-agent-setup agents)
      ;; get dictionary list
      (setq dictionary-list
	    (apply 'append (mapcar 'lookup-agent-dictionaries agents)))
      (lookup-foreach (lambda (id)
			(let ((match (concat "^" (regexp-quote id)))
			      (list dictionary-list) (start dicts))
			  (while list
			    (if (string-match match (lookup-dictionary-id (car list)))
				(setq dicts (cons (car list) dicts)))
			    (setq list (cdr list)))
			  (if (eq start agents)
			      (error "No match dictionary: %s" id))))
		      (lookup-module-get-property module 'id-list))
      (lookup-module-set-dictionaries module (nreverse dicts))
      (lookup-module-put-property module 'setup t))))

(defun lookup-module-clear (module)
  (lookup-module-set-dictionaries module nil)
  (lookup-module-set-history module nil)
  (lookup-module-put-property module 'setup nil))


;;;;;;;;;;;;;;;;;;;;
;: Search Agent
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; 検索エージェントを表わすデータタイプ。各種エージェントはこのタイプの
;; オブジェクトとして生成される。

;; Structure:
;;
;;   [:agent CLASS LOCATION OPTIONS PLIST]
;;
;; CLASS はエージェントのクラス。LOCATION は辞書の所在。
;; OPTIONS はオプションのリスト。変数 `lookup-search-agents' を参照。
;; PLIST は、属性リストの保持に使われる。

;; Properties:
;;
;; id           - エージェントID。("CLASS+LOCATION")
;; title        - エージェントのタイトル。
;; dictionaries - エージェントに含まれる辞書を保持する。
;; defaults     - エージェントのデフォルトの設定を保持する。

(lookup-defstruct agent (class location options)
  :with-properties '(id title dictionaries defaults))

;; Definitions:
;;
;; 各エージェントは同名の elisp ファイルによって定義され、require により
;; 読み込まれる。つまり各エージェントは provide されなければならない。
;;
;; エージェントは検索実行のためにいくつかのコマンドを持つ。各コマンドは
;; 定義ファイルの中で (put AGENT COMMAND FUNCTION) のように定義される。
;; 現在、次のコマンドが意味を持つ。(大文字のは引数)
;;
;; setup AGENT
;;   エージェントのセットアップ・コマンド。これはエージェントの初期化を
;;   行ない、利用可能な辞書オブジェクトのリストを返す。
;; clear AGENT
;;   エージェントの終了コマンド。これはエージェントの後片付けを行ない、
;;   Lookup の終了に備える。
;;
;; エージェントはいくつかの固有の標準設定を持つ。これは定義ファイルの中で
;; (put AGENT KEY VALUE) のように定義され、ユーザ・オプションにより上書き
;; 可能となっている。次の `Options' の項を参照。

(defun lookup-agent-require (agent)
  ;; エージェントの定義ファイルをロードする。
  (require (lookup-agent-class agent)))

(defun lookup-agent-ref (agent key)
  ;; エージェントの標準設定を参照する。
  (get (lookup-agent-class agent) key))

(defun lookup-agent-command-p (agent command &rest args)
  ;; エージェント・コマンドをチェックする。
  (lookup-agent-ref agent command))

(defun lookup-agent-command (agent command &rest args)
  ;; エージェント・コマンドを実行する。
  (apply (lookup-agent-ref agent command) agent args))

;; Options:
;;
;; エージェントはいくつかのオプションを持つことが出来る。オプションは
;; ユーザにより設定されるが、プログラムでそのデフォルト値を設定することも
;; 出来る。設定には次の種類がある。
;;
;; 設定時オプション - エージェントを設定するときに指定されるオプション。
;;   変数 `lookup-search-agents' を参照。
;; 変数 `lookup-agent-options-alist', `lookup-default-agent-options' -
;;   ユーザ指定のオプション設定。
;; デフォルト - プログラムによる設定。`lookup-agent-set-default' を参照。
;; 標準設定 - 定義ファイルに固定された設定。

(defun lookup-agent-set-default (agent key value)
  ;; エージェントのデフォルトの設定を行なう。これはユーザによるオプション
  ;; 指定がない場合に利用される。
  (let ((defaults (plist-put (lookup-agent-defaults agent) key value)))
    (lookup-agent-set-defaults agent defaults)))

(defun lookup-agent-option (agent key)
  ;; エージェントのオプションを参照する。設定時に指定されたオプション、
  ;; 変数 `lookup-agent-options-alist'、`lookup-default-agent-options'、
  ;; デフォルト、標準設定の順に探索する。
  (or (plist-get (lookup-agent-options agent) key)
      (lookup-assq-ref (lookup-assoc-ref lookup-agent-options-alist
					 (lookup-agent-id agent)) key)
      (lookup-assq-ref lookup-default-agent-options key)
      (plist-get (lookup-agent-defaults agent) key)
      (lookup-agent-ref agent key)))

;; Initialize:
;;
;; 検索エージェントの初期化は四つの段階で行なわれる。
;;
;; 1) `lookup-new-agent'
;;    変数 `lookup-search-agents' で設定された内容を解析し、エージェント・
;;    オブジェクトを生成する。続いて `lookup-agent-init' を呼び初期化する。
;; 2) `lookup-agent-init'
;;    エージェントID やタイトルなどを設定する。辞書のセットアップはまだ。
;; 3) `lookup-agent-setup'
;;    エージェント毎の setup コマンドを実行し、辞書の初期化を行なう。
;; 4) 各 setup 関数
;;    エージェント毎のセットアップを行ない、辞書のリストを返す。

(defun lookup-new-agent (spec)
  (let (class location options)
    (setq class (car spec) spec (cdr spec))
    (if (stringp (car spec))
	(setq location (car spec) spec (cdr spec)))
    (setq options spec)
    (lookup-agent-init (lookup-make-agent class location options))))

(defun lookup-agent-init (agent)
  ;; set the agent ID
  (let ((class (symbol-name (lookup-agent-class agent)))
	(location (or (plist-get (lookup-agent-options agent) ':alias)
		      (lookup-agent-location agent))))
    (lookup-agent-set-id agent (if location
				   (concat class "+" location)
				 class)))
  ;; set the agent title
  (lookup-agent-set-title agent (or (lookup-agent-option agent ':title)
				    (lookup-agent-id agent)))
  agent)

(defun lookup-agent-setup (agent)
  (unless (lookup-agent-get-property agent 'setup)
    (let ((lookup-proceeding-message
	   (format "Setting up %s" (lookup-agent-id agent)))
	  dicts enable disable select unselect name)
      (lookup-proceeding-message nil)
      ;; Setup agent by calling the setup function of each agent.
      (lookup-agent-require agent)
      (setq dicts (lookup-agent-command agent 'setup))
      ;; Restrict dictionaries by the options `:enable' and `:disable'.
      (setq enable (lookup-agent-option agent ':enable)
	    disable (lookup-agent-option agent ':disable))
      (when (or enable disable)
	(setq dicts (lookup-grep (lambda (dict)
				   (setq name (lookup-dictionary-name dict))
				   (and (or (not enable) (member name enable))
					(not (member name disable))))
				 dicts)))
      ;; Rearrange the order of dictionaries by the option `:enable'.
      (when enable
	(setq dicts (sort dicts (lambda (d1 d2)
				  (let ((name1 (lookup-dictionary-name d1))
					(name2 (lookup-dictionary-name d2)))
				    (> (length (member name1 enable))
				       (length (member name2 enable))))))))
      ;; Select dictionaries by the options `:select' and `:unselect'.
      (setq select (lookup-agent-option agent ':select)
	    unselect (lookup-agent-option agent ':unselect))
      (lookup-foreach (lambda (dict)
			(setq name (lookup-dictionary-name dict))
			(if (and (or (not select) (member name select))
				 (not (member name unselect)))
			    (lookup-dictionary-set-selected dict t)))
		      dicts)
      ;; Initialize dictionaries.
      (lookup-foreach 'lookup-dictionary-init dicts)
      (lookup-agent-set-dictionaries agent dicts)
      (lookup-proceeding-message t)
      (lookup-agent-put-property agent 'setup t))))

(defun lookup-agent-clear (agent)
  (when (lookup-agent-get-property agent 'setup)
    (let ((lookup-proceeding-message
	   (concat "Clearing " (lookup-agent-id agent))))
      (lookup-proceeding-message nil)
      (lookup-agent-command agent 'clear)
      (lookup-proceeding-message t))))


;;;;;;;;;;;;;;;;;;;;
;: Dictionary
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; 辞書を表わすデータタイプ。全ての辞書はこのタイプのオブジェクトとして
;; 生成される。

;; Structure:
;;
;;   [:dictionary AGENT CODE NAME TITLE PLIST]
;;
;; AGENT - 辞書が属する検索エージェントのオブジェクト。
;; CODE  - 辞書を特定するための任意のオブジェクト。AGENT と CODE の組により
;;         辞書が一意に決定される。
;; NAME  - 辞書名。
;; TITLE - 辞書のタイトル。
;; PLIST - 属性リストの保持。

;; Properties:
;;
;; id       - 辞書ID。
;; selected - 辞書が選択されている場合に non-nil。
;; defaults - 辞書のデフォルトの設定を保持する。
;; methods, gaiji-table, headings - オプションの保持。

(lookup-defstruct dictionary (agent code name title)
  :with-properties '(id methods headings gaiji-table defaults))

(defun lookup-new-dictionary (agent code name &optional title)
  (let ((dictionary (lookup-make-dictionary agent code name nil)))
    (if title (lookup-dictionary-set-default dictionary ':title title))
    dictionary))

(defsubst lookup-dictionary-selected-p (dictionary)
  (lookup-dictionary-get-property dictionary 'selected))

(defsubst lookup-dictionary-set-selected (dictionary value)
  (lookup-dictionary-put-property dictionary 'selected value))

;; Definitions:
;;
;; 辞書は検索のためのいくつかのコマンドを持つ。コマンドは、それが属する
;; 検索エージェントと同ファイルにて定義される。辞書コマンドはエージェント・
;; コマンドと同様にして定義され、現在、次のものが意味を持つ。
;;
;; search DICTIONARY QUERY
;;   辞書から QUERY を検索する。QUERY の解釈は辞書に一任される。
;;   上記 `Query' の節を参照。検索の結果見付かった entry のリストを返す。
;; content DICTIONARY ENTRY
;;   ENTRY の内容を文字列として返す。特に整形は行なわずに、辞書から得られる
;;   ままそのままを返せばよい。整形は別のプロセスによって行なわれる。関数
;;   `lookup-insert-content' を参照。
;; open DICTIONARY ENTRY
;;   ENTRY を特別な関数でオープンする。例えば ENTRY が URL の情報を保持して
;;   いるなら、ブラウザでそれをオープンすることが考えられる。
;; gaiji DICTIONARY CODE
;;   辞書が外字(通常の文字セットに含まれない文字)を持つ場合、その情報を返す。
;;   CODE は外字を特定するための文字列。返却値は、xbm 形式のビットマップを
;;   表わす文字列とする。
;;
;; 辞書はいくつかの固有の標準設定を持つ。これはエージェントの標準設定と
;; 同様に定義され、ユーザ・オプションにより上書き可能となっている。次の
;; `Options' の項を参照。

(defun lookup-dictionary-command-p (dictionary command)
  ;; 辞書コマンドをチェックする。
  (let ((agent (lookup-dictionary-agent dictionary)))
    (lookup-agent-ref agent command)))

(defun lookup-dictionary-command (dictionary command &rest args)
  ;; 辞書コマンドを実行する。
  (let ((agent (lookup-dictionary-agent dictionary)))
    (apply (lookup-agent-ref agent command) dictionary args)))

;; Options:
;;
;; 辞書はいくつかのオプションを持つことが出来る。オプションはユーザにより
;; 設定されるが、プログラムでそのデフォルト値を設定することも出来る。
;; 設定には次の種類がある。
;;
;; 変数 `lookup-dictionary-options-alist', `lookup-default-dictionary-options'
;;   - ユーザ指定のオプション設定。
;; デフォルト - プログラムによる設定。`lookup-dictionary-set-default' を参照。
;; エージェント・オプション - 辞書が属するエージェントのオプションを継承する。

(defun lookup-dictionary-set-default (dictionary key value)
  ;; 辞書のデフォルトの設定を行なう。これはユーザによるオプション指定が
  ;; ない場合に利用される。
  (let ((defaults (plist-put (lookup-dictionary-defaults dictionary)
			     key value)))
    (lookup-dictionary-set-defaults dictionary defaults)))

(defun lookup-dictionary-option (dictionary key &optional inherit)
  ;; 辞書のオプションを参照する。変数 `lookup-dictionary-options-alist'、
  ;; `lookup-default-dictionary-options'、デフォルトの順に探索する。
  ;; オプション引数 INHERIT が non-nil の場合、引き続いてエージェント・
  ;; オプションからも探索する．
  (or (lookup-assq-ref (lookup-assoc-ref lookup-dictionary-options-alist
					 (lookup-dictionary-id dictionary))
		       key)
      (lookup-assq-ref lookup-default-dictionary-options key)
      (plist-get (lookup-dictionary-defaults dictionary) key)
      (if inherit
	  (lookup-agent-option (lookup-dictionary-agent dictionary) key))))

;; Initialize:
;;
;; 辞書はエージェントの :setup コマンドにより生成され、エージェントと同時に
;; 初期化される。いくつかの代表的な属性がセットされる。

(defsubst lookup-dictionary-default-method (dictionary)
  (or (lookup-dictionary-option dictionary ':default-method t)
      lookup-default-method))

(defsubst lookup-dictionary-hiragana (dictionary)
  (lookup-dictionary-option dictionary ':hiragana t))

(defsubst lookup-dictionary-expander (dictionary)
  (lookup-dictionary-option dictionary ':expander t))

(defsubst lookup-dictionary-expand-filter (dictionary)
  (lookup-dictionary-option dictionary ':expand-filter t))

(defsubst lookup-dictionary-stemmer (dictionary)
  (lookup-dictionary-option dictionary ':stemmer t))

(defun lookup-dictionary-init (dictionary)
  (let ((id (concat (lookup-agent-id (lookup-dictionary-agent dictionary))
		    ":" (lookup-dictionary-name dictionary))))
    (lookup-dictionary-set-id dictionary id))
  (let ((title (or (lookup-dictionary-option dictionary ':title)
		   (lookup-dictionary-name dictionary))))
    (lookup-dictionary-set-title dictionary title))
  (if (lookup-dictionary-hiragana dictionary)
      (require 'lookup-kanji))
  (let ((methods (lookup-dictionary-option dictionary ':methods t)))
    (if (or (lookup-dictionary-hiragana dictionary)
	    (lookup-dictionary-expander dictionary))
	(setq methods (append methods '(expansion))))
    (if (lookup-dictionary-stemmer dictionary)
	(setq methods (append methods '(stemming))))
    (lookup-dictionary-set-methods dictionary methods))
  (let ((gaiji-table (or (lookup-dictionary-option dictionary ':gaiji-table)
			 (lookup-make-gaiji-table))))
    (lookup-dictionary-set-gaiji-table dictionary gaiji-table))
  ;; Keep some options as properties to save search step.
  (let ((headings (lookup-dictionary-option dictionary ':headings t)))
    (lookup-dictionary-set-headings dictionary headings))
  dictionary)


;;;;;;;;;;;;;;;;;;;;
;: Entry
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; エントリの情報を表わすデータタイプ。全てのエントリはこのタイプの
;; オブジェクトとして生成される。

;; Structure:
;;
;;   [:entry DICTIONARY CODE HEADING PLIST]
;;
;; DICTIONARY は、エントリが属する辞書のオブジェクト。
;; CODE は、エントリを特定するための任意のオブジェクト。DICTIONARY と CODE
;;   の組によってエントリは一意に決定される。
;; HEADING は、エントリの見出し語。これは表示の際に用いられる。
;; PLIST は、属性リストの保持に使われる。

;; Properties:
;; 

(lookup-defstruct entry (dictionary code heading)
  :with-properties '(prefix compound reference jump))

(defun lookup-new-entry (dictionary code heading)
  (let ((entry (lookup-make-entry dictionary code heading)))
    (lookup-arrange-heading entry)))

(defsubst lookup-entry-refered-p (entry)
  (lookup-contents-cache-get entry lookup-enable-format))

;; Functions:

(defun lookup-entry-id (entry)
  (concat (lookup-dictionary-id (lookup-entry-dictionary entry))
	  "/" (lookup-entry-heading entry)))

(defun lookup-copy-entry (entry)
  (lookup-make-entry (lookup-entry-dictionary entry)
		     (lookup-entry-code entry) (lookup-entry-heading entry)))

(defun lookup-entry-compare (e1 e2)
  (and (eq (lookup-entry-dictionary e1) (lookup-entry-dictionary e2))
       (equal (lookup-entry-code e1) (lookup-entry-code e2))))

(defsubst lookup-unique-entries (entries)
  (lookup-nunique entries 'lookup-entry-compare))


;;;;;;;;;;;;;;;;;;;;
;: Reference
;;;;;;;;;;;;;;;;;;;;

(defun lookup-make-reference (dictionary code heading)
  (let ((entry (lookup-make-entry dictionary code heading)))
    (aset entry 0 ':reference)
    entry))

(defun lookup-reference-p (object)
  (and (vectorp object) (eq (aref object 0) ':reference)))

(defsubst lookup-reference-dynamic-p (reference)
  (lookup-entry-get-property reference 'dynamic-search))

(defsubst lookup-reference-make-dynamic (reference function)
  (lookup-entry-put-property reference 'dynamic-search function))

(defun lookup-reference-refer (reference)
  (let ((function (lookup-entry-get-property reference 'dynamic-search)))
    (when function
      (lookup-reference-set-entries reference (funcall function reference))
      (lookup-reference-make-dynamic reference nil)
      (lookup-entry-put-property reference 'refered t))))

(defsubst lookup-reference-refered-p (reference)
  (or (lookup-entry-get-property reference 'refered)
      (unless (lookup-entry-get-property reference 'dynamic-search)
	(let ((entries (lookup-reference-entries reference)))
	  (catch 'result
	    (while entries
	      (unless (lookup-entry-refered-p reference)
		(throw 'result nil))
	      (setq entries (cdr entries)))
	    t)))))

(defsubst lookup-reference-entries (reference)
  (lookup-entry-get-property reference 'linked-entries))

(defsubst lookup-reference-set-entries (reference entries)
  (lookup-entry-put-property reference 'linked-entries entries))

;; link:

(defun lookup-set-link (start end reference)
  ;; バッファの START から END までのリージョンを REFERENCE へのリンクにする。
  (add-text-properties start end (list 'face 'lookup-reference-face
				       'mouse-face 'highlight
				       'lookup-reference reference)))

(defun lookup-get-link (position)
  ;; バッファの POSITION 位置からリンクされている entry を返す。
  (get-text-property position 'lookup-reference))

(defun lookup-goto-next-link ()
  ;; バッファの次のリンク位置にジャンプする。なければ nil を返す。
  (let ((p (point)))
    (and (setq p (next-single-property-change p 'lookup-reference))
	 (or (get-text-property p 'lookup-reference)
	     (setq p (next-single-property-change p 'lookup-reference)))
	 (goto-char p))))

(defun lookup-goto-previous-link ()
  ;; バッファの前のリンク位置にジャンプする。なければ nil を返す。
  (let ((p (point)))
    (and (setq p (previous-single-property-change p 'lookup-reference))
	 (or (get-text-property p 'lookup-reference)
	     (setq p (previous-single-property-change p 'lookup-reference)))
	 (goto-char p))))


;;;;;;;;;;;;;;;;;;;;
;: Session
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; 検索結果を保持するためのデータタイプ。一回の検索が行なわれる度にこの
;; タイプのデータが生成され、検索履歴の参照等のために用いられる。

;; Structure:
;;
;;   [:session QUERY ENTRIES PLIST]
;;
;; QUERY は、検索パターンのオブジェクト。`Search Query' を参照。
;; ENTRIES は、検索の結果見付かったエントリのリスト。
;; PLIST は、属性リストの保持に使われる。

;; Properties:
;;
;; excursion - カーソル位置等の実行状態の保持。

(lookup-defstruct session (module type)
  :with-properties '(query entries excursion))

(defsubst lookup-session-ref (session key)
  (get (lookup-session-type session) key))

(defun lookup-session-display (session)
  (if lookup-last-session
      (lookup-session-save-excursion lookup-last-session))
  (funcall (lookup-session-ref session 'display) session)
  (setq lookup-current-session session
	lookup-last-session session))

(defun lookup-session-save-excursion (session)
  (let ((func (lookup-session-ref session 'excursion)))
    (when func
      (lookup-session-set-excursion session (funcall func)))))


;;;;;;;;;;;;;;;;;;;;
;: History
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; 検索履歴を保持するためのデータタイプ。これは内部にオブジェクトのリスト
;; を持ち、それを追加・参照することが出来る。履歴はリストの位置(position)
;; も記憶しており、履歴へのオブジェクトの追加は、リストの最後に参照された
;; オブジェクトの直後へと行なわれる。(つまり、履歴を戻ったところでオブジェ
;; クトを追加すると、以前の繋がりは失なわれる)

;; Structure:
;;
;;   (STACK . POSITION)
;;
;; STACK は、履歴を積んでいくオブジェクトのリスト。
;; POSITION は、現在参照されている履歴の位置。
;;
;; 新しいオブジェクトほど、STACK の先頭に置かれる。履歴の位置は 1 から
;; 始まる。ただし、履歴へのアクセスは下記の専用関数にて行ない、これらの
;; 変数は直接操作しない方がよい。

(defalias 'lookup-make-history 'cons)
(defalias 'lookup-history-stack 'car)
(defalias 'lookup-history-set-stack 'setcar)
(defalias 'lookup-history-position 'cdr)
(defalias 'lookup-history-set-position 'setcdr)

(defsubst lookup-new-history ()
  ;; 新しい history オブジェクトを生成して返す。
  (lookup-make-history nil 0))

;; Interface:
;;
;; 履歴へのオブジェクト追加は、関数 `lookup-history-push' により行なう。
;; 追加されたオブジェクトは、関数 `lookup-history-ref' により参照すること
;; が出来る。関数 `lookup-history-length' により、履歴の長さを知ることが
;; 出来る。

(defun lookup-history-push (history object)
  ;; 現在の参照位置にオブジェクトを追加する。もしその位置に同じオブジェクト
  ;; が既に追加されていれば何もしない。
  (let ((stack (lookup-history-stack history))
	(position (lookup-history-position history)))
    (setq stack (nthcdr (- (length stack) position) stack))
    (unless (eq object (car stack))
      (lookup-history-set-stack history (cons object stack))
      (lookup-history-set-position history (1+ position)))))

(defun lookup-history-ref (history &optional n do-not-move)
  ;; 現在の参照位置から N 番目にあるオブジェクトを返す。
  ;; 負の数が指定された場合には、履歴を過去に戻る。正の数だと、その逆。
  ;; 省略された場合には現在の参照位置のものを返す。
  ;; 履歴がない場合には `no-object'、最初の位置から前に戻ろうとしたら
  ;; `first'、最後から先に進もうとしたら `last' を返す。
  ;; DO-NOT-MOVE に non-nil を指定した場合、参照後の移動を行なわない。
  (let* ((stack (lookup-history-stack history))
	 (position (lookup-history-position history))
	 (length (length stack)))
    (setq n (or n 0))
    (cond
     ((eq length 0) 'no-object)
     ((and (= position 1) (< n 0)) 'first)
     ((and (= position length) (> n 0)) 'last)
     (t
      (setq position (+ position n)
	    position (if (< position 1) 1
		       (if (> position length) length position)))
      (unless do-not-move
	(lookup-history-set-position history position))
      (car (nthcdr (- length position) stack))))))

(defun lookup-history-length (history)
  ;; 履歴の長さを返す。
  (length (lookup-history-stack history)))

(defun lookup-history-clear (history)
  ;; 履歴をクリアする。
  (lookup-history-set-stack history nil)
  (lookup-history-set-position history 0))


;;;;;;;;;;;;;;;;;;;;
;: Gaiji
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; 外字を扱うための関数群。これには次の三つのデータタイプがある。
;;
;; 外字         - 外字情報を保持するためのオブジェクト
;; 外字グリフ   - 外字そのものであるオブジェクト
;; 外字テーブル - 外字を記録・参照するためのオブジェクト
;;
;; 外字の扱いは、Emacs の種類や環境によって異なる。

(cond
 ((featurep 'xemacs)
  (defun lookup-glyph-compose (xbm)
    (let (width height data)
      (with-temp-buffer
	(insert xbm)
	(goto-char (point-min))
	(if (re-search-forward "width[ \t]+\\([0-9]+\\)")
	    (setq width (string-to-int (match-string 1))))
	(if (re-search-forward "height[ \t]+\\([0-9]+\\)")
	    (setq height (string-to-int (match-string 1))))
	(while (re-search-forward "0x\\(..\\)" nil t)
	  (setq data (cons (string-to-int (match-string 1) 16) data)))
	(setq data (concat (nreverse data))))
      (make-glyph (vector 'xbm :data (list width height data)))))

  (defun lookup-glyph-paste (start end glyph)
    (set-extent-property (extent-at start nil 'lookup-gaiji) 'invisible t)
    (let (extent extents)
      (while (setq extent (extent-at start nil nil extent 'at))
	(if (eq (extent-start-position extent) (extent-end-position extent))
	    (setq extents (cons extent extents))))
      (while extents
	(set-extent-endpoints (car extents) end end)
	(setq extents (cdr extents)))
      (set-extent-begin-glyph (make-extent end end) glyph)))
  )
 ((featurep 'mule)
  (defun lookup-bitmap-compose (xbm)
    (require 'bitmap)
    (with-temp-buffer
      (insert xbm)
      (let ((cmp (bitmap-decode-xbm
		  (bitmap-read-xbm-buffer (current-buffer)))))
	(bitmap-compose (aref cmp 0)))))

  (defun lookup-bitmap-paste (start end glyph)
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'after-string glyph)))
  ))

;; Gaiji:
;;
;; 外字オブジェクトは次の構造を持つ。
;;
;;   (GLYPH . ALTERNATE)
;;
;; GLYPH は、下記の外字グリフ。
;; ALTERNATE は、外字に対応する代替文字列。
;;
;; GLYPH は、外字の表示の際に用いられる。しかし外字は、読むのには適しているが
;; 検索やコピーには向いていない。そのため、それらを行なう際には ALTERNATE が
;; 代わりに利用される。

(defalias 'lookup-make-gaiji 'cons)
(defalias 'lookup-gaiji-glyph 'car)
(defalias 'lookup-gaiji-set-glyph 'setcar)
(defalias 'lookup-gaiji-alternate 'cdr)
(defalias 'lookup-gaiji-set-alternate 'setcdr)

(defun lookup-gaiji-insert (gaiji)
  (let ((glyph (lookup-gaiji-glyph gaiji))
	(alter (lookup-gaiji-alternate gaiji))
	(start (point)))
    (insert alter)
    (if (not (eq glyph alter))
	(put-text-property start (point) 'lookup-gaiji glyph))))

;; Gaiji Glyph:
;;
;; 外字グリフは、外字の外字たるゆえん、任意のビットマップを示すオブジェクト。
;; これは GNU Emacs と XEmacs とで扱いが異なる。
;;
;; 1) GNU Emacs / Mule の場合
;;
;; 外字は bitmap-mule パッケージを利用して生成される。内部的にはこれは、
;; `compose-string' により生成される文字列である。
;;
;; 2) XEmacs の場合
;;
;; 組み込みの glyph の機能を利用して生成する。つまり、外字を一種の画像
;; データとして扱う。
;;
;; 外字を生成する関数は、変数 `lookup-gaiji-compose-function' により指定
;; される。これは bitmap-mule を利用する場合には `lookup-bitmap-compose'
;; となり、XEmacs の場合には `lookup-glyph-compose' となる。これらの関数は
;; "lookup-gaiji.el" で定義している。
;;
;; また、同様に外字を挿入する関数は、変数 `lookup-gaiji-insert-function'
;; により与えられる。これはそれぞれ、`insert' 及び `lookup-glyph-insert'
;; となる。

(defun lookup-gaiji-glyph-possible-p ()
  ;; 外字グリフを表示可能な状態にあるかどうかチェックする。
  (and window-system lookup-gaiji-compose-function))

(defun lookup-gaiji-glyph-compose (xbm)
  ;; 外字グリフを生成する。データは XBM 形式の文字列として与える。
  (funcall lookup-gaiji-compose-function xbm))

(defun lookup-gaiji-glyph-paste (start end glyph)
  ;; 外字グリフを貼り付ける。
  (funcall lookup-gaiji-paste-function start end glyph))

;; Gaiji Table:
;;
;; 外字テーブルは、外字オブジェクトを記録・参照するためのオブジェクト。
;; 外字オブジェクトは数が多い上に頻繁に参照されるため、ハッシュ(obarray)を
;; 用いて高速に参照出来るようにしている。

(defsubst lookup-make-gaiji-table ()
  ;; 外字テーブルを生成する。
  (make-vector 377 0))

(defun lookup-gaiji-table-set (table code gaiji)
  ;; 外字テーブルに外字オブジェクトをセットする。
  (set (intern code table) gaiji))

(defun lookup-gaiji-table-ref (table code)
  ;; 外字テーブルから外字オブジェクトを参照する。
  (let ((symbol (intern code table)))
    (if (boundp symbol) (symbol-value symbol))))

;; Table Format:
;;
;; 関数 `lookup-new-gaiji-table' により、外字テーブルの初期値を定義することが
;; 出来る。これにより、外字を特定の文字列で置き換えるようなことが可能となる。
;; これは次のようにして行なう。
;;
;;   (lookup-new-gaiji-table
;;    '((CODE1 GLYPH1 [ALTERNATE1]) (CODE2 GLYPH2 [ALTERNATE2]) ...))
;;
;; CODE は辞書における外字コード。GLYPH は外字グリフオブジェクトか任意の
;; 文字列、あるいはそのどちらかを返す評価可能な式。ALTERNATE は省略可能で、
;; 外字の代替文字列を指定する。
;;
;; 同じ文字列を設定する場合にも、GLYPH と ALTERNATE とでは意味が異なる。
;; GLYPH は画面出力に用いられるため、アクセント文字などを含む任意の文字
;; 列を指定出来るが、ALTERNATE は検索等に用いられるため、一般的な ASCII 
;; コードなどを指定した方がよい。

(defun lookup-new-gaiji-table (spec)
  ;; SPEC の指定に従って外字テーブルを作成する。
  (let ((table (lookup-make-gaiji-table)) form glyph alter)
    (while spec
      (setq form (car spec) glyph (cadr form) alter (or (nth 2 form) glyph))
      (if (and (featurep 'xemacs) glyph)
	  (setq glyph (make-glyph (vector 'string ':data glyph))))
      (lookup-gaiji-table-set table (car form) (lookup-make-gaiji glyph alter))
      (setq spec (cdr spec)))
    table))

(provide 'lookup-types)

;;; lookup-types.el ends here
