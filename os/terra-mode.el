;;; terra-mode.el -- a major-mode for Terra, based on lua-mode.el

;; Copyright (C) 1997, 2001, 2004, 2006, 2007, 2010, 2011 Free Software Foundation, Inc.

;; Author: 2011 immerrr <immerrr+lua@gmail.com>
;;         2010-2011 Reuben Thomas <rrt@sc3d.org>
;;         2006 Juergen Hoetzel <juergen@hoetzel.info>
;;         2004 various (support for Lua 5 and byte compilation)
;;         2001 Christian Vogler <cvogler@gradient.cis.upenn.edu>
;;         1997 Bret Mogilefsky <mogul-lua@gelatinous.com> starting from
;;              tcl-mode by Gregor Schmid <schmid@fb3-s7.math.tu-berlin.de>
;;              with tons of assistance from
;;              Paul Du Bois <pld-lua@gelatinous.com> and
;;              Aaron Smith <aaron-lua@gelatinous.com>.
;;
;; URL:         http://immerrr.github.com/lua-mode
;; Version:     20111107
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;; Keywords: languages, processes, tools


;;; Commentary:

;; Thanks to Vedat Hallac <github.com/vhallac> for sharing some of
;; his fixes and updates to core indentation logics

;; Thanks to Rafael Sanchez <rafael@cornerdimension.com> for patch
;; adding lua-mode to interpreter-mode-alist

;; Thanks to Leonardo Etcheverry <leo@kalio.net> for enabling
;; narrow-to-defun functionality

;; Thanks to Tobias Polzin <polzin@gmx.de> for function indenting
;; patch: Indent "(" like "{"

;; Thanks to Fabien <fleutot@gmail.com> for imenu patches.

;; Thanks to Simon Marshall <simonm@mail.esrin.esa.it> and Olivier
;; Andrieu <oandrieu@gmail.com> for font-lock patches.

;; Additional font-lock highlighting and indentation tweaks by
;; Adam D. Moss <adam@gimp.org>.

;; INSTALLATION:

;; To install, just copy this file into a directory on your load-path
;; (and byte-compile it). To set up Emacs to automatically edit files
;; ending in ".lua" or with a lua hash-bang line using terra-mode add
;; the following to your init file:
;;
;; (autoload 'terra-mode "terra-mode" "Lua editing mode." t)
;; (add-to-list 'auto-mode-alist '("\\.\\(t\\|terra\\)$" . terra-mode))
;; (add-to-list 'interpreter-mode-alist '("t" . terra-mode))

;; Usage

;; terra-mode supports c-mode style formatting and sending of
;; lines/regions/files to a Lua interpreter. An interpreter (see
;; variable `terra-default-application') will be started if you try to
;; send some code and none is running. You can use the process-buffer
;; (named after the application you chose) as if it were an
;; interactive shell. See the documentation for `comint.el' for
;; details.

;; Terra-mode works with Hide Show minor mode (see ``hs-minor-mode``).

;; Key-bindings

;; To see all the keybindings for Lua mode, look at `terra-setup-keymap'
;; or start `terra-mode' and type `\C-h m'.
;; The keybindings may seem strange, since I prefer to use them with
;; terra-prefix-key set to nil, but since those keybindings are already used
;; the default for `terra-prefix-key' is `\C-c', which is the conventional
;; prefix for major-mode commands.

;; You can customise the keybindings either by setting `terra-prefix-key'
;; or by putting the following in your .emacs
;;      (define-key terra-mode-map <your-key> <function>)
;; for all the functions you need.


;;; Code:
(eval-when-compile
  (require 'cl))

(require 'comint)

;; Local variables
(defgroup terra nil
  "Major mode for editing terra code."
  :prefix "terra-"
  :group 'languages)

(defcustom terra-indent-level 4
  "Amount by which Terra subexpressions are indented."
  :type 'integer
  :group 'terra)

(defcustom terra-comment-start "-- "
  "Default value of `comment-start'."
  :type 'string
  :group 'terra)

(defcustom terra-comment-start-skip "-- "
  "Default value of `comment-start-skip'."
  :type 'string
  :group 'terra)

(defcustom terra-default-application "terra"
  "Default application to run in terra subprocess."
  :type 'string
  :group 'terra)

(defcustom terra-default-command-switches (list "-i")
  "Command switches for `terra-default-application'.
Should be a list of strings."
  :type '(repeat string)
  :group 'terra)

(defcustom terra-always-show t
  "*Non-nil means display terra-process-buffer after sending a command."
  :type 'boolean
  :group 'terra)

(defcustom terra-search-url-prefix "http://www.terra.org/manual/5.1/manual.html#pdf-"
  "*URL at which to search for documentation on a word"
  :type 'string
  :group 'terra)

(defvar terra-process nil
  "The active Terra subprocess")

(defvar terra-process-buffer nil
  "Buffer used for communication with Terra subprocess")

(defun terra--customize-set-prefix-key (prefix-key-sym prefix-key-val)
  ;; FIXME: enable assertion, it requires 'cl and I'm not sure of its availability
  ;; (assert (eq prefix-key-sym 'terra-prefix-key))
  (set prefix-key-sym (if (and prefix-key-val (> (length prefix-key-val) 0))
                          ;; read-kbd-macro returns a string or a vector
                          ;; in both cases (elt x 0) is ok
                          (elt (read-kbd-macro prefix-key-val) 0)))
  (if (fboundp 'terra-prefix-key-update-bindings)
      (terra-prefix-key-update-bindings))
  (message "prefix key set to %S"  (single-key-description (eval prefix-key-sym))))

(defcustom terra-prefix-key "\C-c"
  "Prefix for all terra-mode commands."
  :type 'string
  :group 'terra
  :set 'terra--customize-set-prefix-key
  :get '(lambda (sym)
          (let ((val (eval sym))) (if val (single-key-description (eval sym)) ""))))

(defvar terra-mode-menu (make-sparse-keymap "Terra")
  "Keymap for terra-mode's menu.")

(defvar terra-prefix-mode-map
  (eval-when-compile
    (let ((result-map (make-sparse-keymap)))
      (mapc (lambda (key_defn)
              (define-key result-map (read-kbd-macro (car key_defn)) (cdr key_defn)))
            '(("C-l" . terra-send-buffer)
              ("C-f" . terra-search-documentation)
              ("C-;" . terra-mark-all-multiline-literals)))
      result-map))
  "Keymap that is used to define keys accessible by `terra-prefix-key'.

If the latter is nil, the keymap translates into `terra-mode-map' verbatim.")

(defvar terra-mode-map
  (let ((result-map (make-sparse-keymap))
        prefix-key)
    (mapc (lambda (key_defn)
            (define-key result-map (read-kbd-macro (car key_defn)) (cdr key_defn)))
          ;; here go all the default bindings
          ;; backquote enables evaluating certain symbols by comma
          `(("}" . terra-electric-match)
            ("]" . terra-electric-match)
            (")" . terra-electric-match)))
    (define-key result-map [menu-bar terra-mode] (cons "Terra" terra-mode-menu))

    ;; FIXME: see if the declared logic actually works
    ;; handle prefix-keyed bindings:
    ;; * if no prefix, set prefix-map as parent, i.e.
    ;;      if key is not defined look it up in prefix-map
    ;; * if prefix is set, bind the prefix-map to that key
    (if (boundp 'terra-prefix-key)
        (define-key result-map (vector terra-prefix-key) terra-prefix-mode-map)
      (set-keymap-parent result-map terra-prefix-mode-map))
    result-map)
  "Keymap used in terra-mode buffers.")

(defvar terra-electric-flag t
  "If t, electric actions (like automatic reindentation) will happen when an electric
 key like `{' is pressed")
(make-variable-buffer-local 'terra-electric-flag)

(defcustom terra-prompt-regexp "[^\n]*\\(>[\t ]+\\)+$"
  "Regexp which matches the Terra program's prompt."
  :type  'regexp
  :group 'terra)

(defcustom terra-traceback-line-re
  "^\\(?:[\t ]*\\|.*>[\t ]+\\)\\([^\n\t ]+\\):\\([0-9]+\\):"
  "Regular expression that describes tracebacks and errors."
  :type 'regexp
  :group 'terra)

(defcustom terra-indent-string-contents nil
  "If non-nil, contents of multiline string will be indented.
Otherwise leading amount of whitespace on each line is preserved."
  :group 'terra
  :type 'boolean)

(defcustom terra-jump-on-traceback t
  "*Jump to innermost traceback location in *terra* buffer.  When this
variable is non-nil and a traceback occurs when running Terra code in a
subprocess, jump immediately to the source code of the innermost
traceback location."
  :type 'boolean
  :group 'terra)

(defvar terra-mode-hook nil
  "Hooks called when Terra mode fires up.")

(defvar terra-region-start (make-marker)
  "Start of special region for Terra communication.")

(defvar terra-region-end (make-marker)
  "End of special region for Terra communication.")

(defvar terra-emacs-menu
  '(["Restart With Whole File" terra-restart-with-whole-file t]
    ["Kill Process" terra-kill-process t]
    ["Hide Process Buffer" terra-hide-process-buffer t]
    ["Show Process Buffer" terra-show-process-buffer t]
    ["Beginning Of Proc" terra-beginning-of-proc t]
    ["End Of Proc" terra-end-of-proc t]
    ["Set Terra-Region Start" terra-set-terra-region-start t]
    ["Set Terra-Region End" terra-set-terra-region-end t]
    ["Send Terra-Region" terra-send-terra-region t]
    ["Send Current Line" terra-send-current-line t]
    ["Send Region" terra-send-region t]
    ["Send Proc" terra-send-proc t]
    ["Send Buffer" terra-send-buffer t]
    ["Search Documentation" terra-search-documentation t])
  "Emacs menu for Terra mode.")

(defvar terra-font-lock-keywords
  (eval-when-compile
    (list
     ;; Handle variable names
     ;;  local blalba =
     ;;        ^^^^^^
     '("\\(local[ \t]+\\(\\sw+\\)[ \t]*=\\)"
       (2 font-lock-variable-name-face))

     ;; Function name declarations.
     '("^[ \t]*\\_<\\(\\(local[ \t]+\\)?function\\)\\_>[ \t]+\\(\\(\\sw:\\|\\sw\\.\\|\\sw_\\|\\sw\\)+\\)"
       (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))

     ;; Terra name declarations.
     '("^[ \t]*\\_<\\(\\(local[ \t]+\\)?terra\\)\\_>[ \t]+\\(\\(\\sw:\\|\\sw\\.\\|\\sw_\\|\\sw\\)+\\)"
       (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))

     ;; Handle function names in assignments
     '("\\(\\(\\sw:\\|\\sw\\.\\|\\sw_\\|\\sw\\)+\\)[ \t]*=[ \t]*\\(function\\)\\_>"
       (1 font-lock-function-name-face nil t) (3 font-lock-keyword-face))

     ;; Keywords.
     (concat "\\_<"
             (regexp-opt '("and" "break" "do" "else" "elseif" "end" "false"
                           "for" "function" "if" "in" "local" "nil" "not"
                           "or" "repeat" "return" "then" "true" "until"
                           "while"
                           "bool" "int" "uint" "float" "double"
                           "int8" "uint8" "int16" "int64"
                           "uint16" "int32" "uint32" "uint64"
                           "intptr" "ptrdiff"
                           "array" "arrayof"
                           "rawstring" "sizeof" "vector" "vectorof"
                           "goto" "quote" "struct" "terra" "tuple" "union" "var") t)
             "\\_>")

     "Default expressions to highlight in Terra mode.")))

(defvar terra-imenu-generic-expression
  '((nil "^[ \t]*\\(?:local[ \t]+\\)?function[ \t]+\\(\\(\\sw:\\|\\sw_\\|\\sw\\.\\|\\sw\\)+\\)" 1))
  "Imenu generic expression for terra-mode.  See `imenu-generic-expression'.")

(defvar terra-sexp-alist '(("then" . "end")
                         ("function" . "end")
                         ("terra" . "end")
                         ("quote" . "end")
                         ("do" . "end")))

(defvar terra-mode-abbrev-table nil
  "Abbreviation table used in terra-mode buffers.")

(define-abbrev-table 'terra-mode-abbrev-table
  ;; Emacs 23 introduced :system property that prevents abbrev
  ;; entries from being written to file specified by abbrev-file-name
  ;;
  ;; Emacs 22 and earlier had this functionality implemented
  ;; by simple nil/non-nil flag as positional parameter
  (if (>= emacs-major-version 23)
      '(("end"    "end"    terra-indent-line :system t)
        ("else"   "else"   terra-indent-line :system t)
        ("elseif" "elseif" terra-indent-line :system t))
    '(("end"    "end"      terra-indent-line nil 'system)
      ("else"   "else"     terra-indent-line nil 'system)
      ("elseif" "elseif"   terra-indent-line nil 'system))))

(eval-and-compile
  (defalias 'terra-make-temp-file
    (if (fboundp 'make-temp-file)
        'make-temp-file
      (lambda (prefix &optional dir-flag) ;; Simple implementation
        (expand-file-name
         (make-temp-name prefix)
         (if (fboundp 'temp-directory)
             (temp-directory)
           temporary-file-directory))))))

;;;###autoload
(defun terra-mode ()
  "Major mode for editing Terra code.
The following keys are bound:
\\{terra-mode-map}
"
  (interactive)
  (let ((switches nil)
        s)
    (kill-all-local-variables)
    (setq major-mode 'terra-mode)
    (setq mode-name "Terra")
    (setq comint-prompt-regexp terra-prompt-regexp)
    (make-local-variable 'terra-default-command-switches)
    (set (make-local-variable 'beginning-of-defun-function)
         'terra-beginning-of-proc)
    (set (make-local-variable 'end-of-defun-function) 'terra-end-of-proc)
    (set (make-local-variable 'indent-line-function) 'terra-indent-line)
    (set (make-local-variable 'comment-start) terra-comment-start)
    (set (make-local-variable 'comment-start-skip) terra-comment-start-skip)
    (set (make-local-variable 'font-lock-defaults)
         '(terra-font-lock-keywords
           nil nil ((?_ . "w"))))
    (set (make-local-variable 'imenu-generic-expression)
         terra-imenu-generic-expression)
    (setq local-abbrev-table terra-mode-abbrev-table)
    (abbrev-mode 1)
    (make-local-variable 'terra-default-eval)
    (use-local-map terra-mode-map)
    (set-syntax-table (copy-syntax-table))
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?- ". 12")
    (modify-syntax-entry ?* ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?^ ".")
    ;; This might be better as punctuation, as for C, but this way you
    ;; can treat table index as symbol.
    (modify-syntax-entry ?. "_")        ; e.g. `io.string'
    (modify-syntax-entry ?> ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?~ ".")
    (modify-syntax-entry ?\n ">")
    (modify-syntax-entry ?\' "\"")
    (modify-syntax-entry ?\" "\"")
    ;; setup menu bar entry (XEmacs style)
    (if (and (featurep 'menubar)
             (boundp 'current-menubar)
             (fboundp 'set-buffer-menubar)
             (fboundp 'add-menu)
             (not (assoc "Terra" current-menubar)))
        (progn
          (set-buffer-menubar (copy-sequence current-menubar))
          (add-menu nil "Terra" terra-emacs-menu)))
    ;; Append Terra menu to popup menu for Emacs.
    (if (boundp 'mode-popup-menu)
        (setq mode-popup-menu
              (cons (concat mode-name " Mode Commands") terra-emacs-menu)))

    ;; hideshow setup
    (unless (assq 'terra-mode hs-special-modes-alist)
      (add-to-list 'hs-special-modes-alist
                   `(terra-mode
                     ,(regexp-opt (mapcar 'car terra-sexp-alist) 'words) ;start
                     ,(regexp-opt (mapcar 'cdr terra-sexp-alist) 'words) ;end
                     nil terra-forward-sexp)))

    (set (make-local-variable 'parse-sexp-lookup-properties) t)
    (terra-mark-all-multiline-literals)
    (terra--automark-multiline-update-timer)
    (run-hooks 'terra-mode-hook)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(t\\|terra\\)$" . terra-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("terra" . terra-mode))

(defun terra-electric-match (arg)
  "Insert character and adjust indentation."
  (interactive "P")
  (insert-char last-command-event (prefix-numeric-value arg))
  (if terra-electric-flag
      (terra-indent-line))
  (blink-matching-open))

;; private functions

(defun terra-prefix-key-update-bindings ()
  (let (old-cons)
    (if (eq terra-prefix-mode-map (keymap-parent terra-mode-map))
        ;; if prefix-map is a parent, delete the parent
        (set-keymap-parent terra-mode-map nil)
      ;; otherwise, look for it among children
      (if (setq old-cons (rassoc terra-prefix-mode-map terra-mode-map))
          (delq old-cons terra-mode-map)))

    (if (null terra-prefix-key)
        (set-keymap-parent terra-mode-map terra-prefix-mode-map)
      (define-key terra-mode-map (vector terra-prefix-key) terra-prefix-mode-map))))

(defun terra-set-prefix-key (new-key-str)
  "Changes `terra-prefix-key' properly and updates keymaps

This function replaces previous prefix-key binding with a new one."
  (interactive "sNew prefix key (empty string means no key): ")
  (terra--customize-set-prefix-key 'terra-prefix-key new-key-str)
  (terra-prefix-key-update-bindings))

(defun terra-string-p (&optional pos)
  "Returns true if the point is in a string."
  (save-excursion (elt (syntax-ppss pos) 3)))

(defun terra-comment-p (&optional pos)
  "Returns true if the point is in a comment."
  (save-excursion (elt (syntax-ppss pos) 4)))

(defun terra-comment-or-string-p (&optional pos)
  "Returns true if the point is in a comment or string."
  (save-excursion (let ((parse-result (syntax-ppss pos)))
                    (or (elt parse-result 3) (elt parse-result 4)))))

(defun terra-comment-or-string-start (&optional pos)
  "Returns start position of string or comment which contains point.

If point is not inside string or comment, return nil."
  (save-excursion (elt (syntax-ppss pos) 8)))

(defun terra-indent-line ()
  "Indent current line for Terra mode.
Return the amount the indentation changed by."
  (let (indent
        (case-fold-search nil)
        ;; save point as a distance to eob - it's invariant w.r.t indentation
        (pos (- (point-max) (point))))
    (back-to-indentation)
    (if (terra-comment-or-string-p)
        (setq indent (terra-calculate-string-or-comment-indentation)) ;; just restore point position
      (setq indent (max 0 (terra-calculate-indentation nil))))

    (when (not (equal indent (current-column)))
      (delete-region (line-beginning-position) (point))
      (indent-to indent))

    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))

    indent))

(defun terra-calculate-string-or-comment-indentation ()
  "This function should be run when point at (current-indentation) is inside string"
  (if (and (terra-string-p) (not terra-indent-string-contents))
      ;; if inside string and strings aren't to be indented, return current indentation
      (current-indentation)
    ;; otherwise indent by terra-indent-level relative to the line where literal starts
    (save-excursion
      (goto-char (terra-get-multiline-start))
      (+ (current-indentation) terra-indent-level))))

(defun terra-find-regexp (direction regexp &optional limit ignore-p)
  "Searches for a regular expression in the direction specified.
Direction is one of 'forward and 'backward.
By default, matches in comments and strings are ignored, but what to ignore is
configurable by specifying ignore-p. If the regexp is found, returns point
position, nil otherwise.
ignore-p returns true if the match at the current point position should be
ignored, nil otherwise."
  (let ((ignore-func (or ignore-p 'terra-comment-or-string-p))
        (search-func (if (eq direction 'forward)
                         're-search-forward 're-search-backward))
        (case-fold-search nil))
    (catch 'found
      (while (funcall search-func regexp limit t)
        (if (and (not (funcall ignore-func (match-beginning 0)))
                 (not (funcall ignore-func (match-end 0))))
            (throw 'found (point)))))))

(defconst terra-block-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("do" "function" "terra" "repeat" "then"
                   "else" "elseif" "end" "until") t)
     "\\_>\\)\\|"
     (regexp-opt '("{" "(" "[" "]" ")" "}") t))))

(defconst terra-block-token-alist
  '(("do"       "\\<end\\>"   "\\<for\\|while\\>"                       middle-or-open)
    ("function" "\\<end\\>"   nil                                       open)
    ("terra"    "\\<end\\>"   nil                                       open)
    ("quote"    "\\<end\\>"   nil                                       open)
    ("repeat"   "\\<until\\>" nil                                       open)
    ("then"     "\\<\\(e\\(lse\\(if\\)?\\|nd\\)\\)\\>" "\\<\\(else\\)?if\\>" middle)
    ("{"        "}"           nil                                       open)
    ("["        "]"           nil                                       open)
    ("("        ")"           nil                                       open)
    ("if"       "\\<then\\>"  nil                                       open)
    ("for"      "\\<do\\>"    nil                                       open)
    ("while"    "\\<do\\>"    nil                                       open)
    ("else"     "\\<end\\>"   "\\<then\\>"                              middle)
    ("elseif"   "\\<then\\>"  "\\<then\\>"                              middle)
    ("end"      nil           "\\<\\(do\\|function\\|terra\\|then\\|else\\)\\>" close)
    ("until"    nil           "\\<repeat\\>"                            close)
    ("}"        nil           "{"                                       close)
    ("]"        nil           "\\["                                     close)
    (")"        nil           "("                                       close))
  "This is a list of block token information blocks.
Each token information entry is of the form:
  KEYWORD FORWARD-MATCH-REGEXP BACKWARDS-MATCH-REGEXP TOKEN-TYPE
KEYWORD is the token.
FORWARD-MATCH-REGEXP is a regexp that matches all possble tokens when going forward.
BACKWARDS-MATCH-REGEXP is a regexp that matches all possble tokens when going backwards.
TOKEN-TYPE determines where the token occurs on a statement. open indicates that the token appears at start, close indicates that it appears at end, middle indicates that it is a middle type token, and middle-or-open indicates that it can appear both as a middle or an open type.")

(defconst terra-indentation-modifier-regexp
  ;; The absence of else is deliberate, since it does not modify the
  ;; indentation level per se. It only may cause the line, in which the
  ;; else is, to be shifted to the left.
  (concat
   "\\(\\_<"
   (regexp-opt '("do" "function" "terra" "quote" "repeat" "then" "if" "else" "elseif" "for" "while") t)
   "\\_>\\|"
   (regexp-opt '("{" "(" "["))
   "\\)\\|\\(\\_<"
   (regexp-opt '("end" "until") t)
   "\\_>\\|"
   (regexp-opt '("]" ")" "}"))
   "\\)")
  )

(defun terra-get-block-token-info (token)
  "Returns the block token info entry for TOKEN from terra-block-token-alist"
  (assoc token terra-block-token-alist))

(defun terra-get-token-match-re (token-info direction)
  "Returns the relevant match regexp from token info"
  (cond
   ((eq direction 'forward) (cadr token-info))
   ((eq direction 'backward) (caddr token-info))
   (t nil)))

(defun terra-get-token-type (token-info)
  "Returns the relevant match regexp from token info"
   (cadddr token-info))

(defun terra-backwards-to-block-begin-or-end ()
  "Move backwards to nearest block begin or end.  Returns nil if not successful."
  (interactive)
  (terra-find-regexp 'backward terra-block-regexp))

(defun terra-find-matching-token-word (token search-start &optional direction)
  (let* ((token-info (terra-get-block-token-info token))
         (match-type (terra-get-token-type token-info))
         ;; If we are on a middle token, go backwards. If it is a middle or open,
         ;; go forwards
         (search-direction (or direction
                               (if (or (eq match-type 'open)
                                       (eq match-type 'middle-or-open))
                                   'forward
                                 'backward)
                               'backward))
         (match (terra-get-token-match-re token-info search-direction))
         maybe-found-pos)
    ;; if we are searching forward from the token at the current point
    ;; (i.e. for a closing token), need to step one character forward
    ;; first, or the regexp will match the opening token.
    (if (eq search-direction 'forward) (forward-char 1))
    (if search-start (goto-char search-start))
    (catch 'found
      ;; If we are attempting to find a matching token for a terminating token
      ;; (i.e. a token that starts a statement when searching back, or a token
      ;; that ends a statement when searching forward), then we don't need to look
      ;; any further.
      (if (or (and (eq search-direction 'forward)
                   (eq match-type 'close))
              (and (eq search-direction 'backward)
                   (eq match-type 'open)))
          (throw 'found nil))
      (while (terra-find-regexp search-direction terra-indentation-modifier-regexp)
        ;; have we found a valid matching token?
        (let ((found-token (match-string 0))
              (found-pos (match-beginning 0)))
          (let ((found-type (terra-get-token-type
                             (terra-get-block-token-info found-token))))
            (if (not (and match (string-match match found-token)))
                ;; no - then there is a nested block. If we were looking for
                ;; a block begin token, found-token must be a block end
                ;; token; likewise, if we were looking for a block end token,
                ;; found-token must be a block begin token, otherwise there
                ;; is a grammatical error in the code.
                (if (not (and
                          (or (eq match-type 'middle)
                              (eq found-type 'middle)
                              (eq match-type 'middle-or-open)
                              (eq found-type 'middle-or-open)
                              (eq match-type found-type))
                          (terra-find-matching-token-word found-token nil
                                                        search-direction)))
                    (when maybe-found-pos
                      (goto-char maybe-found-pos)
                      (throw 'found maybe-found-pos)))
              ;; yes.
              ;; if it is a not a middle kind, report the location
              (when (not (or (eq found-type 'middle)
                             (eq found-type 'middle-or-open)))
                (throw 'found found-pos))
              ;; if it is a middle-or-open type, record location, but keep searching.
              ;; If we fail to complete the search, we'll report the location
              (when (eq found-type 'middle-or-open)
                (setq maybe-found-pos found-pos))
              ;; Cannot use tail recursion. too much nesting on long chains of
              ;; if/elseif. Will reset variables instead.
              (setq token found-token)
              (setq token-info (terra-get-block-token-info token))
              (setq match (terra-get-token-match-re token-info search-direction))
              (setq match-type (terra-get-token-type token-info))))))
      maybe-found-pos)))

(defun terra-goto-matching-block-token (&optional search-start parse-start direction)
  "Find block begion/end token matching the one at the point.
This function moves the point to the token that matches the one
at the current point. Returns the point position of the first character of
the matching token if successful, nil otherwise."
  (if parse-start (goto-char parse-start))
  (let ((case-fold-search nil))
    (if (looking-at terra-indentation-modifier-regexp)
        (let ((position (terra-find-matching-token-word (match-string 0)
                                                      search-start direction)))
          (and position
               (goto-char position))))))

(defun terra-goto-matching-block (&optional noreport)
  "Go to the keyword balancing the one under the point.
If the point is on a keyword/brace that starts a block, go to the
matching keyword that ends the block, and vice versa."
  (interactive)
  ;; search backward to the beginning of the keyword if necessary
  (if (eq (char-syntax (following-char)) ?w)
      (re-search-backward "\\_<" nil t))
  (let ((position (terra-goto-matching-block-token)))
    (if (and (not position)
             (not noreport))
        (error "Not on a block control keyword or brace")
      position)))

(defun terra-forward-line-skip-blanks (&optional back)
  "Move 1 line forward (back if BACK is non-nil) skipping blank lines.

Moves point 1 line forward (or backward) skipping lines that contain
no Terra code besides comments. The point is put to the beginning of
the line.

Returns final value of point as integer or nil if operation failed."
  (catch 'found
    (while t
      (unless (eql (forward-line (if back -1 1)) 0)    ;; 0 means success
        (throw 'found nil))
      (unless (looking-at "\\s *\\(--.*\\)?$")       ;; blank terra line
        (throw 'found (point))))))

(eval-when-compile
  (defconst terra-operator-class
    "-+*/^.=<>~"))

(defconst terra-cont-eol-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("and" "or" "not" "in" "for" "while"
                   "local" "function" "terra" "if" "until" "elseif" "return") t)
     "\\_>\\|"
     "\\(^\\|[^" terra-operator-class "]\\)"
     (regexp-opt '("+" "-" "*" "/" "^" ".." "==" "=" "<" ">" "<=" ">=" "~=") t)
     "\\)"
     "\\s *\\=")
    )
  "Regexp that matches the ending of a line that needs continuation

This regexp starts from eol and looks for a binary operator or an unclosed
block intro (i.e. 'for' without 'do' or 'if' without 'then') followed by
an optional whitespace till the end of the line.")

(defconst terra-cont-bol-regexp
  (eval-when-compile
    (concat
     "\\=\\s *"
     "\\(\\_<"
     (regexp-opt '("and" "or" "not") t)
     "\\_>\\|"
     (regexp-opt '("+" "-" "*" "/" "^" ".." "==" "=" "<" ">" "<=" ">=" "~=") t)
     "\\($\\|[^" terra-operator-class "]\\)"
     "\\)")

    )
  "Regexp that matches a line that continues previous one

This regexp means, starting from point there is an optional whitespace followed
by Terra binary operator. Terra is very liberal when it comes to continuation line,
so we're safe to assume that every line that starts with a binop continues
previous one even though it looked like an end-of-statement.")

(defun terra-last-token-continues-p ()
  "Returns true if the last token on this line is a continuation token."
  (let ((line-begin (line-beginning-position))
        (line-end (line-end-position)))
    (save-excursion
      (end-of-line)
      ;; we need to check whether the line ends in a comment and
      ;; skip that one.
      (while (terra-find-regexp 'backward "-" line-begin 'terra-string-p)
        (if (looking-at "--")
            (setq line-end (point))))
      (goto-char line-end)
      (re-search-backward terra-cont-eol-regexp line-begin t))))

(defun terra-first-token-continues-p ()
  "Returns true if the first token on this line is a continuation token."
  (let ((line-end (line-end-position)))
    (save-excursion
      (beginning-of-line)
      ;; if first character of the line is inside string, it's a continuation
      ;; if strings aren't supposed to be indented, `terra-calculate-indentation' won't even let
      ;; the control inside this function
      (re-search-forward terra-cont-bol-regexp line-end t))))

(defun terra-is-continuing-statement-p (&optional parse-start)
  "Return non-nil if the line continues a statement.
More specifically, return the point in the line that is continued.
The criteria for a continuing statement are:

* the last token of the previous line is a continuing op,
  OR the first token of the current line is a continuing op

"
  (let ((prev-line nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      (save-excursion (setq prev-line (terra-forward-line-skip-blanks 'back)))
      (and prev-line
           (or (terra-first-token-continues-p)
               (and (goto-char prev-line)
                    ;; check last token of previous nonblank line
                    (terra-last-token-continues-p)))))))

(defun terra-make-indentation-info-pair (found-token found-pos)
  "This is a helper function to terra-calculate-indentation-info. Don't
use standalone."
  (cond
   ;; function is a bit tricky to indent right. They can appear in a lot ot
   ;; different contexts. Until I find a shortcut, I'll leave it with a simple
   ;; relative indentation.
   ;; The special cases are for indenting according to the location of the
   ;; function. i.e.:
   ;;       (cons 'absolute (+ (current-column) terra-indent-level))
   ;; TODO: Fix this. It causes really ugly indentations for in-line functions.
   ((or (string-equal found-token "function") (string-equal found-token "terra"))
    (cons 'relative terra-indent-level))

   ;; block openers
   ((member found-token (list "{" "(" "["))
	 (save-excursion
	   ;; expression follows -> indent at start of next expression
       ;; Last token on the line -> simple relative indent
	   (if (and (not (search-forward-regexp "[[:space:]]--" (line-end-position) t))
                (search-forward-regexp "[^[:space:]]" (line-end-position) t))
           (cons 'absolute (1- (current-column)))
         (cons 'relative terra-indent-level))))

   ;; These are not really block starters. They should not add to indentation.
   ;; The corresponding "then" and "do" handle the indentation.
   ((member found-token (list "if" "for" "while"))
    (cons 'relative 0))
   ;; closing tokens follow: These are usually taken care of by
   ;; terra-calculate-indentation-override.
   ;; elseif is a bit of a hack. It is not handled separately, but it needs to
   ;; nullify a previous then if on the same line.
   ((member found-token (list "until" "elseif"))
    (save-excursion
      (let ((line (line-number-at-pos)))
        (if (and (terra-goto-matching-block-token nil found-pos 'backward)
                 (= line (line-number-at-pos)))
            (cons 'remove-matching 0)
          (cons 'relative 0)))))

   ;; else is a special case; if its matching block token is on the same line,
   ;; instead of removing the matching token, it has to replace it, so that
   ;; either the next line will be indented correctly, or the end on the same
   ;; line will remove the effect of the else.
   ((string-equal found-token "else")
     (save-excursion
       (let ((line (line-number-at-pos)))
         (if (and (terra-goto-matching-block-token nil found-pos 'backward)
                  (= line (line-number-at-pos)))
             (cons 'replace-matching (cons 'relative terra-indent-level))
                   (cons 'relative terra-indent-level)))))

   ;; Block closers. If they are on the same line as their openers, they simply
   ;; eat up the matching indentation modifier. Otherwise, they pull
   ;; indentation back to the matching block opener.
   ((member found-token (list ")" "}" "]" "end"))
    (save-excursion
      (let ((line (line-number-at-pos)))
        (terra-goto-matching-block-token nil found-pos 'backward)
        (if (/= line (line-number-at-pos))
            (cons 'absolute
                  (+ (current-indentation)
                     (terra-calculate-indentation-block-modifier
                      nil (point))))
          (cons 'remove-matching 0)))))

   ;; Everything else. This is from the original code: If opening a block
   ;; (match-data 1 exists), then push indentation one level up, if it is
   ;; closing a block, pull it one level down.
   ('other-indentation-modifier
    (cons 'relative (if (nth 2 (match-data))
                        ;; beginning of a block matched
                        terra-indent-level
                      ;; end of a block matched
                      (- terra-indent-level))))))

(defun terra-cleanup-indentation-info (info)
  "Cleanup the list of indentation information.
There are two tokens that cause list cleanup: remove-matching,
and replace matching. These tokens are considered cleanup tokens.

When a remove-matching token is found, the next non cleanup token
is removed from list.

When a replace-matching token is found, the next non-cleanup
token is removed from the list, and the cdr of the
replace-matching token is inserted in its place."
  (let (value
        (erase-count 0))
    (dolist (elt info value)
      (cond
       ( (eq 'remove-matching (car elt))
         (setq erase-count (1+ erase-count)))
       ( (eq 'replace-matching (car elt))
         (setq value (cons (cdr elt) value))
         (setq erase-count (1+ erase-count)))
       ( t
         (if (= erase-count 0)
             (setq value (cons elt value))
           (setq erase-count (1- erase-count))))))
    (reverse value)))

(defun terra-calculate-indentation-info (&optional parse-start parse-end)
  "For each block token on the line, computes how it affects the indentation.
The effect of each token can be either a shift relative to the current
indentation level, or indentation to some absolute column. This information
is collected in a list of indentation info pairs, which denote absolute
and relative each, and the shift/column to indent to."
  (let ((combined-line-end (line-end-position))
        (start-indentation (current-indentation)))
    (save-excursion
      (while (terra-last-token-continues-p)
        (terra-forward-line-skip-blanks)
        (setq combined-line-end (line-end-position))))
    (let ((search-stop (if parse-end
                           (min parse-end combined-line-end)
                         combined-line-end))
          (indentation-info nil))
      (if parse-start (goto-char parse-start))
      (save-excursion
        (beginning-of-line)
        (while (terra-find-regexp 'forward terra-indentation-modifier-regexp
                                search-stop)
          (let ((found-token (match-string 0))
                (found-pos (match-beginning 0))
                (found-end (match-end 0))
                (data (match-data)))
            (setq indentation-info
                  (cons (terra-make-indentation-info-pair found-token found-pos) indentation-info))))

        (or (and indentation-info (terra-cleanup-indentation-info indentation-info))
            (list (cons 'absolute start-indentation)))))))

(defun terra-accumulate-indentation-info (info)
  "Accumulates the indentation information previously calculated by
terra-calculate-indentation-info. Returns either the relative indentation
shift, or the absolute column to indent to."
  (let ((info-list (reverse info))
        (type 'relative)
        (accu 0))
    (mapc (lambda (x)
            (setq accu (if (eq 'absolute (car x))
                           (progn (setq type 'absolute)
                                  (cdr x))
                         (+ accu (cdr x)))))
          info-list)
    (cons type accu)))

(defun terra-calculate-indentation-block-modifier (&optional parse-start
                                                           parse-end)
  "Return amount by which this line modifies the indentation.
Beginnings of blocks add terra-indent-level once each, and endings
of blocks subtract terra-indent-level once each. This function is used
to determine how the indentation of the following line relates to this
one."
  (if parse-start (goto-char parse-start))
  ;; First go back to the line that starts it all
  ;; terra-calculate-indentation-info will scan through the whole thing
  (while (terra-is-continuing-statement-p)
    (terra-forward-line-skip-blanks 'back))
  (let ((case-fold-search nil)
        (indentation-info (terra-accumulate-indentation-info
                           (terra-calculate-indentation-info nil parse-end))))
    (if (eq (car indentation-info) 'absolute)
        (- (cdr indentation-info) (current-indentation))
      (cdr indentation-info))))

(defun terra-point-is-after-left-shifter-p ()
  "Check if point is at a left-shifter.
A left-shifter is a partial terra expression which should be ignored for line up purposes when closing a block. An example of this is:
   local a = function()
      ....
   end
   ^         ^
   |         +- not here
   +- Close here"
  (save-excursion
    (let ((old-point (point)))
      (back-to-indentation)
      (and
       (or (looking-at "local\\s +\\(?:\\(?:\\sw\\|\\s_\\)+\\s *\\(,\\s *\\(?:\\sw\\|\\s_\\)+\\s *\\)*=\\s *\\)?")
           ;; This is too generic, and will screw up a lot of indentations. Will need
           ;; a better regexp for assignments
           (looking-at "[^=]*=\\s *"))
       (= old-point (match-end 0))))))

(defun terra-calculate-indentation-override (&optional parse-start)
  "Return overriding indentation amount for special cases.
Look for an uninterrupted sequence of block-closing tokens that starts
at the beginning of the line. For each of these tokens, shift indentation
to the left by the amount specified in terra-indent-level."
  (let ((indentation-modifier 0)
        (case-fold-search nil)
        (block-token nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      ;; Look for the last block closing token
      (back-to-indentation)
      (if (and (not (terra-comment-or-string-p))
               (looking-at terra-indentation-modifier-regexp)
               (let ((token-info (terra-get-block-token-info (match-string 0))))
                 (and token-info
                      (not (eq 'open (terra-get-token-type token-info))))))
          (when (terra-goto-matching-block-token nil nil 'backward)
            ;; Exception cases: when the start of the line is an assignment,
            ;; go to the start of the assignment instead of the matching item
            (let ((block-start-column (current-column))
                  (block-start-point (point)))
              (if (terra-point-is-after-left-shifter-p)
                  (current-indentation)
                block-start-column)))))))

(defun terra-calculate-indentation (&optional parse-start)
  "Return appropriate indentation for current line as Terra code."
  (save-excursion
    (let ((continuing-p (terra-is-continuing-statement-p)))
      (or
       ;; when calculating indentation, do the following:
       ;; 1. check, if the line starts with indentation-modifier (open/close brace)
       ;;    and if it should be indented/unindented in special way
       (terra-calculate-indentation-override)

       ;; 2. otherwise, use indentation modifiers from previous line + it's own indentation
       ;; 3. if previous line doesn't contain indentation modifiers, additionally check
       ;;    if current line is a continuation line and add terra-indent-level if it is
       (when (terra-forward-line-skip-blanks 'back)
         ;; the order of function calls here is important. block modifier
         ;; call may change the point to another line
         (let ((modifier
                (terra-calculate-indentation-block-modifier nil (line-end-position))))
           (+ (if (and continuing-p (= 0 modifier))
                  terra-indent-level
                modifier)
              (current-indentation))))

       ;; 4. if there's no previous line, indentation is 0
       0))))

(defun terra-beginning-of-proc (&optional arg)
  "Move backward to the beginning of a terra proc (or similar).
With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of proc.
Returns t unless search stops due to beginning or end of buffer."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
        (ret t))
    (while (< arg 0)
      (if (re-search-forward "^\\(function\\|terra\\)[ \t]" nil t)
          (setq arg (1+ arg)
                found t)
        (setq ret nil
              arg 0)))
    (if found
        (beginning-of-line))
    (if (> arg 0)
        (if (re-search-forward "^\\(function\\|terra\\)[ \t]" nil t)
            (setq arg (1+ arg))
          (goto-char (point-max))))
    (while (> arg 0)
      (if (re-search-backward "^\\(function\\|terra\\)[ \t]" nil t)
          (setq arg (1- arg))
        (setq ret nil
              arg 0)))
    ret))

(defun terra-end-of-proc (&optional arg)
  "Move forward to next end of terra proc (or similar).
With argument, do it that many times.  Negative argument -N means move
back to Nth preceding end of proc.

This function just searches for a `end' at the beginning of a line."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
        (ret t))
    (if (and (< arg 0)
             (not (bolp))
             (save-excursion
               (beginning-of-line)
               (eq (following-char) ?})))
        (forward-char -1))
    (while (> arg 0)
      (if (re-search-forward "^end" nil t)
          (setq arg (1- arg)
                found t)
        (setq ret nil
              arg 0)))
    (while (< arg 0)
      (if (re-search-backward "^end" nil t)
          (setq arg (1+ arg)
                found t)
        (setq ret nil
              arg 0)))
    (if found
        (progn
          (beginning-of-line)
          (forward-line)))
    ret))

(defun terra-start-process (&optional name program startfile &rest switches)
  "Start a terra process named NAME, running PROGRAM.
PROGRAM defaults to NAME, which defaults to `terra-default-application'.
When called interactively, switch to the process buffer."
  (interactive)
  (or switches
      (setq switches terra-default-command-switches))
  (setq name (or name terra-default-application))
  (setq program (or program name))
  (setq terra-process-buffer (apply 'make-comint name program startfile switches))
  (setq terra-process (get-buffer-process terra-process-buffer))
  ;; wait for prompt
  (with-current-buffer terra-process-buffer
    (while (not (terra-prompt-line))
      (accept-process-output (get-buffer-process (current-buffer)))
      (goto-char (point-max))))
  ;; when called interactively, switch to process buffer
  (if (called-interactively-p 'any)
      (switch-to-buffer terra-process-buffer)))

(defun terra-kill-process ()
  "Kill terra subprocess and its buffer."
  (interactive)
  (if terra-process-buffer
      (kill-buffer terra-process-buffer)))

(defun terra-set-terra-region-start (&optional arg)
  "Set start of region for use with `terra-send-terra-region'."
  (interactive)
  (set-marker terra-region-start (or arg (point))))

(defun terra-set-terra-region-end (&optional arg)
  "Set end of region for use with `terra-send-terra-region'."
  (interactive)
  (set-marker terra-region-end (or arg (point))))

(defun terra-send-current-line ()
  "Send current line to terra subprocess, found in `terra-process'.
If `terra-process' is nil or dead, start a new process first."
  (interactive)
  (terra-send-region (line-beginning-position) (line-end-position)))

(defun terra-send-region (start end)
  "Send region to terra subprocess."
  (interactive "r")
  ;; make temporary terra file
  (let ((tempfile (terra-make-temp-file "terra-"))
        (last-prompt nil)
        (prompt-found nil)
        (terra-stdin-line-offset (count-lines (point-min) start))
        (terra-stdin-buffer (current-buffer))
        current-prompt )
    (write-region start end tempfile)
    (or (and terra-process
             (comint-check-proc terra-process-buffer))
        (terra-start-process terra-default-application))
    ;; kill terra process without query
    (if (fboundp 'process-kill-without-query)
        (process-kill-without-query terra-process))
    ;; send dofile(tempfile)
    (with-current-buffer terra-process-buffer
      (goto-char (point-max))
      (setq last-prompt (point-max))
      (comint-simple-send (get-buffer-process (current-buffer))
                          (format "dofile(\"%s\")"
                                  (replace-regexp-in-string "\\\\" "\\\\\\\\" tempfile)))
      ;; wait for prompt
      (while (not prompt-found)
        (accept-process-output (get-buffer-process (current-buffer)))
        (goto-char (point-max))
        (setq prompt-found (and (terra-prompt-line) (< last-prompt (point-max)))))
      ;; remove temp. terra file
      (delete-file tempfile)
      (terra-postprocess-output-buffer terra-process-buffer last-prompt terra-stdin-line-offset)
      (if terra-always-show
          (display-buffer terra-process-buffer)))))

(defun terra-postprocess-output-buffer (buf start &optional terra-stdin-line-offset)
  "Highlight tracebacks found in buf. If an traceback occurred return
t, otherwise return nil.  BUF must exist."
  (let ((terra-stdin-line-offset (or terra-stdin-line-offset 0))
        line file bol err-p)
    (with-current-buffer buf
      (goto-char start)
      (beginning-of-line)
      (if (re-search-forward terra-traceback-line-re nil t)
          (setq file (match-string 1)
                line (string-to-number (match-string 2)))))
    (when (and terra-jump-on-traceback line)
      (beep)
      ;; FIXME: highlight
      (terra-jump-to-traceback file line terra-stdin-line-offset)
      (setq err-p t))
    err-p))

(defun terra-jump-to-traceback (file line terra-stdin-line-offset)
  "Jump to the Terra code in FILE at LINE."
  ;; sanity check: temporary-file-directory
  (if (string= (substring file 0 3)  "...")
      (message "Lua traceback output truncated: customize 'temporary-file-directory' or increase 'LUA_IDSIZE' in 'luaconf.h'.")
    (let ((buffer (cond ((or (string-equal file tempfile) (string-equal file "stdin"))
                         (setq line (+ line terra-stdin-line-offset))
                         terra-stdin-buffer)
                        (t (find-file-noselect file)))))
      (pop-to-buffer buffer)
      ;; Force Terra mode
      (if (not (eq major-mode 'terra-mode))
          (terra-mode))
      ;; FIXME: fix offset when executing region
      (goto-line line)
      (message "Jumping to error in file %s on line %d" file line))))

(defun terra-prompt-line ()
  (save-excursion
    (save-match-data
      (forward-line 0)
      (if (looking-at comint-prompt-regexp)
          (match-end 0)))))

(defun terra-send-terra-region ()
  "Send preset terra region to terra subprocess."
  (interactive)
  (or (and terra-region-start terra-region-end)
      (error "terra-region not set"))
  (or (and terra-process
           (comint-check-proc terra-process-buffer))
      (terra-start-process terra-default-application))
  (comint-simple-send terra-process
                      (buffer-substring terra-region-start terra-region-end)
                      )
  (if terra-always-show
      (display-buffer terra-process-buffer)))

(defun terra-send-proc ()
  "Send proc around point to terra subprocess."
  (interactive)
  (let (beg end)
    (save-excursion
      (terra-beginning-of-proc)
      (setq beg (point))
      (terra-end-of-proc)
      (setq end (point)))
    (or (and terra-process
             (comint-check-proc terra-process-buffer))
        (terra-start-process terra-default-application))
    (comint-simple-send terra-process
                        (buffer-substring beg end))
    (if terra-always-show
        (display-buffer terra-process-buffer))))

;; FIXME: This needs work... -Bret
(defun terra-send-buffer ()
  "Send whole buffer to terra subprocess."
  (interactive)
  (terra-send-region (point-min) (point-max)))

(defun terra-restart-with-whole-file ()
  "Restart terra subprocess and send whole file as input."
  (interactive)
  (terra-kill-process)
  (terra-start-process terra-default-application)
  (terra-send-buffer))

(defun terra-show-process-buffer ()
  "Make sure `terra-process-buffer' is being displayed."
  (interactive)
  (display-buffer terra-process-buffer))

(defun terra-hide-process-buffer ()
  "Delete all windows that display `terra-process-buffer'."
  (interactive)
  (delete-windows-on terra-process-buffer))

(defun terra-search-documentation ()
  "Search Terra documentation for the word at the point."
  (interactive)
  (browse-url (concat terra-search-url-prefix (current-word t))))

(defun terra-toggle-electric-state (&optional arg)
  "Toggle the electric indentation feature.
Optional numeric ARG, if supplied, turns on electric indentation when
positive, turns it off when negative, and just toggles it when zero or
left out."
  (interactive "P")
  (let ((num_arg (prefix-numeric-value arg)))
    (setq terra-electric-flag (cond ((or (null arg)
                                       (zerop num_arg)) (not terra-electric-flag))
                                  ((< num_arg 0) nil)
                                  ((> num_arg 0) t))))
  (message "%S" terra-electric-flag))

(defun terra-forward-sexp (&optional count)
  "Forward to block end"
  (interactive "p")
  (save-match-data
    (let* ((count (or count 1))
           (block-start (mapcar 'car terra-sexp-alist))
           (block-end (mapcar 'cdr terra-sexp-alist))
           (block-regex (regexp-opt (append  block-start block-end) 'words))
           current-exp
           )
      (while (> count 0)
        ;; skip whitespace
        (skip-chars-forward " \t\n")
        (if (looking-at (regexp-opt block-start 'words))
            (let ((keyword (match-string 1)))
              (terra-find-matching-token-word keyword nil))
          ;; If the current keyword is not a "begin" keyword, then just
          ;; perform the normal forward-sexp.
          (forward-sexp 1))
        (setq count (1- count))))))


;; menu bar

(define-key terra-mode-menu [restart-with-whole-file]
  '("Restart With Whole File" .  terra-restart-with-whole-file))
(define-key terra-mode-menu [kill-process]
  '("Kill Process" . terra-kill-process))

(define-key terra-mode-menu [hide-process-buffer]
  '("Hide Process Buffer" . terra-hide-process-buffer))
(define-key terra-mode-menu [show-process-buffer]
  '("Show Process Buffer" . terra-show-process-buffer))

(define-key terra-mode-menu [end-of-proc]
  '("End Of Proc" . terra-end-of-proc))
(define-key terra-mode-menu [beginning-of-proc]
  '("Beginning Of Proc" . terra-beginning-of-proc))

(define-key terra-mode-menu [send-terra-region]
  '("Send Terra-Region" . terra-send-terra-region))
(define-key terra-mode-menu [set-terra-region-end]
  '("Set Terra-Region End" . terra-set-terra-region-end))
(define-key terra-mode-menu [set-terra-region-start]
  '("Set Terra-Region Start" . terra-set-terra-region-start))

(define-key terra-mode-menu [send-current-line]
  '("Send Current Line" . terra-send-current-line))
(define-key terra-mode-menu [send-region]
  '("Send Region" . terra-send-region))
(define-key terra-mode-menu [send-proc]
  '("Send Proc" . terra-send-proc))
(define-key terra-mode-menu [send-buffer]
  '("Send Buffer" . terra-send-buffer))
(define-key terra-mode-menu [search-documentation]
  '("Search Documentation" . terra-search-documentation))

(defsubst terra-put-char-property (pos property value &optional object)
  (if value
      (put-text-property pos (1+ pos) property value object)
    (remove-text-properties pos (1+ pos) (list property nil))))

(defsubst terra-put-char-syntax-table (pos value &optional object)
  (terra-put-char-property pos 'syntax-table value object))

(defsubst terra-get-multiline-delim-syntax (type)
  (cond ((eq type 'string) '(15))
        ((eq type 'comment) '(14))
        (nil)))

(defun terra-mark-char-multiline-delim (pos type)
  "Mark character as a delimiter of Terra multiline construct

If TYPE is string, mark char  as string delimiter. If TYPE is comment,
mark char as comment delimiter.  Otherwise, remove the mark if any."
  (let ((old-modified-p (buffer-modified-p)) (inhibit-modification-hooks t))
    (unwind-protect
        (terra-put-char-syntax-table pos (terra-get-multiline-delim-syntax type))
      (set-buffer-modified-p old-modified-p))))

(defsubst terra-inside-multiline-p (&optional pos)
  (let ((status (syntax-ppss pos)))
    (or (eq (elt status 3) t)                ;; inside generic string
        (eq (elt status 7) 'syntax-table)))) ;; inside generic comment

(defun terra-get-multiline-start (&optional pos)
  (interactive)
  (when (terra-inside-multiline-p pos) ;; return string/comment start
    (elt (syntax-ppss pos) 8)))

(defun terra-unmark-multiline-literals (&optional begin end)
  "Clears all Terra multiline construct markers in region

If BEGIN is nil, start from `beginning-of-buffer'.
If END is nil, stop at `end-of-buffer'."
  (interactive)
  (let ((old-modified-p (buffer-modified-p)) (inhibit-modification-hooks t))
    (unwind-protect
        (remove-text-properties (or begin (point-min)) (or end (point-max)) '(syntax-table ()))
      (set-buffer-modified-p old-modified-p)))
  (font-lock-fontify-buffer))

(defun terra-mark-multiline-region (begin end)
  (let ((type (if (eq ?- (char-after begin)) 'comment 'string)))
  (terra-mark-char-multiline-delim begin type)
  (when end
    (terra-mark-char-multiline-delim (1- end) type))))

(defun terra-mark-all-multiline-literals (&optional begin end)
  "Marks all Terra multiline constructs in region

If BEGIN is nil, start from `beginning-of-buffer'.
If END is nil, stop at `end-of-buffer'."
  (interactive)

  (if (and (called-interactively-p 'any) (use-region-p))
      (setq begin (region-beginning)
            end (region-end)))

  (terra-unmark-multiline-literals begin end)
  (save-excursion
    (goto-char (or begin (point-min)))

    (while (and
            ;; must check  for point range,  because matching previous
            ;; multiline  end might  move  point beyond  end and  this
            ;; drives `re-search-forward' crazy
            (if end (< (point) end) t)
            ;; look for
            ;; 1. (optional) two or more dashes followed by
            ;; 2. terra multiline delimiter [[
            (re-search-forward "\\(?2:--\\)?\\[\\(?1:=*\\)\\[" end 'noerror))
      ;; match-start + 1 is considered instead of match-start, because
      ;; such  approach  handles  '---[[' situation  correctly:  Emacs
      ;; thinks 2nd dash (i.e.  match-start) is not yet a comment, but
      ;; the third one is, hence the +1.  In all the other situations,
      ;; '+1'  is safe  to use  because  it bears  the same  syntactic
      ;; properties, i.e.  if match-start is inside string-or-comment,
      ;; then '+1' is too and vice versa.
      ;;
      ;; PS. ping me if you find a situation in which this is not true
      (unless (terra-comment-or-string-p (1+ (match-beginning 0)))
        (let (ml-begin ml-end)
          (setq ml-begin (match-beginning 0))
          (when (re-search-forward (format "\\]%s\\]" (or (match-string 1) "")) nil 'noerror)
            ;; (message "found match %s" (match-string 0))
            (setq ml-end (match-end 0)))
          (terra-mark-multiline-region ml-begin ml-end))))))

(defvar terra-automark-multiline-timer nil
  "Contains idle-timer object used for automatical multiline literal markup which must be cleaned up on exit.")
(make-variable-buffer-local 'terra-automark-multiline-timer)

(defvar terra-automark-multiline-start-pos nil
  "Contains position from which automark procedure should start.

Automarking shall start at the point before which no modification has been
made since last automark. Additionally, if such point is inside string or
comment, rewind start position to its beginning.

nil means automark is unnecessary because there were no updates.")
(make-variable-buffer-local 'terra-automark-multiline-start-pos)

(defun terra--automark-update-start-pos (change-begin change-end old-len)
  "Updates `terra-automark-multiline-start-pos' upon buffer modification."
  (save-excursion
    (goto-char change-begin)
    (beginning-of-line)
    (setq terra-automark-multiline-start-pos
          (or (terra-comment-or-string-start) (point)))))

(defun terra--automark-multiline-update-timer ()
  (terra--automark-multiline-cleanup)  ;; reset previous timer if it existed
  (when terra-automark-multiline-interval
    (add-hook 'change-major-mode-hook 'terra--automark-multiline-cleanup nil 'local)
    (add-hook 'after-change-functions 'terra--automark-update-start-pos  nil 'local)
    (setq terra-automark-multiline-timer
          (run-with-idle-timer terra-automark-multiline-interval 'repeat
                               'terra--automark-multiline-run))))

(defun terra--automark-multiline-cleanup ()
  "Disable automatical multiline construct marking"
  (unless (null terra-automark-multiline-timer)
    (cancel-timer terra-automark-multiline-timer)
    (setq terra-automark-multiline-timer nil)))

(defun terra--automark-multiline-run ()
  (when (<= (buffer-size) terra-automark-multiline-maxsize)
    (when terra-automark-multiline-start-pos
      (terra-mark-all-multiline-literals terra-automark-multiline-start-pos)
      (setq terra-automark-multiline-start-pos nil))))

(defun terra--customize-set-automark-multiline-interval (symbol value)
  (set symbol value)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'terra-mode)
        (terra--automark-multiline-update-timer)))))

(defcustom terra-automark-multiline-interval 1
  "If not 0, specifies idle time in seconds after which terra-mode will mark multiline literals."
  :group 'terra
  :type 'integer
  :set 'terra--customize-set-automark-multiline-interval)

(defcustom terra-automark-multiline-maxsize 100000
  "Maximum buffer size for which terra-mode will mark multiline literals automatically."
  :group 'terra
  :type 'integer)

(provide 'terra-mode)

;;; terra-mode.el ends here
