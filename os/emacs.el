; Copyright (c) 2012-2019 Doug Rogers under the Zero Clause BSD License.
; You are free to do whatever you want with this software. See LICENSE.txt.

(setq inhibit-splash-screen t)  ;; OMG that thing is annoying.

(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

; Run this once on a new system.
(defun dr-install-packages ()
  "Install packages from MELPA, etc."
  (interactive)
  (package-refresh-contents)
  (package-install 'fzf)                ; https://github.com/bling/fzf.el
  (package-install 'request))

(setq font-lock-maximum-decoration t)
;(global-font-lock-mode 1 t)
(global-font-lock-mode t)
;; "blue" works well with a white background, but I'm using black above, so the
;; default ("yellow") has a better look:
; (set-face-foreground 'font-lock-variable-name-face "blue")
(column-number-mode t)

(dr-load "chatgpt.el")
(dr-load "google-c-style.el")
(dr-load "mandiant-c-style.el")
(dr-load "rust-mode.el")
(dr-load "clojure-mode.el")
(if (not (fboundp 'lua-mode))
    (progn
      (dr-load "lua-mode.el")
      (setq auto-mode-alist (cons '("\\.lua" . lua-mode) auto-mode-alist))))
(if (not (fboundp 'terra-mode))
    (progn
      (dr-load "terra-mode.el")
      (setq auto-mode-alist (cons '("\\.\\(t\\|terra\\)$" . terra-mode) auto-mode-alist))))

; (dr-load "armasm-mode.el")
; (dr-load "smart-tabs-mode.el")

; For now while I'm using Prolog and not Perl.
(setq auto-mode-alist (cons '("\.pl$" . prolog-mode)    auto-mode-alist))

; (setq auto-mode-alist (cons '("\\.S"                   . armasm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.\\(spec\\|body\\).a$" . ada-mode)    auto-mode-alist))

(setq auto-mode-alist (cons '("\.\\(4th\\|forth\\)$"   . forth-mode)  auto-mode-alist))
(setq auto-mode-alist (cons '("\\.max"                 . maxima-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.sch\\(\\|eme\\)$"     . scheme-mode) auto-mode-alist))

(setq load-path (cons (concat (dr-home) "/dr/elisp") load-path))

; (setq load-path (cons  "/usr/local/share/maxima/5.9.0/emacs" load-path ))
; (autoload 'maxima      "maxima"      "Running Maxima interactively" t)
; (autoload 'maxima-mode "maxima"      "Maxima editing mode" t)
; (autoload 'lua-mode    "/home/rogers/opt/share/emacs/site-lisp/lua-mode.el" "Lua editing mode" t)

; (setq load-path (cons  "/usr/local/share/lua/emacs" load-path ))
;;; (autoload 'lua "lua" "Running Lua interactively" t)

; (setq auto-mode-alist (cons '("Makefile\..*$" . makefile-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("[Mm]akefile\..*$" . makefile-gmake-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mak[e]*$" . makefile-gmake-mode) auto-mode-alist))

;REMOVED FOR MAC SHELL: (package-initialize)
; (require 'f)
; (require 'let-alist)
; (require 's)
; (require 'elm-mode)

; Some of these functions come from other sources, but most are original.

; -----------------------------------------------------------------------------
; It's usually easier to read regular expressions when the special characters
; are not escaped. This is especially true when dealing just with text.
(defconst simexp-regexp  "[|()<>.]")  ; full regexp for characters to escape

;------------------------------------------------------------------------------
(defun dr-simexp-to-regexp (REGEXP)
  "Converts a simplified regexp into the standard one. "
  "Simplified regular expressions lack all the backslashes."
  (replace-regexp-in-string simexp-regexp
                            (lambda (x) (concat "\\\\" x))
                            REGEXP))

;------------------------------------------------------------------------------
(defun dr-simexp-replace (SIMEXP REP STRING
                                 &optional FIXEDCASE LITERAL SUBEXP START)
  "Like replace-regexp-in-string but with simplified regexp."
  (replace-regexp-in-string (dr-simexp-to-regexp SIMEXP)
                            REP        ; Replacement to put in for regexp.
                            STRING     ; Source string to be modified for return.
                            FIXEDCASE  ; ?
                            LITERAL    ; ?
                            SUBEXP     ; ?
                            START))    ; Starting index for finding regexp.

;------------------------------------------------------------------------------
(defun dr-simexp-match (SIMEXP STRING &optional START)
  "Returns index of first match, or nil. Like string-match."
  (string-match (dr-simexp-to-regexp SIMEXP) STRING START))

;------------------------------------------------------------------------------
(defun dr-delete-to-nonwhite ()
  "Deletes characters at point until nonwhite is found."
  (interactive)
  (while (string-match "[ \t]" (string (char-after)))
    (delete-char 1)))

;------------------------------------------------------------------------------
(defun dr-droc-mark-line ()
  "Inserts a droc markup indicator at the end of the current line."
  (interactive)
  (end-of-line)
  (insert "//@")
)

; -----------------------------------------------------------------------------
(defun dr-hex-case-region (start end case-func)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char 1)
      (let ((count 0))
        (while (re-search-forward (dr-simexp-to-regexp "0([xX])([A-Za-z0-9]+)") nil 1)
          (let ((replacement (concat "0x" (funcall case-func (match-string 2)))))
            (if (not (equal replacement (match-string 0)))
                (progn
                  (replace-match replacement)
                  (setq count (1+ count))))))
        (message "converted %d hexadecimal constant%s" count (if (= count 1) "" "s"))))))

; -----------------------------------------------------------------------------
(defun dr-hex-upcase-region (start end)
  "Converts all C hexadecimal constants in region to be of the form 0xH[H..]."
  (interactive "r")
  (dr-hex-case-region start end 'upcase))

; -----------------------------------------------------------------------------
(defun dr-hex-downcase-region (start end)
  "Converts all C hexadecimal constants in region to be of the form 0xh[h..]."
  (interactive "r")
  (dr-hex-case-region start end 'downcase))

; -----------------------------------------------------------------------------
(defun dr-rm-log-nl ()
  "Removes linefeeds from log_xxxx() constructs."
  (interactive)
  (goto-char 1)
  (let ((count 0))
    (while (re-search-forward "\\(log_\\(error\\|warning\\|info\\|debug\\).*\\)\\\\n" nil 1)
      (replace-match 
       (replace-regexp-in-string "\\\\" "\\\\\\\\" (match-string 1)))
      (setq count (1+ count)))
    (message "replaced %d message%s" count (if (= count 1) "" "s"))))

; -----------------------------------------------------------------------------
; Return the character from the same column in the line above, or 0 if on
; first line, or '\n' if the previous line is shorter than the current
; position.
; NOTE: Don't use previous-line. It's a user-level function that can cause
;       weirdness when changing lines before/after running this macro.
(defun dr-char-above ()
  ; Yeah, I can see the need for line-move-visual. It makes Emacs more like
  ; other editors, and on rare occasion it's what I want Emacs to do with
  ; wrapped lines.
  (let ((save-line-move-visual line-move-visual)
        (column (current-column))
        (result 0))
    (setq line-move-visual nil)
    (if (> (line-number-at-pos) 1)
        (save-excursion
          (forward-line -1)
          (move-to-column column)
          (setq result (if (eolp) ?\n (following-char)))))
    (setq line-move-visual save-line-move-visual)
    result))
 
; -----------------------------------------------------------------------------
(defun dr-align-to-nonwhite-above (move-cursor-with-text)
  "Aligns to next white-to-nonwhite transition on previous line."
  (interactive "P")
  (let ((initial-position (point)))
    (if (> (line-number-at-pos) 1)
        (progn
          (dr-delete-to-nonwhite)
          (while (string-match-p "[^ \t\n\0]" (string (dr-char-above))) (insert " "))
          (while (string-match-p "[ \t]"      (string (dr-char-above))) (insert (dr-char-above)))))
    (if (not move-cursor-with-text)
        (progn (goto-char initial-position) (next-line)))))

; -----------------------------------------------------------------------------
(defun dr-spaceless-comment-start ()
  "Return the comment-start sequence, but without initial or final spaces."
  (car (split-string (if comment-start comment-start "**"))))

; -----------------------------------------------------------------------------
(defun dr-spaceless-comment-end ()
  "Return the comment-end sequence, but without initial or final spaces."
  (car (split-string (if comment-end comment-end "**"))))

; -----------------------------------------------------------------------------
(defun dr-insert-comment-start-space ()
  "Inserts comment-start, adding a space when necessary."
  (insert (dr-spaceless-comment-start) " "))

; -----------------------------------------------------------------------------
(defun dr-insert-space-comment-end ()
  "Inserts comment-end, first inserting a space when necessary."
  (insert " " (if (> (length comment-end) 0)
                  (dr-spaceless-comment-end)
                (dr-spaceless-comment-start))))

; -----------------------------------------------------------------------------
(defun dr-insert-comment-line (line &optional without-end)
  "Inserts comment-start, adding a space when necessary, followed by line."
  (dr-insert-comment-start-space)
  (insert line)
  (if (not without-end)
      (progn
        (dr-insert-space-comment-end)
        (insert "\n"))))

; -----------------------------------------------------------------------------
(defun dr-goto-match-pair (open close &optional backwards)
  (let ((count 1))
    (while (and (not (looking-at open)) (> (point) 1))
      (if backwards (backward-char 1) (forward-char 1)))
    (while (> count 0)
      (if backwards (backward-char 1) (forward-char 1))
      (cond ((looking-at open)  (setq count (1+ count)))
            ((looking-at close) (setq count (1- count)))
            (t nil)))))

; -----------------------------------------------------------------------------
(defun dr-prev-func-name ()
  (save-excursion
    (dr-goto-match-pair "[}]" "[{]" t)
    (dr-goto-match-pair "[)]" "[(]" t)
    (if (re-search-backward "[^A-Za-z0-9_:]\\([A-Za-z0-9_:]+\\)" nil t 1)
        (match-string 1)
      "unfound_function")))

; -----------------------------------------------------------------------------
(defun dr-indent-all ()
  "Runs indent-region on the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max))
)

; -----------------------------------------------------------------------------
(defun dr-insert-commented-function-name ()
  "Inserts start-comment ' ' function-name '()' end-comment."
  (interactive)
  (just-one-space 3)
  (dr-insert-comment-start-space)
  (insert (concat (dr-prev-func-name) "()"))
  (if (> (length comment-end) 0)
      (dr-insert-space-comment-end)))

(defun dr-insert-copyright (name)
  "Inserts a copyright notice for the given argument."
  (goto-char 1)
  (insert "\n")
  (forward-line -1)
  (dr-insert-comment-start-space)
  (insert "Copyright (c) ")
  (insert (format-time-string "%Y" (current-time)))
  (insert " ")
  (insert name)
  (insert ". All rights reserved.")
  (insert (if comment-end comment-end comment-start))
  (insert "\n"))
;  (dr-insert-comment-line "$Id$"))

;------------------------------------------------------------------------------
(defun dr-insert-company-copyright ()
  "Inserts a company copyright at top of the current buffer."
  (interactive)
  (dr-insert-copyright "Raytheon"))

;------------------------------------------------------------------------------
(defun dr-insert-rogers-copyright ()
  "Inserts Doug Rogers copyright at the top of the current buffer."
  (interactive)
  (goto-line 1)
  (beginning-of-line)
  (insert "\n")
  (previous-line 1)
  (dr-insert-comment-start-space)
  (insert "Copyright (c) ")
  (insert (format-time-string "%Y" (current-time)))
  (insert " Doug Rogers under the Zero Clause BSD License.")
  (insert comment-end)
  (insert "\n")
  (dr-insert-comment-start-space)
  (insert "You are free to do whatever you want with this software. See LICENSE.txt.")
  (insert comment-end)
  (insert "\n"))

;------------------------------------------------------------------------------
(defun dr-insert-char-above ()
  "Inserts character directly above point on previous line."
  (interactive)
  (insert (dr-char-above)))

;------------------------------------------------------------------------------
(defun dr-toggle-line-move-visual ()
  "Toggles the value of line-move-visual."
  (interactive)
  (setq line-move-visual (not line-move-visual)))

;------------------------------------------------------------------------------
(defun dr-insert-date-time ()
  "Inserts current date and time at point."
  (interactive)
  (insert (replace-regexp-in-string "[\n]" ""
                                    (format-time-string "%Y-%m-%d %a %H:%M:%S"
                                                        (current-time)))))

;------------------------------------------------------------------------------
(defun dr-insert-horizontal-rule-prefixed (prefix-string middle-char suffix-string)
  "Inserts an indented line of middle-chars after a prefix-string and followed by a suffix-string."
  (interactive)
  (beginning-of-line)
  (insert "\n")
  (previous-line 1)
  (indent-according-to-mode)
  (let ((prefix (if prefix-string prefix-string ""))
        (suffix (if suffix-string suffix-string "")))
    (insert prefix)
    (insert-char middle-char (- 79 (current-column) (length suffix)))
    (insert suffix)
    (beginning-of-line)
    (next-line 1)
    (indent-according-to-mode)))

;------------------------------------------------------------------------------
(defun dr-insert-horizontal-rule-stars ()
  "Inserts an indented line of asterisks inside comment characters."
  (interactive)
  (dr-insert-horizontal-rule-prefixed (dr-spaceless-comment-start) ?* (dr-spaceless-comment-end)))

;------------------------------------------------------------------------------
; TODO(dr): Use dr-insert-horizontal-rule-prefixed.
(defun dr-insert-horizontal-rule ()
  "Inserts an indented line of dashes inside comment characters."
  (interactive)
  (beginning-of-line)
  (insert "\n")
  (previous-line 1)
  (indent-according-to-mode)
  (dr-insert-comment-start-space)
  (insert-char ?- (- 79
                     (current-column)
                     (length (if comment-end comment-end ""))))
  (insert (if comment-end comment-end ""))
  (beginning-of-line)
  (next-line 1)
  (indent-according-to-mode))

;------------------------------------------------------------------------------
(defun dr-insert-buffer-name-base ()
  "Inserts the base of the buffer name (up to the '.')."
  (interactive)
  (string-match "\\([0-9A-Za-z_]*\\).*" (buffer-name))
  (let* ((s (match-string 1 (buffer-name)))
         (n (if s s "?")))
    (insert n)))

;------------------------------------------------------------------------------
(defun dr-last-directory-part (full_dir)
  "Looks for the last '/dir/' in full_dir (which must end with '/')."
  (string-match "/\\([^/]*\\)/$" full_dir)
  (let* ((s (match-string 1 full_dir))
         (n (if s s "?")))
    n))

;------------------------------------------------------------------------------
(defun dr-insert-header-guard ()
  "Uses buffer file name to create #ifndef header guard at point and file bottom."
  (interactive)
  (let* ((dir_name   (dr-last-directory-part (file-name-directory (buffer-file-name))))
         (file_name  (concat dir_name "_" (buffer-name)))
         (guard (upcase (concat (dr-simexp-replace "[^0-9A-Za-z_/\\]" "_" file_name) "_"))))
    (insert "#ifndef " guard "\n")
    (insert "#define " guard "\n")
    (insert "\n")
    (save-excursion
      (goto-char (point-max))
      (insert "\n")
      (insert "\n")
      (insert "#endif  ")
      (insert "// ")
      (insert guard)
      (insert "\n"))))
      ;; Eh, we rarely need C comments for this, so hard-code C++.
      ;; (dr-insert-comment-start-space)
      ;; (insert guard)
      ;; (insert (if comment-end comment-end "")))))

;------------------------------------------------------------------------------
(defun dr-insert-cplusplus-guard ()
  "Inserts #ifdef _cplusplus guard at point and closing guard at buffer bottom."
  (interactive)
  (save-excursion
    (insert "#ifdef __cplusplus\n")
    (insert "extern \"C\" {\n")
    (insert "#endif\n")
    ; Go to the end and start backtracking, skipping blank lines or the #endif
    ; for the include guard. If there are multiple #endifs
    (let ((after-open  (point))
          (endif-found nil))
      (goto-char (point-max))
      (forward-line -1)
      (while (and (not endif-found)
                  (or (looking-at "#endif") (looking-at "\n")))
        (setq endif-found (looking-at "#endif"))
        (forward-line -1))
      (if endif-found (forward-line 1))  ; Makes up for extra -1 in while loop above. Ugh.
      ; If we found no include guard, or if the #endif we found was the one
      ; we just inserted, then go back to the end.
      (if (or (not endif-found) (<= (point) after-open))
          (progn (goto-char (point-max)) (insert "\n"))))
    (insert "#ifdef __cplusplus\n")
    (insert "}\n")
    (insert "#endif\n")
    (insert "\n")))

;------------------------------------------------------------------------------
(defun dr-insert-doxygen-function-header ()
  "Inserts a doxygen function header at start of line containing point."
  (interactive)
  (dr-insert-horizontal-rule)
  (insert "/**\n")
  (insert " *\n")
  (insert " *\n")
  (insert " * @param name - description\n")
  (insert " * @param name - description\n")
  (insert " *\n")
  (insert " * @return\n")
  (insert " */\n")
  (beginning-of-line)
  (previous-line 7)
  (end-of-line)
  (insert " ")
)

;------------------------------------------------------------------------------
(defun dr-insert-agent-services-function-header (&optional error_prefix)
  "Inserts a doxygen-like function header as used by the Agent Services team at start of line containing point."
  (interactive)
  (dr-insert-horizontal-rule)
  (let ((error_prefix (if error_prefix error_prefix "ERROR")))
    (insert "/**\n")
    ;;  (dr-insert-horizontal-rule-prefixed (dr-spaceless-comment-start) ?* "")
    (insert " *\n")   ;; (insert " * @brief\n")
    (insert " *\n")
    (insert " * @param name - description\n")
    (insert " * @param name - description\n")
    (insert " *\n")
    (insert " * @return an error code:\n")
    (insert " * @retval ")
    (insert error_prefix)
    (insert "_SUCCESS if successful.\n")
    (insert " * @retval ")
    (insert error_prefix)
    (insert "_INVALID_PARAMETER if a parameter was incorrect.\n")
    (insert " * @retval ")
    (insert error_prefix)
    (insert "_... \n")
    (insert " * @retval another error code if a Windows API failed.\n")
    (insert " */\n")
    (beginning-of-line)
    (previous-line 11)
    (end-of-line)
    (insert " ")
))

;------------------------------------------------------------------------------
(defun dr-insert-agent-services-kernel-function-header ()
  "Inserts a doxygen-like function header as used by the Agent Services team at start of line containing point."
  (interactive)
  (dr-insert-agent-services-function-header "STATUS")
)

;------------------------------------------------------------------------------
(defun dr-insert-c-debug-line ()
  "Copies the current line ahead of itself, then wraps the result in printf()."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert
     "printf(\"%s:%u: %s()\\n\", __FILE__, __LINE__, __FUNCTION__);\n")))

;------------------------------------------------------------------------------
(defun dr-insert-c-struct-typedef (name)
  "Inserts a typedef/struct definition pair."
  (interactive "sName of struct: ")
  (indent-according-to-mode)
  (insert "typedef struct " name "_s " name "_t;")
  (indent-according-to-mode)
  (insert "\n\nstruct " name "_s")
  (indent-according-to-mode)
  (insert "\n{")
  (indent-according-to-mode)
  (insert "\n\n};   " comment-start "struct " name "_s" comment-end)
  (indent-according-to-mode)
  (insert "\n")
  (previous-line 2)
  (indent-according-to-mode))

;------------------------------------------------------------------------------
(defun dr-insert-c-enum-typedef (name)
  "Inserts a typedef/enum definition pair."
  (interactive "sName of enum: ")
  (indent-according-to-mode)
  (insert "typedef enum " name "_e " name "_t;")
  (indent-according-to-mode)
  (insert "\n\nenum " name "_e")
  (indent-according-to-mode)
  (insert "\n{")
  (indent-according-to-mode)
  (insert "\n\n};   " comment-start "enum " name "_e" comment-end)
  (indent-according-to-mode)
  (insert "\n")
  (previous-line 2)
  (indent-according-to-mode)
  (insert (upcase name))
  (insert "_"))

; -----------------------------------------------------------------------------
; Set the case of the current character and move to the next char.
(defun dr-set-case-current-char (case-func)
  (let ((c (string (char-after))))
    (delete-char 1)
    (insert (funcall case-func c))))   ; Insertion also moves to next char.

; -----------------------------------------------------------------------------
(defun dr-camel-case ()
  "Camel-cases the current word."
  (interactive)
  (let ((save-case-fold-search case-fold-search))
    (setq case-fold-search nil)
    ; Eh. Should I upcase the first letter? If so, do this:
    (if (looking-at-p "[a-z]")
        (dr-set-case-current-char 'upcase))
    (while (looking-at-p "[A-Za-z0-9_]")
      (if (looking-at-p "[_]")
          (progn
            (delete-char 1)     ; Delete '_' then upcase the following char.
            (dr-set-case-current-char 'upcase))
        (forward-char 1)))
    (setq case-fold-search save-case-fold-search)))

; -----------------------------------------------------------------------------
(defun dr-undo-camel-case ()
  "Removes camel case from the current word."
  (interactive)
  (let ((save-case-fold-search case-fold-search))
    (setq case-fold-search nil)
    (while (looking-at-p "[A-Za-z0-9_]")
      (if (looking-at-p "[A-Z]")
          (dr-set-case-current-char 'downcase))
        (forward-char 1)
        (if (looking-at-p "[A-Z]")
            (let ((c (string (char-after))))
              (delete-char 1)
              (insert (concat "_" (downcase c))))))
    (setq case-fold-search save-case-fold-search)))

; -----------------------------------------------------------------------------
(defun dr-quote-c-line ()
  "Trims and quotes the current line using the double-quote character."
  (interactive)
  (beginning-of-line)
  (just-one-space)
  (c-indent-command)
  (insert "\"")
  (end-of-line)
  (insert "\\n\"")
  (beginning-of-line)
  (next-line 1))

; -----------------------------------------------------------------------------
(defun dr-indent-insert-line (text)
  (indent-according-to-mode)
  (insert text)
  (indent-according-to-mode)    ; In case there's a syntactic element that will change indentation ({}, etc.)
  (insert "\n"))

;------------------------------------------------------------------------------
(defun dr-insert-C-Usage ()
  "Inserts a doxygen function header and code template for usage information."
  (interactive)
  (dr-insert-horizontal-rule)
  (insert "/**\n")
  (insert " * Prints usage information to @a file.\n")
  (insert " *\n")
  (insert " * Note: This function does not return.\n")
  (insert " *\n")
  (insert " * @param file - FILE stream to which to write the usage information.\n")
  (insert " *\n")
  (insert " * @param exit_code - value to pass to exit() when ending program.\n")
  (insert " */\n")
  (insert "void Usage(FILE* file, int exit_code) __attribute__((noreturn));\n")
  (insert "void Usage(FILE* file, int exit_code) {\n")
  (dr-indent-insert-line "fprintf(file,")
  (dr-indent-insert-line "\"\\n\"")
  (dr-indent-insert-line "\"USAGE\\n\"")
  (dr-indent-insert-line "\"    %s [options] args...\\n\"")
  (dr-indent-insert-line ", g_program);")
  (dr-indent-insert-line "fprintf(file,")
  (dr-indent-insert-line "\"\\n\"")
  (dr-indent-insert-line "\"DESCRIPTION\\n\"")
  (dr-indent-insert-line "\"    %s does this or that.\\n\"")
  (dr-indent-insert-line "\"    Or the other.\\n\"")
  (dr-indent-insert-line ", g_program);")
  (dr-indent-insert-line "fprintf(file,")
  (dr-indent-insert-line "\"\\n\"")
  (dr-indent-insert-line "\"OPTIONS\\n\"")
  (dr-indent-insert-line "\"    Options may begin with '-' or '--'. A ':' indicates where options may be\\n\"")
  (dr-indent-insert-line "\"    abbreviated\\n\");")
  (dr-indent-insert-line "fprintf(file,")
  (dr-indent-insert-line "\"\\n\"")
  (dr-indent-insert-line "\"    -h:elp                      Show this usage information.\\n\");")
  (dr-indent-insert-line "fprintf(file,")
  (dr-indent-insert-line "\"\\n\"")
  (dr-indent-insert-line "\"    -[no-]v:erbose              Print verbose (debug) messages. [%s-verbose]\\n\"")
  (dr-indent-insert-line ", kDefaultVerbose ? \"\" : \"-no\");")
  (dr-indent-insert-line "exit(exit_code);")
  (insert "}   /* Usage() */\n")
  (insert "\n"))

; -----------------------------------------------------------------------------
(defun dr-insert-C-PrintUsageError ()
  (dr-insert-horizontal-rule)
  (insert "/**\n")
  (insert " * Print an error message to stderr then exit the program with exit code 1.\n")
  (insert " */\n")
  (dr-indent-insert-line "void PrintUsageError(const char* format, ...) {")
  (dr-indent-insert-line "char text[0x0100] = \"\";")
  (dr-indent-insert-line "va_list va;")
  (dr-indent-insert-line "va_start(va, format);")
  (dr-indent-insert-line "vsnprintf(text, sizeof(text), format, va);")
  (dr-indent-insert-line "va_end(va);")
  (dr-indent-insert-line "fprintf(stderr, \"%s: %s\\n\", g_program, text);")
  (dr-indent-insert-line "fprintf(stderr, \"%s: Use '%s --help' for usage information.\\n\", g_program, g_program);")
  (dr-indent-insert-line "exit(1);")
  (dr-indent-insert-line "}   /* PrintUsageError() */")
  (insert "\n"))

; -----------------------------------------------------------------------------
(defun dr-insert-C-NamePartOfPath ()
      (dr-insert-horizontal-rule)
      (insert "/**\n")
      (insert " * Find and return a pointer to the file name portion of @a path.\n")
      (insert " *\n")
      (insert " * @param path - a path whose name is desired. Typically this is argv[0] from\n")
      (insert " * main().\n")
      (insert " *\n")
      (insert " * @return a pointer the first character after the last directory delimiter\n")
      (insert " * (forward or back slash) in @a path, or @a path if none is found.\n")
      (insert " */\n")
      (dr-indent-insert-line "const char* NamePartOfPath(const char* path) {")
      (dr-indent-insert-line "const char* rval = path;")
      (dr-indent-insert-line "if (NULL != path) {")
      (dr-indent-insert-line "  for (; 0 != *path; ++path) {")
      (dr-indent-insert-line "    if ((('/' == path[0]) || ('\\\\' == path[0])) &&")
      (dr-indent-insert-line "        !((0 == path[1]) || ('/' == path[1]) || ('\\\\' == path[1]))) {")
      (dr-indent-insert-line "       rval = &path[1];")
      (dr-indent-insert-line "    }")
      (dr-indent-insert-line "  }")
      (dr-indent-insert-line "}")
      (dr-indent-insert-line "return rval;")
      (dr-indent-insert-line "}   /* NamePartOfPath() */")
      (insert "\n"))

; -----------------------------------------------------------------------------
(defun dr-insert-C-IsOption ()
      (dr-insert-horizontal-rule)
      (insert "/**\n")
      (insert " * Look for an option of the form \"[-[-]]option[=value]\".\n")
      (insert " *\n")
      (insert " * If @a input contains '=' then non-null @a *value_ptr is set to point\n")
      (insert " * to the character after '=' (or is set to NULL if there is no argument).\n")
      (insert " *\n")
      (insert " * @a descriptor may contain ':' characters which indicate abbreviation\n")
      (insert " * points for the option. For example, \"o:pt:ion\" will match \"-o\",\n")
      (insert " * \"-o=value\", \"-opt\", \"-opt=value\", \"-option\" and \"-option=value\".\n")
      (insert " *\n")
      (insert " * @return 1 if @a input matches @a descriptor, 0 otherwise.\n")
      (insert " */\n")
      (dr-indent-insert-line "int IsOption(const char* input, const char** value_ptr, const char* descriptor) {")
      (dr-indent-insert-line "int rval = 0;")
      (dr-indent-insert-line "int finished = 0;")
      (dr-indent-insert-line "assert(NULL != input);")
      (dr-indent-insert-line "assert(NULL != descriptor);")
      (dr-indent-insert-line "if ('-' == *input) {")
      (dr-indent-insert-line "  ++input;")
      (dr-indent-insert-line "  if ('-' == *input) {")
      (dr-indent-insert-line "    ++input;")
      (dr-indent-insert-line "  }")
      (dr-indent-insert-line "} else {")
      (dr-indent-insert-line "  finished = 1;")
      (dr-indent-insert-line "}")
      (dr-indent-insert-line "while (!finished) {")
      (dr-indent-insert-line "  if ((0 == *input) || ('=' == *input)) {")
      (dr-indent-insert-line "    finished = 1;")
      (dr-indent-insert-line "    rval = (0 == *descriptor) || (':' == *descriptor);")
      (dr-indent-insert-line "  } else if ((0 == *descriptor) || ((':' != *descriptor) && (*input != *descriptor))) {")
      (dr-indent-insert-line "    finished = 1;")
      (dr-indent-insert-line "  } else {")
      (dr-indent-insert-line "    if (':' != *descriptor) {")
      (dr-indent-insert-line "      ++input;")
      (dr-indent-insert-line "    }")
      (dr-indent-insert-line "    ++descriptor;")
      (dr-indent-insert-line "  }")
      (dr-indent-insert-line "}")
      (dr-indent-insert-line "if (NULL != value_ptr) {")
      (dr-indent-insert-line "  *value_ptr = (rval && ('=' == *input)) ? (input + 1) : NULL;")
      (dr-indent-insert-line "}")
      (dr-indent-insert-line "return rval;")
      (dr-indent-insert-line "}   /* IsOption() */")
      (insert "\n"))

; -----------------------------------------------------------------------------
(defun dr-insert-C-IsFlagOption ()
      (dr-insert-horizontal-rule)
      (insert "/**\n")
      (insert " * Look for flag option of the form \"-[-][no-]option\".\n")
      (insert " *\n")
      (insert " * @a descriptor may contain ':' characters which indicate abbreviation\n")
      (insert " * points for the option. See IsOption() for more information.\n")
      (insert " *\n")
      (insert " * If @a input matches the descriptor then the value of @a *flag_value_ptr (if\n")
      (insert " * not NULL) will be set to 1. If @a input matches the descriptor with \"no-\"\n")
      (insert " * prefixed then @a *flag_value_ptr will be set to 0. If @a input does not\n")
      (insert " * match @a descriptor, @a *flag_value_ptr is not modified.\n")
      (insert " *\n")
      (insert " * @return 1 if @a input matches @a descriptor with or without a \"no-\" prefix,\n")
      (insert " * 0 otherwise.\n")
      (insert " */\n")
      (dr-indent-insert-line "int IsFlagOption(const char* input, int* flag_value_ptr, const char* descriptor) {")
      (dr-indent-insert-line "int flag_value = 1;")
      (dr-indent-insert-line "int rval = 0;")
      (dr-indent-insert-line "assert(NULL != input);")
      (dr-indent-insert-line "assert(NULL != descriptor);")
      (dr-indent-insert-line "if ('-' == *input) {")
      (dr-indent-insert-line "  rval = IsOption(input, NULL, descriptor);")
      (dr-indent-insert-line "  if (!rval) {")
      (dr-indent-insert-line "    flag_value = 0;")
      (dr-indent-insert-line "    const int k = ('-' == input[1]) ? 1 : 0;")
      (dr-indent-insert-line "    if (('n' == input[k+1]) && ('o' == input[k+2]) && ('-' == input[k+3])) {")
      (dr-indent-insert-line "      rval = IsOption(&input[k+3], NULL, descriptor);")
      (dr-indent-insert-line "    }")
      (dr-indent-insert-line "  }")
      (dr-indent-insert-line "}")
      (dr-indent-insert-line "if (rval && (NULL != flag_value_ptr)) {")
      (dr-indent-insert-line "  *flag_value_ptr = flag_value;")
      (dr-indent-insert-line "}")
      (dr-indent-insert-line "return rval;")
      (dr-indent-insert-line "}   /* IsFlagOption() */")
      (insert "\n"))

; -----------------------------------------------------------------------------
(defun dr-insert-C-ParseOptions ()
      (dr-insert-horizontal-rule)
      (insert "/**\n")
      (insert " * Parse options from the command line, removing them from @a argv[].\n")
      (insert " *\n")
      (insert " * Note that on error this function does not return.\n")
      (insert " *\n")
      (insert " * If an argument starts with '-' then it will be treated as an option. See\n")
      (insert " * the OPTIONS section of Usage() for the list of options available to this\n")
      (insert " * program.\n")
      (insert " *\n")
      (insert " * If \"--\" is encountered as an argument, no further option processing will\n")
      (insert " * occur, even if a later argument begins with '-'.\n")
      (insert " *\n")
      (insert " * @param argc - number of argument pointers in array @a argv[].\n")
      (insert " *\n")
      (insert " * @param argv - array of argument string pointers to parse.\n")
      (insert " *\n")
      (insert " * @return the number of (non-option) command line arguments that remain in\n")
      (insert " * @a argv[] after option processing.\n")
      (insert " */\n")
      (dr-indent-insert-line "int ParseOptions(int argc, char* argv[]) {")
      (dr-indent-insert-line "int rval = 1;       /* Skip program name. */")
      (dr-indent-insert-line "char** non_option_argument_list = argv;")
      (dr-indent-insert-line "int i = 0;")
      (dr-indent-insert-line "int end_of_options = 0;")
      (dr-indent-insert-line "for (i = 1; i < argc; ++i) {")
      (dr-indent-insert-line "  char* arg = argv[i];")
      (dr-indent-insert-line "  if (end_of_options || ('-' != *arg)) {")
      (dr-indent-insert-line "    non_option_argument_list[rval++] = arg;")
      (dr-indent-insert-line "  } else if (('-' == arg[1]) && (0 == arg[2])) {")
      (dr-indent-insert-line "    end_of_options = 1;")
      (dr-indent-insert-line "  } else if (IsOption(arg, NULL, \"h:elp\")) {")
      (dr-indent-insert-line "    Usage(stdout, 0);")
      (dr-indent-insert-line "  } else if (IsFlagOption(arg, &g_verbose, \"v:erbose\")) {")
      (dr-indent-insert-line "  } else {")
      (dr-indent-insert-line "    PrintUsageError(\"invalid option \\\"%s\\\"\", arg);")
      (dr-indent-insert-line "  }")
      (dr-indent-insert-line "}")
      (dr-indent-insert-line "return rval;")
      (dr-indent-insert-line "}   /* ParseOptions() */")
      (insert "\n"))

;------------------------------------------------------------------------------
(defun dr-insert-C-main ()
  "Inserts a doxygen function header at start of line containing point."
  (interactive)
  (dr-insert-horizontal-rule)
  (insert "/**\n")
  (insert " * Main program. Parses command line arguments. See Usage().\n")
  (insert " *\n")
  (insert " * @param argc - number of command line arguments, including program name.\n")
  (insert " *\n")
  (insert " * @param argv - list of pointers to command line argument strings.\n")
  (insert " *\n")
  (insert " * @return the program's exit code: 0 on success, something else on failure.\n")
  (insert " */\n")
  (dr-indent-insert-line "int main(int argc, char* argv[]) {")
  (dr-indent-insert-line "g_program = NamePartOfPath(argv[0]);")
  (dr-indent-insert-line "argc = ParseOptions(argc, argv);  /* Remove options; leave program name and arguments. */")
  (dr-indent-insert-line "if (argc < 2) {")
  (dr-indent-insert-line "  Usage(stderr, 1);")
  (dr-indent-insert-line "}")
  (dr-indent-insert-line "/* add code here */")
  (dr-indent-insert-line "return 0;")
  (dr-indent-insert-line "}   /* main() */"))

;------------------------------------------------------------------------------
(defun dr-insert-C-main-program ()
  "Inserts the main chunks for a C program."
  (interactive)
  (if (not comment-start) (message "Set mode to one that has comment characters.")
    (progn
      (dr-insert-company-copyright)
      (insert "\n")
      (insert "#include <assert.h>\n")
      (insert "#include <stdarg.h>\n")
      (insert "#include <stdio.h>\n")
      (insert "#include <stdlib.h>\n")
      (insert "\n")
      (insert "/**\n")
      (insert " * Default name of this program.\n")
      (insert " */\n")
      (insert "#define kProgram \"")
      (insert (file-name-sans-extension (file-name-nondirectory (buffer-name))))
      (insert "\"\n")
      (insert "\n")
      (insert "/**\n")
      (insert " * Name of this program; this may be modified by argv[0] in main().\n")
      (insert " */\n")
      (insert "const char* g_program = kProgram;\n")
      (insert "\n")
      (insert "/**\n")
      (insert " * Default value for verbose setting.\n")
      (insert " */\n")
      (insert "#define kDefaultVerbose 0\n")
      (insert "\n")
      (insert "/**\n")
      (insert " * Whether or not to emit verbose messages.\n")
      (insert " */\n")
      (insert "int g_verbose = 0;\n")
      (insert "\n")
      (dr-insert-C-Usage)
      (dr-insert-C-PrintUsageError)
      (dr-insert-C-NamePartOfPath)
      (dr-insert-C-IsOption)
      (dr-insert-C-IsFlagOption)
      (dr-insert-C-ParseOptions)
      (dr-insert-C-main)
      (previous-line 3)        ; This will place us on the open line in main().
      (indent-according-to-mode))))

;------------------------------------------------------------------------------
(defun dr-mark-all ()
  "Sets the mark at the top of the file then moves to the bottom."
  (interactive)
  (beginning-of-buffer)
  (mark)
  (end-of-buffer)
  (message "Set region to entire buffer.")
)

; -----------------------------------------------------------------------------
; This is a hack until I can figure out how to gleen syntax information from
; the font-lock system. Using (c-guess-basic-syntax) appears to report only
; the syntax context for the start of the line.
(defun dr-insert-previous-function-name ()
  "Inserts the name of the previously defined or declared function."
  (interactive)
  (message "syntax: %s" (c-guess-basic-syntax)))
  ;; (let* ((name   "Unknown")
  ;;        (syntax (if (boundp 'c-syntactic-context)
  ;;   	     ;; Use `c-syntactic-context' in the same way as
  ;;   	     ;; `c-indent-line', to be consistent.
  ;;   	     c-syntactic-context
  ;;   	   (c-save-buffer-state nil
  ;;   	     (c-guess-basic-syntax)))))
  ;;   (save-excursion
  ;;     (while
  ;;         (

;------------------------------------------------------------------------------
; Compile, but place cursor at bottom of other window to watch it scroll.
; (fset 'recompile-watch   [?\C-x ?\C-s ?\M-x ?r ?e ?c ?o ?m ?p ?i ?l ?e return ?\C-x ?o ?\M-> ?\C-x ?o])
(defun dr-recompile-watch ()
  "Recompile with previous settings and watch the output scroll."
  (interactive)
  (recompile)
  (end-of-buffer-other-window 0))

; -----------------------------------------------------------------------------
(defun dr-compile-gb-capture ()
  "Compile Ghostbuster capture software."
  (interactive)
  (compile "make -C ~/svn/gbfirmware/exes/capture_arm"))

;------------------------------------------------------------------------------
(defun dr-untabify-entire-buffer ()
  "Untabifies the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

;------------------------------------------------------------------------------
(defun dr-disable-indent-tabs-mode ()
  "Set 'indent-tabs-mode to nil."
  (interactive)
  (setq 'indent-tabs-mode nil))

;------------------------------------------------------------------------------
(defun dr-revert-buffer-now ()
  "Reverts the buffer immediately without confirming."
  (interactive)
  (revert-buffer t))

;------------------------------------------------------------------------------
; From http://happygiraffe.net/emacstips
(defun dr-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;------------------------------------------------------------------------------
; From http://stackoverflow.com/questions/530461/
;            emacs-lisp-function-to-toggle-variable-tab-width-between-4-8
(defun dr-toggle-tab-width-setting ()
    "Toggle setting tab widths between 4 and 8"
    (interactive)
    (setq tab-width (if (= tab-width 8) 4 8))
    (redraw-display))

;------------------------------------------------------------------------------
; My own...
(defun dr-set-tab-width-for-buffer (new-width)
    "Set tab-width for current buffer"
    (interactive "NTab width: ")
    (setq tab-width new-width)
    (redraw-display))
(defun dr-set-tab-width-4 () (interactive) (dr-set-tab-width-for-buffer 4))
(defun dr-set-tab-width-8 () (interactive) (dr-set-tab-width-for-buffer 8))

(defun dr-set-tab-width-default (new-width)
    "Set tab-width for subsequent buffers"
    (interactive "NTab width: ")
    (setq-default tab-width new-width)
    (redraw-display))

;------------------------------------------------------------------------------
; My bell function doesn't do anything. No screen flash, no noise. I'm amazed
; that there is no way to turn it off besides completely replacing the
; function. The problem with this approach is that it does not then interrupt
; failing macros.
; See: http://www.emacswiki.org/emacs/AlarmBell

;; (setq bell-on nil)      ; I hate that thing.

;; ;; (if (not (boundp 'orig-bell-function))
;; ;;     (setq orig-bell-function bell)
;; ;;   nil)

;; (defun dr-ring-bell-function (&optional do-not-terminate)
;;   (if (not (equal bell-on nil)) (bell do-not-terminate) nil))

;; (setq ring-bell-function (lambda () (dr-ring-bell-function)))
;; ;; (message "bell-on is %S, ring-bell-function is %S" bell-on ring-bell-function)

;; ;------------------------------------------------------------------------------
;; ; Toggles the bell function.
;; (defun dr-toggle-bell ()
;;   "Toggles bell activity - visible or audible."
;;   (interactive)
;;   (setq ring-bell-function (if (boundp ring-bell-function)
;;                                (if (not (equal ring-bell-function
;;   (setq bell-on (not bell-on)))  ; (if bell-on nil t)))

(setq visible-bell t)   ; This is actually tolerable in Windows.

(defun dr-toggle-bell ()
  "Toggles the visual bell."
  (interactive)
  (setq visible-bell (equal visible-bell nil)))

;------------------------------------------------------------------------------
;(defun dr-add-comma-to-word ()
;  "Adds a comma to the next word."
;  (interactive)
;  (indent-according-to-mode)
;  (just-one-space)
;  (indent-according-to-mode)
;  (while (string-match "[-+._a-zA-Z0-9]" (string (char-after)))
;    (next-char 1))
;  (insert ","))

;------------------------------------------------------------------------------
; These are from ORA's _Learning GNU Emacs_, chapter 13.
(defun dr-count-words-buffer ()
  "Counts the words in the current buffer."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-word 1)
        (setq count (1+ count)))
      (message "buffer contains %d words." count))))

;------------------------------------------------------------------------------
(defun dr-goto-percent (pct)
  "Move point to a percentage into the buffer."
  (interactive "NPercent: ")     ; 'N' means read integer or use
                                 ;   C-u value; 'n' will always prompt.
  (goto-char (/ (* pct (point-max)) 100)))

;------------------------------------------------------------------------------
; Based on code from Joe Seeger 20051110:
; NOTE: base64-decode-region already exists in Emacs!
;(defun dr-base64-fixchar (c)
;  "Converts a character from its base64 ASCII to a number 0 .. 63."
;  (interactive c)
;  (cond ((and (> c 64) (< c  91)) (setq c (- c 65)))
;        ((and (> c 96) (< c 123)) (setq c (- c 71)))
;        ((and (> c 47) (< c  58)) (setq c (+ c 4)))
;        ((= c 43)                 (setq c 62))
;        ((= c 47)                 (setq c 63))
;        (t c)))

;------------------------------------------------------------------------------
; From Joe Seeger 20051110:
;(defun dr-base64-decode-region (start end)
;  "Converts region from base64 to plain text."
;  (interactive "r")
;  (narrow-to-region start end)
;  (goto-char (point-min))
;  (while (re-search-forward "[^A-Za-z0-9/+]" nil t)
;    (replace-match "" nil nil))
;  (goto-char (point-min))
;  (let ((a "") (b "") (c "") (d "")
;    (x 0) (y 0) (z 0))
;    (while (< (point) (point-max))
;      (setq a (dr-base64-fixchar (following-char))) (delete-char 1)
;      (setq b (dr-base64-fixchar (following-char))) (delete-char 1)
;      (setq c (dr-base64-fixchar (following-char))) (delete-char 1)
;      (setq d (dr-base64-fixchar (following-char))) (delete-char 1)
;
;      (setq x (+ (* a 4) (/ b 16)))
;      (setq y (+ (mod (* b 16) 256) (/ c 4)))
;      (setq z (+ (mod (* c 64) 256) d))
;
;      (insert x y z)
;    )
;  )
;  (widen)
;)

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; Load all the custom functions followed by key bindings for them.
; Miscellaneous:
(global-set-key "\C-^"      'dr-insert-char-above)
; With C-c:
(global-set-key "\C-c "     'dr-delete-to-nonwhite)
(global-set-key "\C-c>"     'dr-align-to-nonwhite-above)
(global-set-key "\C-c%"     'dr-match-paren)    ; Like vi's use of '%', but with C-c first.
(global-set-key "\C-c-"     'dr-insert-horizontal-rule)
(global-set-key "\C-c*"     'dr-insert-horizontal-rule-stars)
(global-set-key "\C-c4"     'dr-set-tab-width-4)
(global-set-key "\C-c8"     'dr-set-tab-width-8)
(global-set-key "\C-cdA"    'dr-mark-all)
(global-set-key "\C-cdb"    'dr-insert-buffer-name-base)
(global-set-key "\C-cdB"    'dr-toggle-bell)
(global-set-key "\C-cdc"    'dr-insert-rogers-copyright)
(global-set-key "\C-cdC"    'c++-mode)
(global-set-key "\C-cd\C-C" 'mandiant-set-c-style)
(global-set-key "\C-cdd"    'dr-insert-date-time)
(global-set-key "\C-cde"    'dr-insert-c-enum-typedef)
(global-set-key "\C-cdf"    'dr-insert-commented-function-name)
(global-set-key "\C-cdh"    'dr-insert-header-guard)
(global-set-key "\C-cd\C-h" 'dr-hex-upcase-region)
(global-set-key "\C-cd\M-h" 'dr-hex-downcase-region)
(global-set-key "\C-cdi"    'indent-region)
(global-set-key "\C-cd\C-i" 'dr-disable-indent-tabs-mode)
(global-set-key "\C-cdI"    'dr-indent-all)
(global-set-key "\C-cdm"    'dr-insert-C-main)
(global-set-key "\C-cdM"    'dr-insert-C-main-program)
(global-set-key "\C-cd\C-m" 'dr-droc-mark-line)
(global-set-key "\C-cdp"    'dr-insert-cplusplus-guard)
(global-set-key "\C-cdr"    'dr-insert-rogers-copyright)
(global-set-key "\C-cdu"    'dr-undo-camel-case)
(global-set-key "\C-cdU"    'dr-untabify-entire-buffer)
(global-set-key "\C-cd\C-u" 'dr-camel-case)
(global-set-key "\C-cdx"    'compile)
(global-set-key "\C-cdX"    'dr-recompile-watch)
(global-set-key "\C-cdy"    'dr-insert-doxygen-function-header)
(global-set-key "\C-cdY"    'dr-insert-agent-services-function-header)
(global-set-key "\C-cd\C-y" 'dr-insert-agent-services-kernel-function-header)
(global-set-key "\C-cdw"    'whitespace-mode)
(global-set-key "\C-cdW"    'whitespace-cleanup)
(global-set-key "\C-cf"     'describe-function)
(global-set-key "\C-cg"     'goto-line)
(global-set-key "\C-cl"     'dr-toggle-line-move-visual)
(global-set-key "\C-cN"     'dr-insert-previous-function-name)
(global-set-key "\C-co"     'overwrite-mode)
(global-set-key "\C-cp"     'dr-insert-c-debug-line)
(global-set-key "\C-cq"     'dr-quote-c-line)
(global-set-key "\C-ct"     'dr-insert-c-struct-typedef)
(global-set-key "\C-cw"     'write-region)
(global-set-key "\C-c\C-c"  'comment-region)
(global-set-key "\C-c\C-i"  'dr-set-tab-width-for-buffer)
(global-set-key "\C-c\C-m"  'menu-bar-open)
(global-set-key "\C-c\C-r"  'dr-revert-buffer-now)
; With C-x:
(global-set-key "\C-xk"     'kill-this-buffer)      ; 'kill-buffer prompts for buffer to kill.
(global-set-key "\C-x\C-m"  'eval-region)
; (global-set-key "\C-xc"     'compile)
; (global-set-key "\C-xC"     'dr-recompile-watch)   ; Takes over find-file-read-only short-cut.
(global-set-key "\C-xg"     'dr-compile-gb-capture)
(global-set-key '[M-up]     (lambda () (interactive) (scroll-up 1)))
(global-set-key '[M-down]   (lambda () (interactive) (scroll-down 1)))

; It gets confused with these in -nw on some systems. Not sure why.
;; (global-set-key '[S-up]    'windmove-up)
;; (global-set-key '[S-down]  'windmove-down)
;; (global-set-key '[S-left]  'windmove-left)
;; (global-set-key '[S-right] 'windmove-right)
;; (global-set-key '[S-C-up]    'enlarge-window)
;; (global-set-key '[S-C-down]  'shrink-window)
;; (global-set-key '[S-C-left]  'shrink-window-horizontally)
;; (global-set-key '[S-C-right] 'enlarge-window-horizontally)
(global-set-key "\C-xK" 'windmove-up)   ; Homage to vi.
(global-set-key "\C-xJ" 'windmove-down)
(global-set-key "\C-xH" 'windmove-left)
(global-set-key "\C-xL" 'windmove-right)
(global-set-key "\C-ch" 'shrink-window-horizontally)
(global-set-key "\C-cH" 'enlarge-window-horizontally)
(global-set-key "\C-cv" 'shrink-window-vertically)
(global-set-key "\C-cV" 'enlarge-window-vertically)

; Move between adjacent buffers.
(global-set-key [(control meta n)] 'next-buffer)
(global-set-key [(control meta p)] 'previous-buffer)
(global-set-key (kbd "C-M-_") 'mode-line-other-buffer)  ; Actually C-M--, but it uses _ for the key name.

(fset 'hex-char-convert
   [?\C-d ?\C-d ?\C-  ?\C-f ?\C-w ?\C-f ?\C-y])

(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

; (defconst dr-c-style "ellemtel")
(defconst dr-c-style "linux")

(defun add-unix-c-hooks ()
  "Adds C/C++ mode hooks for Unix environments."
  (add-hook 'c-mode-hook
            '(lambda ()
               (c-set-style dr-c-style)
               (c-set-offset 'inextern-lang 0)   ; Do not indent in extern "C".
               (c-set-offset 'innamespace   0)   ; Do not indent in namespaces.
               (setq indent-tabs-mode nil)       ; In Unix I never want to use tabs.
               (setq c-basic-offset 4)))
  (add-hook 'c++-mode-hook
            '(lambda ()
               (c-set-style mandiant-c-style)
               (c-set-offset 'inextern-lang 0)   ; Do not indent in extern "C".
               (c-set-offset 'innamespace   0)   ; Do not indent in namespaces.
               (setq indent-tabs-mode nil)       ; In Unix I never want to use tabs.
               (setq c-basic-offset 4))))

(defun add-windows-c-hooks ()
  "Adds C/C++ mode hooks for Unix environments."
  (add-hook 'c-mode-hook
            '(lambda ()
               (c-set-style dr-c-style)
               (c-set-offset 'inextern-lang 0)   ; Do not indent in extern "C".
               (c-set-offset 'innamespace   0)   ; Do not indent in namespaces.
               (setq indent-tabs-mode nil)       ; At Mandiant, drivers (C) do not use tabs.
               (setq tab-width 4)
               (setq c-basic-offset 4)))

  (add-hook 'c++-mode-hook
            '(lambda ()
               (c-set-style mandiant-c-style)
               (c-set-offset 'inextern-lang 0)   ; Do not indent in extern "C".
               (c-set-offset 'innamespace   0)   ; Do not indent in namespaces.
;               (setq indent-tabs-mode t)         ; At Mandiant it appears to be the norm in Windows...
               (setq indent-tabs-mode nil)       ; ... not necessarily true.
               (setq tab-width 4)
               (setq c-basic-offset 4))))

(if (eq 'windows-nt system-type)
    (add-windows-c-hooks)
  (add-unix-c-hooks))

; If line-move-visual is non-nil, then moving up and down lines will follow
; the display lines, not the buffer lines. This only matters when lines are
; wrapped. Setting it to non-nil might seem natural - the cursor will always
; move up or down a single line - but it makes the definition of keyboard
; macros depend on the size of the window. I don't like that, hence nil:
(setq line-move-visual nil)      ; (setq line-move-visual t)

; Taken from http://www.emacswiki.org/emacs/HideShow
(global-set-key "\C-c+"  'hs-toggle-hiding)
(global-set-key "\C-c+"  'hide-ifdef-block)
; (global-set-key "\C-\\") 'toggle-selective-display)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)

;REMOVED FOR MAC SHELL: (require 'package)
(custom-set-variables
 '(column-number-mode t)
 '(auto-save-interval 1000)
 '(c-basic-offset 4)
 '(sh-basic-offset 4)
 '(sh-indentation 4)
 '(c-label-minimum-indentation (quote -))
 '(indent-tabs-mode nil)     ; In Unix I never want to use tabs. Use t (not nil) for tabs.
 '(fill-column 77)
 '(hide-ifdef-read-only 1)
 '(python-indent 4))

(prefer-coding-system 'utf-8)

(customize-set-variable 'whitespace-line-column 120)
(customize-set-variable 'whitespace-style '(trailing lines-tail face tabs tab-mark))

(set-face-foreground 'font-lock-comment-face "red")

(custom-set-faces
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 90 :width normal)))))

; Taken from the unaccepted answer at:
; https://stackoverflow.com/questions/92971/how-do-i-set-the-size-of-emacs-window:
(if (display-graphic-p)
    (set-frame-size (selected-frame) 120 64))

; Old method that doesn't work well with graphical display:
;(add-to-list 'default-frame-alist (cons 'width  120))
;(add-to-list 'default-frame-alist (cons 'height  64))
