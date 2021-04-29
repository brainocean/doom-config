;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Bing Han"
      user-mail-address "bing.han@sap.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 20 :weight 'semilight))
;; (setq doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :size 28))
;; (setq doom-font (font-spec :family "SauceCodePro Nerd Font" :size 20 :weight 'light))
(setq doom-font (font-spec :family "Iosevka SS04" :size 18 :weight 'semilight))
(setq doom-big-font (font-spec :family "Iosevka SS04" :size 28 :weight 'semilight))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "PingFang SC" :size 18)))

;; Use Sarasa Mono SC for org table because it helps align Chinese and English in table
(after! org (set-face-attribute 'org-table nil :font (if writeroom-mode "Sarasa Mono SC 38"  "Sarasa Mono SC 18")))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-oceanic-next)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq evil-cleverparens-use-s-and-S nil)

(add-hook 'prog-mode-hook #'mac-auto-operator-composition-mode)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
(add-hook 'racket-mode-hook #'evil-cleverparens-mode)

(use-package evil-pinyin
  :init
  (setq-default evil-pinyin-with-search-rule 'always)
  :config
  (global-evil-pinyin-mode))

(map! :map emacs-lisp-mode-map
      "C-c C-c" #'eval-defun
      "C-c C-e" #'eval-last-sexp)

;; (use-package forge
;;   :config
;;   (add-to-list 'forge-alist '("github.wdf.sap.corp" "github.wdf.sap.corp/api" "github.wdf.sap.corp" forge-github-repository)))
;; (setq ghub-use-workaround-for-emacs-bug 'force)
(doom/set-indent-width 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-width 2)

(setq doom-modeline-height 20)
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)

; indents configurations for JavaScript and React development
(setq js-indent-level 2)
(setq-default js2-basic-offset 2)
(setq-default typescript-indent-level 2)
(setq-default web-mode-markup-indent-offset 2)
(setq-default web-mode-code-indent-offset 2)
(setq tide-format-options '(:indentSize 2 :tabSize 2 :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; (add-hook 'web-mode-hook 'skewer-html-mode)

(setq completion-ignore-case  t)
(setq tide-completion-ignore-case t)

(setq +format-with-lsp nil)

(setq prettier-js-show-errors nil)

; need this setting for displaying picture in Jupyter notebooks
(setq ein:output-area-inlined-images t)

;; (setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|127\\.0\\.0\\.1\\|10\\..*\\|192\\.168\\..*\\|*\\.sap\\.corp\\|*\\.corp\\.sap\\)")
;;      ("http" . "proxy.pvgl.sap.corp:8080")
;;      ("https" . "proxy.pvgl.sap.corp:8080")))

(put 'dired-find-alternate-file 'disabled nil)

(setq racket-program "/Applications/Racket v7.8/bin/racket")

(setq comint-prompt-read-only nil)

(setq clojure-toplevel-inside-comment-form t)

(setq lsp-ui-sideline-show-code-actions nil)
