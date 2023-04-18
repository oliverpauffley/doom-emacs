;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Oliver Pauffley"
      user-mail-address "mrpauffley@gmail.com")
(setq auth-sources '("~/.authinfo.gpg"))

;; turn off tabs
(setq-default indent-tabs-mode nil)
;; set indents to 4
(setq-default tab-width 4)

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-pine)

;; Modeline settings
    (custom-set-faces!
      '(mode-line :family "GohuFont Nerd Font")
      '(mode-line-inactive :family "GohuFont Nerd Font"))
(setq doom-modeline-modal-icon nil)

(load-file "~/.doom.d/functions.el")

;; TODO use something like this unless to set the font without errors.
;;(setq doom-font (font-spec :family "Attribute Mono" :size 22))
;;(unless (find-font doom-font)
;;  (setq doom-font (font-spec :family "Cascadia Code PL" :size 20)))
;; set font
(setq doom-font (font-spec :family "mononoki Nerd Font" :size 25 )
      doom-variable-pitch-font (font-spec :family "GohuFont Nerd Font" :size  25))

;; Projectile
(setq
 projectile-project-search-path '("~/code" "~/code/rust" "~/go/src/github.com/utilitywarehouse")
 )

;; Magit
(setq magit-repository-directories `(("~/code". 5)))

(setq forge-owned-accounts '(("oliverpauffley" nil)))

;; Kubernetes bindings
(use-package kubernetes
  :defer
  :commands (kubel))
(use-package kubel-evil
  :defer
  :after kubel)
(map! :leader
      (:prefix "o"
       :desc "Kubernetes" "K" 'kubel))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Go settings
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; Rust Settings
;;
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (add-to-list 'auto-mode-alist '("\\.ron\\'" . rustic-mode))
  (setq lsp-rust-analyzer-inlay-hints-mode "true")
  )


(use-package! ron-mode
  :defer t
  :mode (("\\.ron\\'" . ron-mode)))

(setq dap-cpptools-extension-version "1.5.1")

  (with-eval-after-load 'lsp-rust
    (require 'dap-cpptools))

  (with-eval-after-load 'dap-cpptools
    ;; Add a template specific for debugging Rust programs.
    ;; It is used for new projects, where I can M-x dap-edit-debug-template
    (dap-register-debug-template "Launch Executable"
                                 (list :type "cppdbg"
                                       :request "launch"
                                       :name "Rust::Run"
                                       :MIMode "gdb"
                                       :miDebuggerPath "rust-gdb"
                                       :environment []
                                       :program "${workspaceFolder}/target/debug/hello / replace with binary"
                                       :cwd "${workspaceFolder}"
                                       :console "external"
                                       :dap-compilation "cargo build"
                                       :dap-compilation-dir "${workspaceFolder}")))
  (with-eval-after-load 'dap-mode
    (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
    (dap-auto-configure-mode +1))


;; Java
;;
(setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")

(setq org-format-latex-options
   '(:foreground default :background default :scale 3 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
     ("begin" "$1" "$" "$$" "\\(" "\\[")))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq org-table-convert-region-max-lines 10000)

(map! :map cdlatex-mode-map
      :i "TAB" #'cdlatex-tab)
(add-hook 'latex-mode-hook 'turn-on-cdlatex)

(add-hook! 'elfeed-search-mode-hook 'elfeed-update)
(setq rmh-elfeed-org-files ' ("~/.doom.d/rss.org"))
(map! :leader
      (:prefix "o"
       :desc "Elfeed" "E" 'elfeed))

;; set org capture templates
(after! org
  ;; PlantUML settings
  (setq
   plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar")
   org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar")
   plantuml-default-exec-mode 'jar
   org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
   )
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)(nix . t)))
  (setq
   ;; Org Capture
   org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)"))
   org-capture-templates
   '(
     ("t" "Personal todo" entry
      (file+olp +org-capture-todo-file "Inbox" "Home")
      "* TODO %?\n%i\n%a" :prepend t)
     ("w" "Work todo" entry
      (file+olp +org-capture-todo-file "Inbox" "Work")
      "* TODO %?\n%i\n%a" :prepend t)
     ("n" "Personal notes" entry
      (file+headline +org-capture-notes-file "Inbox")
      "* %u %?\n%i\n%a" :prepend t)
     ("W" "Wishlist" table-line
      (file+headline "wishlist.org" "Wishlist")
      "| %t | %? | %x |  |")
     ("j" "Journal" entry
      (file+olp+datetree +org-capture-journal-file)
      "* %U %?\n%i\n%a" :prepend t)
     ("j" "Journal" entry
      (file+olp+datetree +org-capture-journal-file)
      "* %U %?\n%i\n%a" :prepend t)
     ("p" "Templates for projects")
     ("pt" "Project-local todo" entry
      (file+headline +org-capture-project-todo-file "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
     ("pn" "Project-local notes" entry
      (file+headline +org-capture-project-notes-file "Inbox")
      "* %U %?\n%i\n%a" :prepend t)
     ("pc" "Project-local changelog" entry
      (file+headline +org-capture-project-changelog-file "Unreleased")
      "* %U %?\n%i\n%a" :prepend t)
     ("o" "Centralized templates for projects")
     ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
     ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
     ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))))



;; Emails
(set-email-account! "mrpauffley@gmail.com"
                    '((mu4e-sent-folder       . "/[Gmail].Sent Mail")
                      (mu4e-drafts-folder     . "/drafts")
                      (mu4e-trash-folder      . "/[Gmail].Bin")
                      (mu4e-refile-folder     . "/[Gmail].All Mail")
                      (smtpmail-smtp-user . "mrpauffley@gmail.com")
                      (mu4e-compose-signature . "---\nOliver Pauffley"))t)
;; tell message-mode how to send mail
(setq message-send-mail-function 'smtpmail-send-it)
;; if our mail server lives at smtp.example.org; if you have a local
;; mail-server, simply use 'localhost' here.
(setq smtpmail-smtp-server "smtp.gmail.com")

;; vterm settings
(setq vterm-shell "fish")

;; nix lsp
(add-hook 'nix-mode-hook #'lsp)
;; org dnd export
(require 'ox-dnd)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
