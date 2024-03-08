;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Oliver Pauffley"
      user-mail-address "mrpauffley@gmail.com")
(setq auth-sources '("~/.authinfo.gpg"))

;; TODO set this up
;;(auth-source-1password-enable)
;;(setq auth-source-1password-vault "Private")

;; turn off tabs
(setq-default indent-tabs-mode nil)
;; set indents to 4
(setq-default tab-width 4)
(setq-default require-final-newline "visit-save")

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
(setq doom-theme 'doom-tokyo-night)

;; Modeline settings
(custom-set-faces!
  '(mode-line :family "GohuFont 14 Nerd Font")
  '(mode-line-inactive :family "GohuFont 14 Nerd Font"))
(setq doom-modeline-modal-icon t
      doom-modeline-hud t)
(setq doom-font (font-spec :family "mononoki Nerd Font" :size 24 )
      doom-variable-pitch-font (font-spec :family "mononoki Nerd Font" :size  24)
      doom-big-font (font-spec :family "DroidSansM Nerd Font" :size 40))

;; open weblinks in emacs
(setq +lookup-open-url-fn #'eww)

(set-docsets! 'haskell-mode "Haskell")
(use-package lsp-mode
  :ensure t
  :hook ((haskell-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(add-load-path! "lisp")
(autoload 'refill-mode "refill" "Refill minor mode." t)

;; Projectile
(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "/nix/store*")
  (setq
   projectile-project-search-path '(("~/code" . 2))
   projectile-auto-discover "true"
   ))

;; Magit
(setq magit-repository-directories `(("~/code". 5)))
(setq straight-vc-git-default-protocol "ssh")

(setq forge-owned-accounts '(("oliverpauffley" nil)))

;; just file
(use-package! justl
  :config
  (map! :n "e" 'justl-exec-recipe))

;; Leetcode
(setq leetcode-prefer-language "rust")
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/code/leetcode/src")

;; Exercism
(require 'exercism)
(setq exercism-display-tests-after-run 't)

;; Slack
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "Utility-Warehouse"
   :default t
   :token (auth-source-pick-first-password
           :host "utilitywarehouse.slack.com"
           :user "opauffley@uw.co.uk")
   :cookie (auth-source-pick-first-password
            :host "utilitywarehouse.slack.com"
            :user "opauffley@uw.co.uk^cookie")
   :full-and-display-names t
   :subscribed-channels '(team-energy-sfe development-tech))

  (evil-define-key 'normal slack-info-mode-map
    ",u" 'slack-room-update-messages)
  (evil-define-key 'normal slack-mode-map
    ",c" 'slack-buffer-kill
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete
    ",u" 'slack-room-update-messages
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message)
  (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel))
(after! slack
  (url-cookie-store "d" (auth-source-pick-first-password :host "utilitywarehouse.slack.com" :user "opauffley@uw.co.uk^cookie") nil ".slack.com" "/" t)
  )



;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Go settings
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; Rust Settings
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (add-to-list 'auto-mode-alist '("\\.ron\\'" . rustic-mode))
  (setq lsp-rust-analyzer-inlay-hints-mode "true")
  )

(use-package! ron-mode
  :defer t
  :mode (("\\.ron\\'" . ron-mode)))

;; Haskell settings
(after! haskell-mode
  (setq haskell-stylish-on-save t))

;; irc
(set-irc-server! "irc.libera.chat"
  '(:tls t
    :port 6697
    :nick "olliep"
    :sasl-password my-nickserver-password
    :sasl-password (auth-source-pick-first-password :host "Libera" :user "password")
    :channels ("#haskell-beginners")))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


(add-hook! 'elfeed-search-mode-hook 'elfeed-update)
(setq rmh-elfeed-org-files ' ("./rss.org"))
(map! :leader
      (:prefix "o"
       :desc "Elfeed" "E" 'elfeed))

;; set org capture templates
(after! org
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-src-block-backend 'minted)

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
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
     ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5)
        org-table-convert-region-max-lines 10000)
  ;; src block indentation / editing / syntax highlighting
  (setq org-src-fontify-natively t
        org-src-window-setup 'current-window ;; edit in current window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t ;; do not put two spaces on the left
        org-src-tab-acts-natively t)
  (require 'ox-hugo)
  (require 'ox-zola)
  )

;; Emails
(set-email-account! "mrpauffley@gmail.com"
                    '((mu4e-sent-folder       . "/[Gmail].Sent Mail")
                      (mu4e-drafts-folder     . "/[Gmail].Drafts")
                      (mu4e-trash-folder      . "/[Gmail].Bin")
                      (mu4e-refile-folder     . "/[Gmail].All Mail")
                      (smtpmail-smtp-user . "mrpauffley@gmail.com")
                      (smtpmail-smtp-server . "smtp.gmail.com")
                      (user-mail-address . "mrpauffley@gmail.com")
                      (mu4e-compose-signature . "---\nOliver Pauffley"))t)

;; tell message-mode how to send mail
(setq message-send-mail-function 'smtpmail-send-it)

;; set shell
(setq vterm-shell "fish")
(setq explicit-shell-file-name "fish")

;; nix mode
(add-hook 'nix-mode-hook #'lsp)

;; org dnd export
(require 'ox-dnd)


;; Linear
(setq linear-auth-token (auth-source-pick-first-password :host "api.linear.app"))

;; chatgpt shell
(setq chatgpt-shell-openai-key (auth-source-pick-first-password :host "chatgpt.api"))

;; show images in the correct orientations
(with-eval-after-load 'image-dired
  (add-to-list 'image-dired-cmd-create-thumbnail-options "-auto-orient")
  (add-to-list 'image-dired-cmd-create-temp-image-options "-auto-orient")
  (add-to-list 'image-dired-cmd-create-standard-thumbnail-options
               "-auto-orient"))


;; Smartparens bindings set to be called with SPC + l as prefix
(map!
 :map smartparens-mode-map
 :leader (:prefix ("l" . "Lisps")
          :nvie "f" #'sp-next-sexp
          :nvie "b" #'sp-forward-barf-sexp
          :nvim "u" #'sp-unwrap-sexp
          :nie "k" #'sp-kill-sexp
          :nie "s" #'sp-split-sexp
          :nie "l" #'sp-forward-slurp-sexp
          :nie "h" #'sp-backward-slurp-sexp
          :nie "y" #'sp-copy-sexp
          :nie "(" #'sp-wrap-round
          :nie "[" #'sp-wrap-square
          :nie "{" #'sp-wrap-curly))
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
