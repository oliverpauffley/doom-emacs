;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
(package! protobuf-mode)
(package! earthfile-mode)
(package! go-impl)
(package! uuidgen
  :recipe (:host github :repo "kanru/uuidgen-el"))
(package! string-inflection)
(package! mermaid-mode)
(package! highlight)
(package! graphql-mode)
(package! graphql)
(package! ob-graphql :recipe (:host github :repo "jdormit/ob-graphql" :files ("ob-graphql.el")))
(package! org-d20)
(package! ox-dnd
  :recipe (:host github :repo "xeals/emacs-org-dnd" :files ("ox-dnd.el")))
(package! base16-theme)
(package! org-drill)
(package! csv-mode)
(package! slack
  :recipe (:host github :repo "Konubinix/emacs-slack"))
(package! just-mode)
(package! justl)
(package! haskell-snippets)
;; a package to learn transient
(package! transient-showcase
  :recipe (:host github :repo "positron-solutions/transient-showcase"))
(package! leetcode)
(package! exercism
  :recipe (:host github :repo "anonimitoraf/exercism.el" :files ("exercism.el")))
(package! ox-hugo)
(package! ox-zola :recipe (:host github :repo "gicrisf/ox-zola"))
(package! chatgpt-shell)
;; (use-package chatgpt-shell
;;   :requires shell-maker
;;   :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))))
