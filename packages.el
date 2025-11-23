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
(package! go-impl)
(package! uuidgen
  :recipe (:host github :repo "kanru/uuidgen-el"))
(package! mermaid-mode)
(package! highlight)
(package! graphql-mode)
(package! graphql)
(package! ob-graphql :recipe (:host github :repo "jdormit/ob-graphql" :files ("ob-graphql.el")))
(package! ob-grpc :recipe (:host github :repo "shsms/ob-grpc"))
(package! doric-themes)
(package! ef-themes)
(package! org-drill)
(package! csv-mode)
(package! just-mode)
;;a package to learn transient
(package! transient-showcase
  :recipe (:host github :repo "positron-solutions/transient-showcase"))
(package! exercism
  :recipe (:host github :repo "anonimitoraf/exercism.el" :files ("exercism.el")))
(package! gptel)
(package! feature-mode)
(package! org-dp
  :recipe (:host github :repo "tj64/org-dp"))
(package! pr-review)
(package! structurizr-mode :recipe (:host github :repo "gilesp/structurizr-mode"))
(package! consult-gh)
(package! consult-gh-embark)
(package! consult-gh-forge)
(package! evil-motion-trainer :recipe (:host github :repo "martinbaillie/evil-motion-trainer"))
(package! linear-emacs
  :recipe (:host github :repo "anegg0/linear-emacs" :files ("*.el")))
;; Play ironsworn in emacs
(package! emacs-rpgdm
  :recipe (:host gitlab :repo "howardabrams/emacs-rpgdm"))
(package! emacs-ironsworn
  :recipe (:host gitlab :repo "oliverpauffley/emacs-ironsworn" :branch "fix/org-entry-properties"))

;; record screencasts
(package! emacs-gif-screencast
  :recipe (:host gitlab :repo "ambrevar/emacs-gif-screencast"))
;; My packages
(package! vaarn
  :recipe (:host github :repo "oliverpauffley/vaarn.el"))
