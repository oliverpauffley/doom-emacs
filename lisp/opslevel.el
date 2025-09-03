;;; lisp/opslevel.el -*- lexical-binding: t; -*-

(defun opslevel-update-directory (folder)
  "For the given FOLDER find all deployment files that need opslevel annotations"
  (interactive "DFolder: ")
  (let ((files (opslevel--files-in-folder folder)))
    (opslevel--update-directory files)))

(defun opslevel--select-tier ()
  "Gets user input to select the correct ops level tier."
  (let ((xlist '("tier_1" "tier_2" "tier_3" "tier_4")))
    (completing-read "pick tier:" xlist nil t)))

(defun opslevel--get-repository-name ()
  "Gets user input for the repository name."
  (read-string "repository name: "))

(defun opslevel--get-repository-url ()
  "Gets user input for the repository url."
  (read-string "repository url: " "https://github.com/utilitywarehouse/"))

(defun opslevel--get-description ()
  "Gets user input for the description."
  (read-string "description: "))

(defun opslevel--get-is-component ()
  "Gets Y/N if the deployment is a component of a larger system."
  (y-or-n-p "is-component: "))


(defun opslevel--check-string-p (searchstring)
  "Checks if given SEARCHSTRING exists in current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward searchstring nil t)))

(defun opslevel--repository-p ()
  "Checks if current buffer contains a repository annotation."
  (opslevel--check-string-p "app.uw.systems/repos"))

(defun opslevel--tier-p ()
  "Checks if current buffer contains a tier annotation."
  (opslevel--check-string-p "app.uw.systems/tier"))

(defun opslevel--description-p ()
  "Checks if current buffer contains a description annotation."
  (opslevel--check-string-p "app.uw.systems/description"))

(defun opslevel--has-annotations-p ()
  "Checks if the current buffer has either a
Statefulset or Deployment that needs annotations field"
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp (rx "kind: " (or "StatefulSet" "Deployment")
                               "\n"
                               "metadata:"
                               (*? anything)
                               "annotations:") nil t)))


(defun opslevel--files-in-folder (foldername)
  "Returns a list of all the files in a folder."
  (directory-files-recursively foldername ".*" nil nil nil))

(defun opslevel-connect-component-files (folder-name)
  "Checks the FOLDER-NAME for components that are not connected correctly
  and then adds them to the correct yaml files."
  (interactive "DFolder: ")
  (opslevel--is-component-no-deployment-files (opslevel--files-in-folder folder-name)))

(defun opslevel--is-component-no-deployment-files (files)
  "Filters a list of FILES for any that are component files
  that do not have a matching deployment within the set of FILES"
  (let ((baseDirectory files))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (if (opslevel--is-component-file-p)
            (let* ((names (opslevel--get-deployment-name))
                   (component-name (car names))
                   (deploy-name (car (cdr names))))
              (if deploy-name
                  (unless (cl-some (lambda (check-file)
                                     (opslevel--contains-deployment-name-p component-name check-file))
                                   baseDirectory)
                    (opslevel--insert-component-name deploy-name component-name)))))))))

(defun opslevel--construct-component-name (type deploy-name)
  "changes a TYPE and DEPLOY-NAME into a component name for opslevel."
  (format "k8s:%s:%s" type deploy-name))

(defun opslevel--insert-component-name (deploy-name component-name)
  "adds the DEPLOY-NAME annotated as COMPONENT-NAME
  to the file selected by the user"
  (let ((selected-file (read-file-name (format "Where should %s be added: " component-name)))
        (component-annotation (format "component.%s" deploy-name)))
    (with-current-buffer (find-file-noselect selected-file)
      (opslevel--insert-annotations component-annotation component-name)
      (save-buffer))))

(defun opslevel--contains-deployment-name-p (component-name file-name)
  "Checks if the FILE-NAME contains DEPLOY-NAME within it."
  (with-current-buffer (find-file-noselect file-name)
    (goto-char (point-min))
    (search-forward component-name nil t)))

(defun opslevel--get-deployment-name ()
  "For the current current buffer, get the deployment name"
  (save-excursion
    (goto-char (point-min))
    (when (string-match (rx "kind: " (group (or (literal "StatefulSet") (literal "Deployment"))))(buffer-string))
      (let ((type-name (downcase (match-string 1 (buffer-string)))))
        (search-forward-regexp (rx "kind: " (or "StatefulSet" "Deployment")) nil t)
        (search-forward "name:" nil t)
        (let* ((rawname (string-remove-prefix "&app " (string-trim (buffer-substring-no-properties (point) (line-end-position)))))
               (nameprefix (opslevel--get-name-prefix))
               (namesuffix (opslevel--get-name-suffix))
               (name (concat nameprefix rawname namesuffix)))
          (list (opslevel--construct-component-name type-name name) name))))))

(defun opslevel--get-name-prefix ()
  "From a file, finds the parent kustomize-file
and gets the name-prefix if one exists"
  (let* ((directory (file-name-parent-directory (buffer-file-name)))
         (kustomize-name (string-join (list directory "kustomization.yaml") "/")))
    (with-current-buffer (find-file-noselect kustomize-name)
      (goto-char (point-min))
      (if (search-forward "namePrefix: " nil t)
          (string-trim (buffer-substring-no-properties (point) (line-end-position)))
        ))))

(defun opslevel--get-name-suffix ()
  "From a file, finds the parent kustomize-file
and gets the name-suffix if one exists"
  (let* ((directory (file-name-parent-directory (buffer-file-name)))
         (kustomize-name (string-join (list directory "kustomization.yaml") "/")))
    (with-current-buffer (find-file-noselect kustomize-name)
      (goto-char (point-min))
      (if (search-forward "nameSuffix: " nil t)
          (string-trim (buffer-substring-no-properties (point) (line-end-position)))
        ))))

(defun opslevel--is-deployment-file-p ()
  "Checks if the current buffer is a deployment file."
  (opslevel--check-string-p "kind: Deployment"))

(defun opslevel--is-statefulset-file-p ()
  "Checks if the current buffer is a statefulset file."
  (opslevel--check-string-p "kind: StatefulSet"))

(defun opslevel--should-have-annotation-file-p ()
  "Checks if the current buffer should be annotated with opslevel annotations"
  (or
   (opslevel--is-deployment-file-p)
   (opslevel--is-statefulset-file-p)
   ))

(defun opslevel--is-component-file-p ()
  "Checks if the current buffer is a component file."
  (opslevel--check-string-p "app.uw.systems/is-component"))

(defun opslevel--needs-changes-p ()
  "checks if the current buffer needs any updated fields."
  (and
   (opslevel--should-have-annotation-file-p)
   (not (opslevel--is-component-file-p))
   (or (not (opslevel--repository-p))(not (opslevel--description-p)) (not (opslevel--tier-p)))
   ))


(defun opslevel--update-directory (files)
  ""
  (dolist (file files)
    (with-current-buffer (find-file-noselect file)
      (if (opslevel--needs-changes-p) (opslevel--update-file file)))))

(defun opslevel--update-file (filename)
  "Gets all the required values and inserts into the file FILENAME."
  (switch-to-buffer (find-file-noselect filename))
  (if (not (opslevel--is-component-file-p)) (let ((is-component (opslevel--get-is-component)))
                                              (if is-component (opslevel--insert-annotations "is-component" "true")
                                                (if (not (opslevel--description-p)) (let ((description (opslevel--get-description)))
                                                                                      (opslevel--insert-annotations "description" description)))
                                                (if (not (opslevel--tier-p)) (let ((tier (opslevel--select-tier)))
                                                                               (opslevel--insert-annotations "tier" tier)))
                                                (if (not (opslevel--repository-p)) (let ((repository (opslevel--get-repository-name))
                                                                                         (url (opslevel--get-repository-url)))
                                                                                     (opslevel--insert-annotations-repo repository url))))))
  (save-buffer))

(defun opslevel--insert-annotations (type annotations)
  "Edits the current buffer to insert the given TYPE ANNOTATIONS."
  (if (not (opslevel--has-annotations-p))
      (save-excursion
        (goto-char (point-min))
        (search-forward-regexp (rx "kind: " (or "StatefulSet" "Deployment")) nil t)
        (if (not (search-forward-regexp (rx line-start "metadata:") nil t))
            (progn (insert ?\n "metadata:" ?\n)
                   (insert "  annotations:"))
          )
        (if (not (search-forward-regexp (rx line-start "  annotations:") nil t))
            (insert ?\n "  annotations:"))))
  (if (not (string= "" annotations))
      (save-excursion
        (goto-char (point-min))
        (search-forward-regexp (rx "kind: " (or "StatefulSet" "Deployment")) nil t)
        (search-forward "  annotations:" nil t)
        (let* ((key (format "app.uw.systems/%s" type))
               (annotate (format "    \"%s\": \"%s\"" key annotations)))
          (opslevel--clear-key key)
          (newline)
          (insert annotate)))))

(defun opslevel--insert-annotations-repo (name url)
  "Edits the current buffer to insert the given TYPE ANNOTATIONS."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp (rx "kind: " (or "StatefulSet" "Deployment")) nil t)
    (search-forward "  annotations:" nil t)
    (let* ((key (format "app.uw.systems/repos.%s" name))
           (annotate (format "    \"%s\": \"%s\"" key url)))
      (opslevel--clear-key key)
      (newline)
      (insert annotate))
    ))

(defun opslevel--clear-key (key)
  "if the given annotation KEY already exists then delete in current buffer."
  (save-excursion
    (if (search-forward key nil t)
        (delete-line))))
