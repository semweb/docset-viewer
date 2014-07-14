(eval-when-compile (require 'cl))
(require 'json)
(require 'helm)
(require 'w3m-load)

(defvar dv:default-docsets-directory
  "~/Library/Developer/Shared/Documentation/DocSets")

(defcustom dv:docsets-directory
  dv:default-docsets-directory
  "Docsets directory path")

(defcustom dv:open-w3m-other-buffer
  t
  "w3m open in other buffer")

(defun dv:ends-with (string suffix)
  (equalp (substring string (max 0 (- (length string) (length suffix))))
          suffix))

(defun dv:find-docsets (dir)
  (mapcar (lambda (docset)
            (format "%s/%s" dir docset))
          (sort* (remove-if-not (lambda (file)
                                  (dv:ends-with file ".docset"))
                                (directory-files dir))
                 '>
                 :key (lambda (docset)
                        (if (string-match "\\(iOS\\|OSX\\)\\([0-9]+\\).\\([0-9]+\\)" docset)
                            (let ((os (match-string 1 docset))
                                  (major (match-string 2 docset))
                                  (minor (match-string 3 docset)))
                              (+ (if (equalp os "iOS") 1000 0)
                                 (* (string-to-number major) 10)
                                 (string-to-number minor)))
                          0)))))

(defun dv:docset-db-path (docset)
  (format "%s%s"
          (expand-file-name docset)
          "/Contents/Resources/docSet.dsidx"))

(defcustom dv:selected-docsets
  (list (car (dv:find-docsets dv:docsets-directory)))
  "Selected docset paths")

(defun dv:add-comment-face (type)
  (if type
      (propertize type 'font-lock-face 'font-lock-keyword-face)
    ""))

(defun dv:buffer-to-display (swift)
  (goto-char (point-min))
  (while (re-search-forward "c:\\(objc(\\(cs\\|pl\\))\\|@\\)" nil t)
    (let* ((cs-pl-type (match-string 2)))
      (cond
       (cs-pl-type
        (replace-match "")
        (re-search-forward "\\([0-9a-z_]+?\\)\\((\\(cm\\|im\\|py\\))\\([0-9a-z_:]+\\)\\)?|\\([][0-9a-z_()!-<>?]+\\)" nil t)
        (let* ((im-py-type (match-string 3))
               (cs-pl (match-string 1))
               (im-py-objc (match-string 4))
               (im-py-swift (match-string 5)))
          (replace-match
           (cond
            (im-py-type
             (format "%s %s %s"
                     cs-pl
                     (dv:add-comment-face im-py-type)
                     (if swift
                         im-py-swift
                       im-py-objc)))
            (t
             (format "%s" cs-pl)))
           t)))
       ((match-string 0)
        (replace-match "")
        (re-search-forward "\\(\\(S\\|E\\|F\\|T\\|Ea\\|macro\\)@\\([0-9a-z_]+\\)\\(@\\([0-9a-z_]+\\)\\)?\\|\\([0-9a-z_]+\\)\\)|\\([][0-9a-z_,()!-<>?]+\\)" nil t)
        (let* ((type (match-string 2))
               (name (match-string 3))
               (e-name (match-string 5))
               (const-name (match-string 6))
               (swift-name (match-string 7)))
          (replace-match
           (cond
            (const-name const-name)
            (e-name
             (format "%s %s %s"
                     name
                     (dv:add-comment-face type)
                     (if swift
                         swift-name
                       e-name)))
            (t
             (format "%s %s"
                     (if swift
                         swift-name
                       name)
                     (dv:add-comment-face type))))
           t))))
      (re-search-forward "|\\([0-9a-z/_.]+\\)|\\([0-9a-z/_:]+\\)$" nil t)
      (let ((path (match-string 1))
            (anchor (match-string 2)))
        (add-text-properties (line-beginning-position)
                             (1+ (line-beginning-position))
                             `(path ,path anchor ,anchor)))
      (replace-match "")))
  (sort-lines nil (point-min) (point-max)))


(defvar dv:sql
  (mapconcat 'identity
             '("select t.ZTOKENUSR, t.ZTOKENNAME, fp.ZPATH, m.ZANCHOR"
               "from ZTOKEN t, ZFILEPATH fp, ZTOKENTYPE ty, ZTOKENMETAINFORMATION m, ZAPILANGUAGE l"
               "WHERE ty.Z_PK = t.ZTOKENTYPE"
               "AND t.ZLANGUAGE = l.Z_PK"
               "AND l.ZFULLNAME NOT LIKE 'Swift'"
               "AND fp.Z_PK = m.ZFILE"
               "AND t.ZMETAINFORMATION = m.Z_PK"
               "GROUP BY t.ZTOKENUSR"
               "ORDER BY t.ZTOKENNAME;")
             " "))

(defun dv:search-apis ()
  (interactive)
  (let* ((docset (car dv:selected-docsets))
         (docset-name (car (last (split-string docset "/")))))
    (helm-other-buffer
     `((name . ,docset-name)
       (init
        . (lambda ()
            (with-current-buffer (helm-candidate-buffer 'global)
              (if (and (boundp 'dv:candidates-cache) dv:candidates-cache)
                  (insert dv:candidates-cache)
                (progn
                  (call-process "sqlite3"
                                nil
                                (current-buffer)
                                nil
                                (expand-file-name (dv:docset-db-path docset))
                                dv:sql)
                  (dv:buffer-to-display nil)
                  (setq dv:candidates-cache
                        (buffer-substring (point-min) (point-max))))))))
       (get-line . dv:get-line)
       (candidates-in-buffer)
       (action . (("w3m" . dv:open-in-w3m)
                  ("Default Browser" . dv:open-in-default-browser)))
       (requires-pattern . 2))
     "*Docset Viewer*")))

(defun dv:open-in-w3m (x)
  (let (path anchor)
    (with-current-buffer helm-buffer
      (setq path (get-text-property (point-at-bol) 'path))
      (setq anchor (get-text-property (point-at-bol) 'anchor)))
    (let* ((db (dv:docset-db-path (car dv:selected-docsets)))
           (base (let (b)
                   (with-temp-buffer
                     (insert db)
                     (re-search-backward "/" nil t)
                     (goto-char (1+ (point)))
                     (kill-line)
                     (setq b (buffer-substring (point-min) (point-max))))
                   b))
           (url (format "file://%sDocuments/%s" base path)))
      (cond
       (dv:open-w3m-other-buffer
        (save-window-excursion
          (w3m-browse-url url nil)
          (get-buffer "*w3m*"))
        (save-selected-window
          (pop-to-buffer "*w3m*" t)
          (if anchor (w3m-search-name-anchor anchor))))
       (t
        (w3m-browse-url url nil))))))

(defun dv:open-in-default-browser (x)
  (let (path anchor)
    (with-current-buffer helm-buffer
      (setq path (get-text-property (point-at-bol) 'path))
      (setq anchor (get-text-property (point-at-bol) 'anchor)))
    (let* ((db (dv:docset-db-path (car dv:selected-docsets)))
           (base (let (b)
                   (with-temp-buffer
                     (insert db)
                     (re-search-backward "/" nil t)
                     (goto-char (1+ (point)))
                     (kill-line)
                     (setq b (buffer-substring (point-min) (point-max))))
                   b))
           (url (format "file://%sDocuments/%s" base path)))
      (browse-url url))))

(defun dv:get-line (s e)
  (buffer-substring s e))

(defun dv:clear-candidates-cache ()
  (interactive)
  (setq dv:candidates-cache nil))

(define-key global-map "\C-cd" 'dv:search-apis)

(provide 'docset-viewer)
