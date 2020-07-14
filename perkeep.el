;; -*- lexical-binding: t -*-
;; A client for the perkeep server.
;;
(require 'dash)
(require 'deferred)
(require 'request-deferred)

(setq perkeep--client-config-cache nil)

;; (perkeep-client-config)
(defun perkeep-client-config ()
  "Returns the default perkeep client config."
  (interactive)
  (if perkeep--client-config-cache
      perkeep--client-config-cache
    (setq perkeep--client-config-cache (perkeep--fetch-client-config))))

(defun perkeep--fetch-client-config ()
  (json-read-file "~/.config/perkeep/client-config.json"))

;; (perkeep--client-config-server)
(defun perkeep--client-config-server ()
  "Returns the default server from the perkeep client config."
  (interactive)
  (let (servers default-server auth)
    (setq servers (cdr (assoc-string "servers" (perkeep-client-config))))
    (setq default-server (cdr (-find (lambda (s) (not (eq (cdr (assoc-string "default" s)) json-false))) servers)))
    (setq auth (cdr (split-string (cdr (assoc-string "auth" default-server)) ":")))
    (list
     (cons "url" (cdr (assoc-string "server" default-server)))
     (cons "user" (car auth))
     (cons "password" (car (cdr auth))))))

(setq perkeep--default-ui-config-cache nil)

(defun perkeep--default-ui-config ()
  "Returns the configuration of the server's UI handler."
  (interactive)
  (if perkeep--default-ui-config-cache
      (deferred:$
        (deferred:next
          (lambda ()
            perkeep--default-ui-config-cache)))
    (setq
     perkeep--default-ui-config-cache
     (deferred:$
       (perkeep--fetch-default-ui-config)
       (deferred:nextc it
         (lambda (ui-config)
           (setq perkeep--default-ui-config-cache ui-config)
           perkeep--default-ui-config-cache))))))

;; (perkeep--fetch-default-ui-config `(lambda (data) (message "hello %S" data)))
(defun perkeep--fetch-default-ui-config ()
  (perkeep--request "/ui/?camli.mode=config"))

;; (perkeep-search (perkeep-query-expression "tag:solln") `(lambda (data) (switch-to-buffer (generate-new-buffer "perkeep-debug")) (insert (json-encode data)) (json-pretty-print-buffer) (javascript-mode)))
(defun perkeep-search (query)
  "perkeep-search is the main entry point for performing a search
against the perkeep repository.

The function takes a query object which is the equivalent of the perkeep
query object. It must be an associate list with the properties defined
in the SearchQuery perkeep Go struct (see
perkeep.org/pkg/search/query.go within perkeep repository).

There the following builder functions to create query lists:
- `perkeep-query-expression'
"
  (deferred:$
    (perkeep--default-ui-config)
    (deferred:nextc it
      (lambda (ui-config) 
        (perkeep--request
         (concat
          (cdr (assoc-string "searchRoot" ui-config))
          "camli/search/query")
         :data (json-encode query))))))

;; (perkeep-query-expression "is:image")
(defun perkeep-query-expression (expression)
  "Builds a perkeep query object from the given search expression. The
build query object is the input for the perkeep-search function.

Example query strings are:
  is:image
  tag:todo
  attr:year:2020"
  `(
    ("expression" . ,(eval expression))
    ("describe" .
     (
      ("rules" .
       (
        (
         ("attrs" . ("camliContent")))))))))

(cl-defun perkeep--download (blob-ref file-name)
  (deferred:$
    (perkeep--default-ui-config)
    (deferred:nextc it
      (lambda (ui-config)
        (perkeep--request
         (concat
          (cdr (assoc-string "downloadHelper" ui-config))
          blob-ref
          "/"
          file-name)
         :parser 'buffer-string)))))

(defun perkeep--upload-claim (form)
  (deferred:$
    (perkeep--default-ui-config)
    (deferred:nextc it
      (lambda (ui-config)
        (perkeep--request
         (concat
          (cdr (assoc-string "blobRoot" ui-config))
          "camli/upload")
         :files form)))))

(defun perkeep--upload-form (form)
  "Uploads the given alist form to perkeep. Invokes callback when done
with the content ref as argument."
  (deferred:$
    (perkeep--default-ui-config)
    (deferred:nextc it
      (lambda (ui-config)
        (perkeep--request
         (cdr (assoc-string "uploadHelper" ui-config))
         :files form)))
    (deferred:nextc it
      (lambda (response)
        (cdr (assoc-string "fileref" (elt (cdr (assoc-string "got" response)) 0)))))))

(defun perkeep--sign (claim)
  (deferred:$
    (perkeep--default-ui-config)
    (deferred:nextc it
      (lambda (ui-config)
        (perkeep--request
         (cdr (assoc-string "signHandler" (assoc-string "signing" ui-config)))
         :data (let ((json-encoding-pretty-print t)
                     (json-encoding-default-indentation "\t"))
                 (concat
                  "json="
                  (url-hexify-string
                   ;; stick to perkeeps unfixable weird claim json formatting
                   (concat
                    "{\"camliVersion\":1,\n"
                    (substring
                     (json-encode
                      (append
                       claim
                       `(("camliSigner" . ,(cdr (assoc-string "publicKeyBlobRef" (assoc-string "signing" ui-config)))))))
                     2
                     )))))
         :parser 'buffer-string)))))

(cl-defun perkeep--request (path
                            &key
                            (data nil)
                            (files nil)
                            (parser 'json-read))
  (let (server user password response-data)
    (setq server (perkeep--client-config-server))
    (setq user (cdr (assoc-string "user" server)))
    (setq password (cdr (assoc-string "password" server)))
    ;; (setq request-log-level 'debug)
    (deferred:$
      (request-deferred
       (concat
        (cdr (assoc-string "url" server))
        path)
       :data data
       :files files
       :headers `(("Authorization" . ,(concat "Basic " (base64-encode-string (concat user ":" password)))))
       :parser parser
       ;; :sync "t"
       )
      (deferred:nextc it
        (lambda (response)
          (request-response-data response))))))

;;;###autoload
(defun perkeep-find-permanode (expression)
  "Performs a perkeep serach for a given expression and shows the found permanodes in a newly created buffer"
  (interactive "sExpression: ")
  (deferred:$
    (perkeep-search
     (perkeep-query-expression expression))
    (deferred:nextc it
      (lambda (response)
        (let (permanodes-buffer permanodes-start)
          (setq permanodes-buffer (generate-new-buffer expression))
          (switch-to-buffer permanodes-buffer)
          (insert expression "\n\n")
          (setq permanodes-start (point))
          (perkeep--insert-search-results response)
          (goto-char permanodes-start)
          (perkeep-mode)
          )))))

(defun perkeep--insert-search-results (results)
  (mapc
   (lambda (blob-obj)
     (perkeep--insert-found-permanode (cdr (assoc-string "blob" blob-obj)) results))
   (cdr (assoc-string "blobs" results))))

(defun perkeep--insert-found-permanode (blob-ref results)
  (insert blob-ref "\n")
  (let ((description (perkeep--find-description-by-ref blob-ref results)))
    (setq description (cdr (assoc-string blob-ref (assoc-string "meta" (assoc-string "description" results)))))
    (when
        (not (string= "permanode" (cdr (assoc-string "camliType" description))))
      (error
       "Expected permanode response from server but got %S"
       (cdr (assoc-string "camliType" description))))
    (mapc
     (lambda (attrs)
       (let ((key (car attrs)))
         (insert (format-message "\t%S\n" key))
         (mapc
          (lambda (value)
            (perkeep--insert-permanode-attribute-value 2 key value results))
          (cdr attrs))
         ))
     (cdr (assoc-string "attr" (assoc-string "permanode" description))))
    ))

(defun perkeep--insert-permanode-attribute-value (level key value results)
  (perkeep--insert-permanode-value level value)
  (cond
   ((string= key "camliContent")
    (let ((file-description (perkeep--find-description-by-ref value results)))
      (perkeep--insert-permanode-value (1+ level) "file")
      (mapc
       (lambda (key)
         (let ((file-value (cdr (assoc-string key (assoc-string "file" file-description)))))
           (when file-value
             (perkeep--insert-permanode-value (+ level 2) key)
             (perkeep--insert-permanode-value (+ level 3) file-value))))
       '("fileName" "size" "mimeType"))))))

(defun perkeep--find-description-by-ref (blob-ref results)
  (cdr (assoc-string blob-ref (assoc-string "meta" (assoc-string "description" results)))))

(defun perkeep--insert-permanode-value (level value)
  (insert (make-string level ?\t))
  (insert
   (cond
    ((numberp value) (format-message "%S" value))
    (t value))
   "\n"))

(defun perkeep-follow-permanode ()
  "Visit the permanode named on this line.

This will open the permanode's content in a new buffer if it has a
camliContent attribute. Otherwise it will open a new search buffer which
shows the members of this permanode."
  (interactive)
  (let (permanode-ref camli-content)
    (setq permanode-ref (perkeep--get-permanode-ref))
    (setq camli-content (car (perkeep--get-permanode-attr '("camliContent"))))
    (if camli-content
        (perkeep--follow-permanode-camli-content
         permanode-ref
         (car (perkeep--get-permanode-attr '("title")))
         (car (perkeep--get-permanode-attr (list "camliContent" camli-content "file" "fileName")))
         camli-content)
      (error "TODO implement visiting permanode members"))))

(defun perkeep--follow-permanode-camli-content (permanode-ref title file-name camli-content)
  (deferred:$
    (perkeep--download
     camli-content
     "blob")
    (deferred:nextc it
      (lambda (blob)
        (let (blob-buffer)
          (setq blob-buffer (generate-new-buffer (or title file-name)))
          (switch-to-buffer blob-buffer)
          (insert blob)
          (set-buffer-modified-p nil)
          (goto-char (point-min))
          (setq-local buffer-file-name file-name)
          (normal-mode)
          (setq-local perkeep-permanode-ref permanode-ref)
          (perkeep-sourced-mode))))))

(defun perkeep-previous-permanode ()
  (interactive)
  (perkeep--move-to-permanode)
  (unless (bobp)
    (forward-line -1))
  (perkeep--move-to-permanode))

(defun perkeep-next-permanode ()
  (interactive)
  (beginning-of-line)
  (unless (eobp)
    (forward-line 1))
  (perkeep--collect-level-values 1)
  (recenter))

(defun perkeep--move-to-permanode ()
  "Moves point to the beginning of the current permanode"
  (beginning-of-line)
  (while
      (and
       (not (bobp))
       (string= "\t" (substring (thing-at-point 'whitespace) 0 1)))
    (forward-line -1))
  )

(defun perkeep--get-permanode-ref ()
  "Returns the permanode ref of the permanode below point."
  (save-excursion
    (perkeep--move-to-permanode)
    (substring (thing-at-point 'line) 0 -1)))

;; TODO rename function. maybe it makes sense to call the perkeep permanode attr property attr and the buffer's permanode values (which are a combinaion of permanote attr and file properties) attributes? there are some more functions which would benefit from a clear definiton of the names.
(defun perkeep--get-permanode-attr (key-chain)
  "Returns the attribute values with a given key of the permanode below
point."
  (save-excursion
    (perkeep--move-to-permanode)
    (forward-line 1)
    (let ((level 1))
      (mapc
       (lambda (key)
         (when
             (perkeep--find-permanode-attribute level key))
         (setq level (1+ level)))
       key-chain)
      (perkeep--collect-level-values level))))

(defun perkeep--find-permanode-attribute (level key)
  (let ((level-prefix (make-string level ?\t))
        current-key
        result)
    (while
        (and
         (not result)
         (not (eobp))
         (string= level-prefix (thing-at-point 'whitespace)))
      (setq current-key (substring (thing-at-point 'line) level -1))
      (forward-line 1)
      (if (string= current-key key)
          (setq result t)
        (perkeep--collect-level-values (1+ level))))
    result))

(defun perkeep--collect-level-values (level)
  "Collects values for a given level at point and walks over them."
  (let ((level-prefix (make-string level ?\t))
        current-key
        (values ()))
    (while
        (and
         (not (eobp))
         (string= level-prefix (substring (thing-at-point 'whitespace) 0 (min level (length (thing-at-point 'whitespace))))))
      (when (string= level-prefix (thing-at-point 'whitespace))
        (setq values (append values (list (substring (thing-at-point 'line) level -1)))))
      (forward-line 1))
    values))

(defvar perkeep-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "f" 'perkeep-follow-permanode)
    (define-key map "\C-m" 'perkeep-follow-permanode)
    (define-key map "p" 'perkeep-previous-permanode)
    (define-key map " " 'perkeep-next-permanode)
    (define-key map "n" 'perkeep-next-permanode)
    map)
  "Local keymap for perkeep mode buffers.")

(defgroup perkeep-faces nil
  "Faces used by perkeep."
  :group 'perkeep
  :group 'faces)

(defface perkeep-permanode-ref
  '((t (:inherit link)))
  "Face used for permanode refs."
  :group 'perkeep-faces
  :version "22.1")
(defvar perkeep-permanode-ref-face 'perkeep-permanode-ref
  "Face name used for permanode refs.")

(defface perkeep-permanode-attribute-key
  '((t (:inherit bold)))
  "Face used for permanode attribute keys."
  :group 'perkeep-faces
  :version "22.1")
(defvar perkeep-permanode-attribute-key-face 'perkeep-permanode-attribute-key
  "Face name used for permanode attribute keys.")

(defface perkeep-permanode-attribute-metadata
  '((t (:inherit shadow)))
  "Face used for extra metadata shown below permanode attribute values."
  :group 'perkeep-faces
  :version "22.1")
(defvar perkeep-permanode-attribute-metadata-face 'perkeep-permanode-attribute-metadata
  "Face name used for permanode attribute metadata.")

(defvar perkeep-font-lock-keywords
  (list
   (list "^sha[0-9]+-[a-z0-9]+$" '(0 perkeep-permanode-ref-face))
   (list "^\t[^\t].*$" '(0 perkeep-permanode-attribute-key-face))
   (list "^\t\t\t.*$" '(0 perkeep-permanode-attribute-metadata-face))
  ))

(defcustom perkeep-mode-hook nil
  "Run at the very end of `perkeep-mode'."
  :group 'perkeep
  :type 'hook)

;;;###autoload
(defun perkeep-mode ()
  "Mode for browsing perkeep search results.
Type \\[perkeep-follow-permanode] to visit the permanode below the
  cursor. Permanodes with a camliContent attribute will be fetched from
  perkeep and shown an another buffer. camliMember and camliPath
  attributes of a permanodes without a camliContent attribute will be
  searched for in another buffer.
Type \\[perkeep-previous-permanode] to move the cursor to the previous permanode.
Type \\[perkeep-next-permanode] to move the cursor to the next permanode."
  (kill-all-local-variables)
  (use-local-map perkeep-mode-map)
  (setq major-mode 'perkeep-mode
	mode-name "perkeep"
	;; case-fold-search nil
	buffer-read-only t
	selective-display t) ; for subdirectory hiding
  (setq-local tab-width 2)
  (setq-local font-lock-defaults
              '(perkeep-font-lock-keywords t nil nil beginning-of-line))
  (font-lock-ensure)
  (run-mode-hooks 'perkeep-mode-hook)
  )

(defun perkeep-save-buffer ()
  "Saves the current buffer into perkeep."
  (interactive)
  (deferred:$
    (perkeep--upload-form
     `(("ui-upload-file-helper-form" . (,(file-name-nondirectory buffer-file-name) :data ,(buffer-string)))))
    (deferred:nextc it
      (lambda (buffer-content-ref)
        (perkeep--sign
         (perkeep--set-attribute-claim perkeep-permanode-ref "camliContent" buffer-content-ref))))
    (deferred:nextc it
      (lambda (signed-claim)
        (let (claim-key)
          (setq claim-key (concat "sha224-" (secure-hash 'sha224 signed-claim)))
          (perkeep--upload-claim
           `((,claim-key . ("blob" :data ,(eval signed-claim))))))))
    (deferred:nextc it
      (lambda ()
        (set-buffer-modified-p ())))))

(defun perkeep--set-attribute-claim (permanode-ref key value)
  `(
    ("camliType" . "claim")
    ("permaNode" . ,(eval permanode-ref))
    ("claimType" . "set-attribute")
    ("claimDate" . ,(perkeep--now))
    ("attribute" . ,(eval key))
    ("value" . ,(eval value))
    ))

;; (perkeep--now)
(defun perkeep--now ()
  ;; TODO use correct milliseconds instead of 000
  (substring (shell-command-to-string "date -u +%Y-%m-%dT%H:%M:%S.000Z") 0 -1))

(defvar perkeep-sourced-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") 'perkeep-save-buffer)
    map)
  "Keymap for `perkeep-sourced-mode'.")

(define-minor-mode perkeep-sourced-mode
  "Saves the buffer into perkeep instead of into a file."
  (interactive (list (or current-prefix-arg 'toggle)))
  (let ((enable
         (if (eq arg 'toggle)
             (not perkeep-sourced-mode)
           (> (prefix-numeric-value arg) 0))))
    (if enable
        (perkeep-enable-perkeep-sourced-mode)
      (perkeep-disable-perkeep-sourced-mode)))
  :keymap perkeep-sourced-mode-map)

(defun perkeep-enable-perkeep-sourced-mode ()
  )

(defun perkeep-disable-perkeep-sourced-mode ()
  )
          
(provide 'perkeep)

;; (require 'perkeep)
;; (setf debug-on-error t)

;; (perkeep--default-ui-config `(lambda (data) (message "hello %S" data)))

;; (setq server (perkeep-client-config-server))
;; (cdr (assoc-string "url" server))

;; TODO (put 'funny-mode 'mode-class 'special)
