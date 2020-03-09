;; -*- lexical-binding: t -*-
;; A client for the perkeep server.
;;
(require 'request)

(setq perkeep--client-config-cache nil)

(defun perkeep-client-config ()
  "Returns the default perkeep client config."
  (interactive)
  (if perkeep--client-config-cache
      perkeep--client-config-cache
    (setq perkeep--client-config-cache (perkeep--fetch-client-config))))

(defun perkeep--fetch-client-config ()
  (json-read-file "~/.config/perkeep/client-config.json"))

(defun perkeep--client-config-server ()
  "Returns the default server from the perkeep client config."
  (interactive)
  (let (servers default-server auth)
    (setq servers (assoc-string "servers" (perkeep-client-config)))
    (setq default-server (cdr (-find (lambda (s) (assoc-string "default" s)) servers)))
    (setq auth (cdr (split-string (cdr (assoc-string "auth" default-server)) ":")))
    (list
     (cons "url" (cdr (assoc-string "server" default-server)))
     (cons "user" (car auth))
     (cons "password" (car (cdr auth))))))

(setq perkeep--default-ui-config-cache nil)

(defun perkeep--default-ui-config (callback)
  "Returns the configuration of the server's UI handler."
  (interactive)
  (if perkeep--default-ui-config-cache
      (funcall callback perkeep--default-ui-config-cache)
    (setq
     perkeep--default-ui-config-cache
     (perkeep--fetch-default-ui-config
      (lambda (ui-config)
        (setq perkeep--default-ui-config-cache ui-config)
        (funcall callback perkeep--default-ui-config-cache))))))

;; (perkeep--fetch-default-ui-config `(lambda (data) (message "hello %S" data)))
(defun perkeep--fetch-default-ui-config (callback)
  (perkeep--request "/ui/?camli.mode=config" callback))

;; (perkeep-search (perkeep-query-expression "tag:solln") `(lambda (data) (switch-to-buffer (generate-new-buffer "perkeep-debug")) (insert (json-encode data)) (json-pretty-print-buffer) (javascript-mode)))
(defun perkeep-search (query callback)
  "perkeep-search is the main entry point for performing a search
against the perkeep repository.

The function takes a query object which is the equivalent of the perkeep
query object. It must be an associate list with the properties defined
in the SearchQuery perkeep Go struct (see
perkeep.org/pkg/search/query.go within perkeep repository).

There the following builder functions to create query lists:
- `perkeep-query-expression'
"
  (perkeep--default-ui-config
   (lambda (ui-config)
     (perkeep--request
      (concat
       (cdr (assoc-string "searchRoot" ui-config))
       "camli/search/query")
      callback
      :data (json-encode query)))))

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

(cl-defun perkeep--download (blob-ref file-name callback)
  (perkeep--default-ui-config
   (lambda (ui-config)
     (perkeep--request
      (concat
       (cdr (assoc-string "downloadHelper" ui-config))
       blob-ref
       "/"
       file-name)
      callback
      :parser 'buffer-string))))

(cl-defun perkeep--request (path callback
                              &key
                              (data nil)
                              (parser 'json-read))
  (let (server user password)
    (setq server (perkeep--client-config-server))
    (setq user (cdr (assoc-string "user" server)))
    (setq password (cdr (assoc-string "password" server)))
    (request
     (concat
      (cdr (assoc-string "url" server))
      path)
     :data data
     :headers `(("Authorization" . ,(concat "Basic " (base64-encode-string (concat user ":" password)))))
     :parser parser
     ;; :sync "t"
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data))))))

(defun perkeep-find-permanode (expression)
  "Performs a perkeep serach for a given expression and shows the found permanodes in a newly created buffer"
  (interactive "sExpression: ")
  (perkeep-search
   (perkeep-query-expression expression)
   (lambda (response)
     (let (permanodes-buffer permanodes-start)
       (setq permanodes-buffer (generate-new-buffer "perkeep"))
       (switch-to-buffer permanodes-buffer)
       (insert expression "\n\n")
       (setq permanodes-start (point))
       (perkeep--insert-search-results response)
       (goto-char permanodes-start)
       (perkeep-mode)
       ))))

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
  (while (> level 0)
    (insert "\t")
    (setq level (1- level)))
  
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
    (setq camli-content (car (perkeep--get-permanode-attr "camliContent")))
    (if camli-content
        (perkeep--follow-permanode-camli-content
         permanode-ref
         (car (perkeep--get-permanode-attr "title"))
         "blob" ;; TODO determine actual file name from perkeep
         camli-content)
      (error "TODO implement visiting permanode members"))))

(defun perkeep--follow-permanode-camli-content (permanode-ref title file-name camli-content)
  (perkeep--download
   camli-content
   file-name
   (lambda (blob)
     (let (blob-buffer)
       (setq blob-buffer (generate-new-buffer (or title file-name)))
       (switch-to-buffer blob-buffer)
       (setq-local perkeep-permanode-ref permanode-ref)
       (setq-local perkeep-file-name file-name)
       (insert blob)
       (goto-char (point-min))))))

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

(defun perkeep--get-permanode-attr (key)
  "Returns the attribute values with a given key of the permanode below
point."
  (save-excursion
    (perkeep--move-to-permanode)
    (forward-line 1)
    (let (values current-key)
      (setq values ())
      (while
          (and
           (not (eobp))
           (string= "\t" (thing-at-point 'whitespace)))
        (setq current-key (substring (thing-at-point 'line) 1 -1))
        (forward-line 1)
        (while
            (and
             (not (eobp))
             (string= "\t\t" (thing-at-point 'whitespace)))
          (when (string= current-key key)
            (setq values (append (list (substring (thing-at-point 'line) 2 -1)) values))
            )
          (forward-line 1)
          )
        )
      values)))

(defvar perkeep-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "f" 'perkeep-follow-permanode)
    (define-key map "C-m" 'perkeep-follow-permanode)
    map)
  "Local keymap for perkeep mode buffers.")

(defun perkeep-mode ()
  (kill-all-local-variables)
  (use-local-map perkeep-mode-map)
  (setq major-mode 'perkeep-mode
	mode-name "perkeep"
	;; case-fold-search nil
	buffer-read-only t
	selective-display t) ; for subdirectory hiding
  (setq-local tab-width 2)
  )

(provide 'perkeep)

;; (require 'perkeep)
;; (setf debug-on-error t)

;; (perkeep--default-ui-config `(lambda (data) (message "hello %S" data)))

;; (setq server (perkeep-client-config-server))
;; (cdr (assoc-string "url" server))

;; TODO (put 'funny-mode 'mode-class 'special)
