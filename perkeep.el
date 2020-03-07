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

;; (perkeep-search (perkeep-query-expression "tag:solln") `(lambda (data) (message "Result: %S" data)))
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
   ("expression" . ,(eval expression))))

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
       (mapc
        (lambda (blob-obj)
          (insert
           (cdr (assoc-string "blob" blob-obj))
           "\n"))
        (cdr (assoc-string "blobs" response)))
       (goto-char permanodes-start)
       (perkeep-mode)
       ))))

(defun perkeep-follow-permanode ()
  "Visit the permanode named on this line."
  (interactive)
  (message "TODO follow")
  )
  
(defvar perkeep-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "f" 'perkeep-follow-permanode)
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
  )

(provide 'perkeep)

;; (require 'perkeep)
;; (setf debug-on-error t)

;; (perkeep--default-ui-config `(lambda (data) (message "hello %S" data)))

;; (setq server (perkeep-client-config-server))
;; (cdr (assoc-string "url" server))

;; TODO (put 'funny-mode 'mode-class 'special)
