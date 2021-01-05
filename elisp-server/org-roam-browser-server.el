;; [[file:../README.org::*The elisp server][The elisp server:1]]
;;; org-roam-browser-server -- A package providing information to the browser on what you have stored in org-roam.

;;; Commentary:
;;;
;;; More information at https://github.com/madnificent/org-roam-browser-server.git

;;; Code:
;; The elisp server:1 ends here

;; [[file:../README.org::*The elisp server][The elisp server:2]]
(ws-start
 'org-roam-server-handler
 10001)
;; The elisp server:2 ends here

;; [[file:../README.org::*The elisp server][The elisp server:3]]
(defun org-roam-browser-server--sub-urls (url)
  "Generate a list of sub-urls from URL."
  (when (string-prefix-p "//" url)
    (remove
     "//"
     (reduce (lambda (acc val)
               (let ((start (first acc)))
                 `(,(concat start val "/")
                   ,(concat start val)
                   ,@acc)))
             (split-string (string-trim url "//") "/" "")
             :initial-value '("//")))))
;; The elisp server:3 ends here

;; [[file:../README.org::*The elisp server][The elisp server:4]]
(defun org-roam-browser-server--reference-exists-as-key (&rest references)
  "Verify if any of REFERENCES is known in org-roam."
  (org-roam-db-query
   [:select file :from refs
    :where ref :in $v1]
   (apply #'vector references)))

(defun org-roam-browser-server--reference-exists-as-link (&rest references)
  "Verify if any of REFERENCES is referred to in org-roam."
  (org-roam-db-query
   [:select source
    :from links
    :where links:dest :in $v1]
   (apply #'vector references)))
;; The elisp server:4 ends here

;; [[file:../README.org::*The elisp server][The elisp server:5]]
(defun org-roam-server-handler (request)
  (with-slots (process headers) request
    (condition-case ex
        (let ((process-response
               (let ((url (cdr (assoc "url" headers))))
                 (let ((page-exists (org-roam-browser-server--reference-exists-as-key url))
                       (page-referenced (org-roam-browser-server--reference-exists-as-link url))
                       (parent-known
                        (let ((parent-list (org-roam-browser-server--sub-urls url)))
                          (or (apply #'org-roam-browser-server--reference-exists-as-key parent-list)
                              (apply #'org-roam-browser-server--reference-exists-as-link parent-list)))))
                   (concat
                    "{\"pageExists\": " (if page-exists "true" "false") ",\n"
                    " \"linkExists\": " (if page-referenced "true" "false") ",\n"
                    " \"parentKnown\": " (if parent-known "true" "false") ",\n"
                    " \"bestLink\": \"" (or (first (first page-exists)) (first (first page-referenced)) (first (first parent-known)) "false") "\"}")
                    ))))
          (ws-response-header process 200 '("Content-type" . "application/json") '("Access-Control-Allow-Origin" . "*"))
          (process-send-string process process-response))
      ('error (backtrace)
              (ws-response-header process 500 '("Content-type" . "application/json") '("Access-Control-Allow-Origin" . "*"))
              (process-send-string process "{\"error\": \"Error occurred when fetching result\" }")))))

;; The elisp server:5 ends here

;; [[file:../README.org::*The elisp server][The elisp server:6]]
(provide 'org-roam-browser-server)
;;; org-roam-browser-server.el ends here
;; The elisp server:6 ends here
