;; [[file:../README.org::*The elisp server][The elisp server:1]]
;;; org-roam-browser-server -- A package providing information to the browser on what you have stored in org-roam.

;;; Commentary:
;;;
;;; More information at https://github.com/madnificent/org-roam-browser-server.git

;;; Code:
;; The elisp server:1 ends here

;; [[file:../README.org::*Information requests][Information requests:1]]
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
;; Information requests:1 ends here

;; [[file:../README.org::*Information requests][Information requests:2]]
(defun org-roam-browser-server--reference-exists-as-key (&rest references)
  "Verify if any of REFERENCES is known in org-roam."
  (org-roam-db-query
   [:select file
    :from refs
    :where ref :in $v1]
   (apply #'vector references)))

(defun org-roam-browser-server--reference-exists-as-link (&rest references)
  "Verify if any of REFERENCES is referred to in org-roam."
  (org-roam-db-query
   [:select source
    :from links
    :where links:dest :in $v1]
   (apply #'vector references)))
;; Information requests:2 ends here

;; [[file:../README.org::*Information requests][Information requests:3]]
(defun org-roam-browser-server--info-handler (request)
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
                   (let ((best-link (or (first (first page-exists)) (first (first page-referenced)) (first (first parent-known)))))
                     (concat
                      "{\"pageExists\": " (if page-exists "true" "false") ",\n"
                      " \"linkExists\": " (if page-referenced "true" "false") ",\n"
                      " \"parentKnown\": " (if parent-known "true" "false") ",\n"
                      " \"bestLink\": " (if best-link
                                            (concat "\"" best-link "\"")
                                          "false")
                      "}"))
                    ))))
          (ws-response-header process 200 '("Content-type" . "application/json") '("Access-Control-Allow-Origin" . "*"))
          (process-send-string process process-response))
      ('error (backtrace)
              (ws-response-header process 500 '("Content-type" . "application/json") '("Access-Control-Allow-Origin" . "*"))
              (process-send-string process "{\"error\": \"Error occurred when fetching result\" }")))))
;; Information requests:3 ends here

;; [[file:../README.org::*Opening a file][Opening a file:1]]
(defun org-roam-browser-server--open-handler (request)
  (with-slots (process headers) request
    (condition-case ex
        (let ((page (cdr (assoc "page" headers))))
          (message "Opening file %s" page)
          (find-file-existing page)
          (ws-response-header process 200 '("Content-type" . "application/json") '("Access-Control-Allow-Origin" . "*"))
          (process-send-string process "{ \"success\": true }"))
      ('error (backtrace)
              (ws-response-header process 500 '("Content-type" . "application/json") '("Access-Control-Allow-Origin" . "*"))
              (process-send-string process "{\"error\": \"Error occurred when trying to open file\"}")))))
;; Opening a file:1 ends here

;; [[file:../README.org::*Booting up the server][Booting up the server:1]]
(ws-start
 '(((:GET . "/roam/info") . org-roam-browser-server--info-handler)
   ((:GET . "/roam/open") . org-roam-browser-server--open-handler))
 10001)
;; Booting up the server:1 ends here

;; [[file:../README.org::*Closing the sources][Closing the sources:1]]
(provide 'org-roam-browser-server)
;;; org-roam-browser-server.el ends here
;; Closing the sources:1 ends here
