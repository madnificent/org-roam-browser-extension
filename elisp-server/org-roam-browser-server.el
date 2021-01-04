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
(defun org-roam-server-handler (request)
  (with-slots (process headers) request
    (ws-response-header process 200 '("Content-type" . "application/json") '("Access-Control-Allow-Origin" . "*"))
    (process-send-string
     process
     (let ((page-exists
            (org-roam-db-query
             [:select file :from refs
                      :where (= ref $v1)]
             (vector (cdr (assoc "url" headers)))))
           (page-referenced
            (org-roam-db-query [:select source :from links :where (= links:dest $s1)]
                               (cdr (assoc "url" headers)))))
      (concat
       "{\"pageExists\": " (if page-exists "true" "false") ",\n"
       " \"linkExists\": " (if page-referenced "true" "false") "}")))))
;; The elisp server:3 ends here

;; [[file:../README.org::*The elisp server][The elisp server:4]]
(provide 'org-roam-browser-server)
;;; org-roam-browser-server.el ends here
;; The elisp server:4 ends here
