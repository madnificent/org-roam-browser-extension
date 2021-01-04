;;; org-roam-browser-server -- A package providing information to the browser on what you have stored in org-roam.

;;; Commentary:
;;;
;;; More information at https://github.com/madnificent/org-roam-browser-server.git

;;; Code:

(ws-start
 'org-roam-server-handler
 10001)

(defun org-roam-server-handler (request)
  (with-slots (process headers) request
    (ws-response-header process 200 '("Content-type" . "application/json") '("Access-Control-Allow-Origin" . "*"))
    (process-send-string
     process
     (let ((url (cdr (assoc "url" headers))))
       (let ((page-exists
              (org-roam-db-query
               [:select file :from refs
                        :where (= ref $v1)]
               (vector url)))
             (page-referenced
              (org-roam-db-query [:select source :from links :where (= links:dest $s1)]
                                 (vector url))))
         (concat
          "{\"pageExists\": " (if page-exists "true" "false") ",\n"
          " \"linkExists\": " (if page-referenced "true" "false") "}"))))))

(provide 'org-roam-browser-server)
;;; org-roam-browser-server.el ends here
