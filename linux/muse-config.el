(require 'muse-mode)                   ; load authoring mode
(require 'muse-publish)
(require 'muse-html)          ; load publishing styles I use
;;(require 'muse-latex)
;;(require 'muse-texinfo)
;;(require 'muse-docbook)
;; (require 'info)
;; (setq Info-directory-list
;;       (cons (expand-file-name "/usr/local/emacs/info")
;;             Info-directory-list))
(require 'muse-project)

(defun replace-in-string (target old new &optional literal)
  (replace-regexp-in-string old new  target nil literal))

(require 'muse-wiki) ;;; Allow wiki-links
(setq muse-wiki-allow-nonexistent-wikiword t)
;;planner
(require 'planner)
(require 'planner-publish)
(require 'planner-id)
(setq planner-id-add-task-id-flag t)
(require 'planner-tasks-overview)
(require 'planner-cyclic)
(setq planner-cyclic-diary-nag nil)
(require 'remember)
(require 'remember-planner)
(setq remember-handler-functions '(remember-planner-append))
(setq remember-annotation-functions planner-annotation-functions)

(setq planner-project "WikiPlanner")

(setq muse-project-alist
      '(
        ("WikiPlanner"
         ("~/net9svn/plan" ;; where your Planner pages are located
          :default "TaskPool" ;; use value of `planner-default-page'
          :major-mode planner-mode
          :visit-link planner-visit-link)

         ;; This next part is for specifying where Planner pages
         ;; should be published and what Muse publishing style to
         ;; use.  In this example, we will use the XHTML publishing
         ;; style.
         (:base "planner-html"
                ;; where files are published to
                ;; (the value of `planner-publishing-directory', if
                ;;  you have a configuration for an older version
                ;;  of Planner)
                :path "~/net9svn/plan/public"))
        ("linux_notes"                 ; my various writings
         ("~/net9svn/muse"
          :default "index"
          :major-mode muse-mode
          :visit-link planner-visit-link)
         (:base "html"
                :path "~/net9svn/muse/public"))
        ("lessen_notes"                 ; notes from lessons
         ("/opt/English/iBT/notes"
          :default "index"
          :major-mode muse-mode)
         (:base "planner-html"
                :path "/opt/English/iBT/notes/public"))
        ))

(setq muse-file-extension nil
      muse-mode-auto-p t)

(setq debug-on-error t)

(setq muse-html-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"core/core.css\">")
(setq muse-html-charset-default "utf-8")
(setq muse-html-meta-content-encoding "utf-8")

(setq muse-mode-hook (quote (highlight-changes-mode record-last-changed)))

(setq muse-html-footer "<div class=\"navfoot\">
      <hr>
      <table summary=\"Footer navigation\" border=\"0\" width=\"100%\">
        <col width=\"33%\"><col width=\"34%\"><col width=\"33%\">
        <tbody><tr>
          <td align=\"left\">
            <span class=\"footdate\">Last Updated:
                <lisp>
                   (format-time-string \"%D %T\"
                       (nth 5 (file-attributes
                            muse-publishing-current-file)))
                </lisp>
          </span>
          </td>
          <td align=\"center\">

            <span class=\"foothome\">
              <a href=\"/\">Home</a> / <a href=\"index.html\">Index</a>
            </span>
          </td>
          <td align=\"right\">
			<a href=\"mailto:\"></a>
          </td>
        </tr>
      </tbody></table>
	 </div>")
