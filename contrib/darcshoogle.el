; an example of hoogle search integration in emacs haskell mode
; load from or add to your .emacs

(require 'haskell-mode)
(define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
(define-key haskell-mode-map "\C-cd" 'darcs-hoogle)

; to show results in emacs, provide the full path of darcs/tools/darcshoogle:
;(setq darcs-hoogle-command (expand-file-name "~/src/darcs/tools/darcshoogle"))
; or leave it nil to show results in a web browser:
(setq darcs-hoogle-command nil)

(setq darcs-hoogle-url "http://joyful.com/repos/darcs-unstable/hoogle")

; a copy of haskell-hoogle that calls the darcs web or command-line hoogle
(defun darcs-hoogle (query)
  "Do a Hoogle search for QUERY in darcs code."
  (interactive
   (let ((def (haskell-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Darcs hoogle query (default %s): " def)
                          "Darcs hoogle query: ")
                        nil nil def))))
  (if (null darcs-hoogle-command)
      (browse-url (format (concat darcs-hoogle-url "/?q=%s") query))
    (if (fboundp 'help-setup-xref)
        (help-setup-xref (list 'darcs-hoogle query) (interactive-p)))
    (with-output-to-temp-buffer
        (if (fboundp 'help-buffer) (help-buffer) "*Help*")
      (with-current-buffer standard-output
        (start-process "darcshoogle" (current-buffer) darcs-hoogle-command
                       query)))))

