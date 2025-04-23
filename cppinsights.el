;;; -*- lexical-binding: t; -*-
;;; cppinsights.el --- Integration with cppinsights tool

;; Author: Chris Chen
;; Keywords: c++, tools
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This package provides integration with the cppinsights command-line tool.
;; It allows you to run cppinsights on the current buffer and view the results
;; in a popup buffer.

;;; Code:

(defgroup cppinsights nil
  "Integration with cppinsights tool."
  :group 'tools
  :prefix "cppinsights-")

(defcustom cppinsights-command "insights"
  "The command to run cppinsights."
  :type 'string
  :group 'cppinsights)

(defcustom cppinsights-arguments '()
  "Additional arguments to pass to cppinsights."
  :type '(repeat string)
  :group 'cppinsights)

(defun cppinsights-run ()
  "Run cppinsights on the current buffer and show results in a popup."
  (interactive)
  (when (buffer-modified-p)
    (when (y-or-n-p "Buffer has unsaved changes. Save first? ")
      (save-buffer)))
  
  (let* ((file-name (buffer-file-name))
         (buffer-name (buffer-name))
         (insights-buffer-name (format "*cppinsights %s*" buffer-name))
         (buffer (get-buffer-create insights-buffer-name)))
    (unless file-name
      (error "Buffer is not visiting a file"))
    
    (unless (string-match-p "\\.\\(cpp\\|cc\\|cxx\\|h\\|hpp\\|hxx\\)$" file-name)
      (error "Not a C++ file"))
    
    (with-current-buffer buffer
      (erase-buffer)
      (c++-mode)
      (setq buffer-read-only nil))
    
    (let ((display-buffer-alist
           '(("\\*cppinsights.*\\*"
              (display-buffer-reuse-window display-buffer-in-side-window)
              (side . right)
              (window-width . 80)
              (reusable-frames . visible)))))
      (select-window (display-buffer buffer)))
    
    (let ((command (append (list cppinsights-command) 
                           cppinsights-arguments
                           (list file-name))))
      (make-process
       :name "cppinsights"
       :buffer buffer
       :command command
       :sentinel (lambda (process event)
                   (when (string= event "finished\n")
                     (with-current-buffer (process-buffer process)
                       (setq buffer-read-only t)
                       (use-local-map (let ((map (make-sparse-keymap)))
                                        (set-keymap-parent map c++-mode-map)
                                        (define-key map (kbd "q") 'kill-buffer-and-window)
                                        (define-key map (kbd "g") 'cppinsights-run)
                                        map))
                       (goto-char (point-min))
                       (let ((win (get-buffer-window (current-buffer))))
                         (when win
                           (set-window-start win (point-min)))))))))))

(defun cppinsights-close ()
  "Close the cppinsights results buffer."
  (interactive)
  (let ((buffers (cl-remove-if-not
                  (lambda (buf)
                    (string-match-p "\\*cppinsights .*\\*" (buffer-name buf)))
                  (buffer-list))))
    (dolist (buf buffers)
      (kill-buffer buf))))

;;;###autoload
(define-minor-mode cppinsights-mode
  "Minor mode for cppinsights integration."
  :lighter " CPPInsights"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c i r") 'cppinsights-run)
            (define-key map (kbd "C-c i c") 'cppinsights-close)
            map))

(provide 'cppinsights)
;;; cppinsights.el ends here
