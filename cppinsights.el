;;; cppinsights.el --- Integration with cppinsights tool -*- lexical-binding: t; -*-

;; Author: Chris Chen <chrischen@ignity.xyz>
;; Version: 0.1
;; Keywords: c++, tools, cppinsights
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/chrischen3121/cppinsights.el
;; SPDX-License-Identifier: Apache-2.0

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This package provides integration with the cppinsights command-line tool.
;; It allows you to run cppinsights on the current buffer and view the results
;; in a popup buffer.
;;
;; Usage:
;; Call `M-x cppinsights-run` when visiting a C++ file to analyze it with cppinsights.
;; The results will be displayed in a side window.

;;; Code:
(require 'cc-mode)

(defgroup cppinsights nil
  "Integration with cppinsights tool."
  :group 'cppinsights)

(defcustom cppinsights-program "insights"
  "The name or path of the cppinsights program."
  :type 'string
  :group 'cppinsights)

(defcustom cppinsights-clang-opts '("-O0" "-std=c++20")
  "Additional arguments to pass to clangs."
  :type '(repeat string)
  :group 'cppinsights)

(defvar cppinsights--window-width-percent 0.4
  "Width of the side window for displaying cppinsights results.")

(defun cppinsights--check-and-save-buffer ()
  "Check if buffer needs saving and prompt user to save if needed."
  (when (buffer-modified-p)
    (when (y-or-n-p "Buffer has unsaved changes. Save first? ")
      (save-buffer))))

(defun cppinsights--validate-file ()
  "Validate that current buffer is a C++ file with a filename."
  (let ((file-name (buffer-file-name)))
    (unless file-name
      (error "Buffer is not visiting a file"))
    
    (unless (string-match-p "\\.\\(cpp\\|cc\\|cxx\\|h\\|hpp\\|hxx\\)$" file-name)
      (error "Not a C++ file"))
    
    file-name))

(defun cppinsights--prepare-output-buffer (buffer-name &optional mode)
  "Prepare the output buffer for cppinsights results.
BUFFER-NAME is the name of the buffer to create.
MODE is the major mode to use (defaults to c++-mode)."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (funcall (or mode 'c++-mode))
      (setq buffer-read-only nil)
      ;; Set up key bindings
      (use-local-map (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map (current-local-map))
                       (define-key map (kbd "q") 'kill-buffer-and-window)
                       map)))
    buffer))

(defun cppinsights--display-buffer (buffer)
  "Display BUFFER in a side window."
  (if (fboundp '+popup-buffer)
      (+popup-buffer buffer `((side . right)
                              (window-width . ,cppinsights--window-width-percent)))
    (let ((display-buffer-alist
           `(("\\*cppinsights.*\\*"
              (display-buffer-reuse-window display-buffer-in-side-window)
              (side . right)
              (window-width . ,cppinsights--window-width-percent)
              (reusable-frames . visible)))))
      (display-buffer buffer)))
  (select-window (get-buffer-window buffer)))

(defun cppinsights--build-command (file-name)
  "Build the command to run cppinsights on FILE-NAME."
  (let* ((default-directory (file-name-directory file-name))
         (has-compile-commands (file-exists-p "compile_commands.json")))
    (if has-compile-commands
        (append (list cppinsights-program)
                (list file-name))
      (append (list cppinsights-program)
              (list file-name)
              '("--")
              cppinsights-clang-opts))))

(defun cppinsights--cleanup-buffer (buffer)
  "Safely kill BUFFER if it exists and is live."
  (when (and buffer (buffer-live-p buffer))
    (kill-buffer buffer)))

(defun cppinsights--handle-process-success (stdout-buffer stderr-buffer)
  "Handle successful cppinsights process.
Show STDOUT-BUFFER with C++ mode and clean up STDERR-BUFFER."
  (with-current-buffer stdout-buffer
    (goto-char (point-min)))
  (cppinsights--display-buffer stdout-buffer)
  
  ;; Clean up stderr buffer since we don't need it
  (cppinsights--cleanup-buffer stderr-buffer))

(defun cppinsights--handle-process-error (status source-buffer stdout-buffer stderr-buffer)
  "Handle failed cppinsights process.
STATUS is the process exit status.
SOURCE-BUFFER is the original buffer name.
STDOUT-BUFFER is the buffer with stdout content.
STDERR-BUFFER is the buffer with stderr content."
  (let ((error-buffer (cppinsights--prepare-output-buffer 
                       (format "*cppinsights error: %s*" source-buffer)
                       'compilation-mode)))
    ;; Add error information to the buffer
    (with-current-buffer error-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "C++ Insights process failed with status %d\n\n" status))
        ;; Add stderr content
        (when (buffer-live-p stderr-buffer)
          (let ((stderr-content (with-current-buffer stderr-buffer
                                  (buffer-string))))
            (unless (string-empty-p stderr-content)
              (insert stderr-content))))
        (goto-char (point-min))))
    
    ;; Display the error buffer
    (cppinsights--display-buffer error-buffer)
    
    ;; Clean up unneeded buffers
    (cppinsights--cleanup-buffer stdout-buffer)
    (cppinsights--cleanup-buffer stderr-buffer)))

(defun cppinsights--process-sentinel (process event)
  "Handle the completion of the cppinsights process.
PROCESS is the process object, EVENT is the process event."
  (let ((status (process-exit-status process))
        (stdout-buffer (process-buffer process))
        (stderr-buffer (process-get process 'stderr-buffer))
        (source-buffer (process-get process 'source-buffer)))
    
    (if (= status 0)
        (cppinsights--handle-process-success stdout-buffer stderr-buffer)
      (cppinsights--handle-process-error status source-buffer stdout-buffer stderr-buffer))))

;;;###autoload
(defun cppinsights-run ()
  "Run cppinsights on the current buffer and show results in a popup."
  (interactive)
  (cppinsights--check-and-save-buffer)
  
  (let* ((file-name (cppinsights--validate-file))
         (buffer-name (buffer-name))
         (stdout-buffer-name (format "*cppinsights %s*" buffer-name))
         (stderr-buffer-name (format "*cppinsights %s* stderr" buffer-name))
         (stdout-buffer (cppinsights--prepare-output-buffer stdout-buffer-name))
         (stderr-buffer (generate-new-buffer stderr-buffer-name))
         (command (cppinsights--build-command file-name))
         (proc nil))
    
    ;; Start the process (no buffer displayed initially)
    (setq proc (make-process
                :name "cppinsights"
                :buffer stdout-buffer
                :command command
                :stderr stderr-buffer
                :sentinel #'cppinsights--process-sentinel))
    
    ;; Store additional information for use in the sentinel
    (process-put proc 'stderr-buffer stderr-buffer)
    (process-put proc 'source-buffer buffer-name)))

(provide 'cppinsights)
;;; cppinsights.el ends here
