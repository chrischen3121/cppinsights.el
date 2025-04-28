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

(defun cppinsights--prepare-output-buffer (buffer-name)
  "Prepare the output buffer for cppinsights results.
BUFFER-NAME is the name of the buffer to create."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (c++-mode)
      (setq buffer-read-only nil))
    buffer))

(defun cppinsights--display-buffer (buffer)
  "Display BUFFER in a side window."
  (if (fboundp '+popup-buffer)
      (+popup-buffer buffer `((side . right)
                              (window-width . ,cppinsights--window-width-percent)))
    (let ((display-buffer-alist
           '(("\\*cppinsights.*\\*"
              (display-buffer-reuse-window display-buffer-in-side-window)
              (side . right)
              (window-width . cppinsights--window-width-percent)
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

(defun cppinsights--process-sentinel (process event)
  "Handle the completion of the cppinsights process.
PROCESS is the process object, EVENT is the process event."
  (when (string= event "finished\n")
    (with-current-buffer (process-buffer process)
      (use-local-map (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map c++-mode-map)
                       (define-key map (kbd "q") 'kill-buffer-and-window)
                       map))
      (goto-char (point-min))
      (let ((win (get-buffer-window (current-buffer))))
        (when win
          (set-window-start win (point-min)))))))

;;;###autoload
(defun cppinsights-run ()
  "Run cppinsights on the current buffer and show results in a popup."
  (interactive)
  (cppinsights--check-and-save-buffer)
  
  (let* ((file-name (cppinsights--validate-file))
         (buffer-name (buffer-name))
         (insights-buffer-name (format "*cppinsights %s*" buffer-name))
         (buffer (cppinsights--prepare-output-buffer insights-buffer-name))
         (command (cppinsights--build-command file-name)))
    
    (cppinsights--display-buffer buffer)
    
    (make-process
     :name "cppinsights"
     :buffer buffer
     :command command
     :sentinel #'cppinsights--process-sentinel)))

(provide 'cppinsights)
;;; cppinsights.el ends here
