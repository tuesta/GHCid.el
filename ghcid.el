;;; ghcid.el --- GHCid support -*- lexical-binding: t -*-

;; Copyright (C) 2025 Tuesta
;; Author: Tuesta <tuesta@proton.me>
;; Version: 0.1.0
;; Created: 29 Dec 2025
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages, haskell, tools
;; URL: https://github.com/tuesta/GHCid.el

;;; Commentary:
;; Este paquete proporciona una integración minimalista con GHCid.
;; Gestiona la navegación de errores de forma fluida.
;; Use M-x ghcid

;;; Code:
(require 'compile)
(require 'project)
(require 'ansi-color)

(defgroup ghcid nil
  "Interface for GHCid in Emacs."
  :group 'languages
  :prefix "ghcid-")

;;; * Configuration variables
(defcustom ghcid-command "ghcid --no-status --height=53 --color=always --command \"cabal repl $1--repl-options=-ferror-spans --repl-options=-fdiagnostics-color=always\" --allow-eval"
  "El comando básico para ejecutar ghcid."
  :type 'string
  :group 'ghcid)

(defcustom ghcid-compilation-finish-functions nil
  "Lista de funciones a ejecutar cuando GHCid termina de mostrar la salida.
Cada función recibe como argumento el buffer del proceso ghcid."
  :type 'hook
  :group 'ghcid)

;;; * Detect project
(defun ghcid-get-project-root ()
  "Obtiene la raíz del proyecto usando project.el o el directorio actual."
  (if-let ((pr (project-current)))
      (expand-file-name (project-root pr))
    default-directory))

;;; * GHCid
(defvar-local ghcid--state 'loading)
(defvar-local ghcid--finish-timer nil)

(defun ghcid--filter-loading (proc string)
  (let ((sentinel-regexp "\\(?:Ok\\|Failed\\), \\(?:.*modules? loaded\\|unloaded all modules\\)\\."))
    (if (string-match sentinel-regexp string)
        (let ((post (substring string (match-end 0))))
          (setq ghcid--state 'reloading)
          (erase-buffer)
          (set-marker (process-mark proc) (point-min))

          (unless (string-empty-p (string-trim post))
            (ghcid--filter-reloading proc post)))

      (let ((start (process-mark proc)))
        (insert (ansi-color-apply string))
        (set-marker (process-mark proc) (point))))))

(defun ghcid--filter-reloading (proc string)
  (let ((inhibit-read-only t))
    ;; Detección de reinicio (OSC 0)
    (when (string-match "\e]0;" string)
      (erase-buffer)
      (set-marker (process-mark proc) (point-min)))

    ;; Limpieza de basura de terminal
    (setq string (replace-regexp-in-string "\e].*?[\e\\]" "" string))
    (setq string (replace-regexp-in-string "\\`[ \t\n\r\\]+" "" string))

    (unless (string-empty-p string)
      ;; cancel-timer if more data is still being processed
      (when ghcid--finish-timer
        (cancel-timer ghcid--finish-timer))

      ;; MANUAL INSERTION: We bypass `compilation-filter` to prevent it from
      ;; interfering with ANSI colors or triggering the parser prematurely.
      (save-excursion
        (goto-char (process-mark proc))
        (insert (ansi-color-apply string))
        (set-marker (process-mark proc) (point)))

      ;; (Re)start the timer. We use a 0-second timer to defer the execution
      ;; until the next iteration of the Emacs event loop.
      (setq ghcid--finish-timer
            (run-with-timer 0 nil
                            (lambda (b)
                              (when (buffer-live-p b)
                                (with-current-buffer b
                                  (ghcid--trigger-atomic-scan b)
                                  (setq ghcid--finish-timer nil)
                                  (run-hook-with-args 'ghcid-compilation-finish-functions b))))
                            (process-buffer proc)))

      (save-selected-window
        (let ((win (get-buffer-window (current-buffer) t)))
          (when win
            (with-selected-window win
              (goto-char (point-min))
              (set-window-start win (point-min))
              (set-window-point win (point-min)))))))))

(defun ghcid--clean()
  ;; TODO debería investigar cuáles son necesarias
  (when (fboundp 'compilation-forget-errors)
    (compilation-forget-errors))

  ;; Resetear contadores de errores
  (setq-local compilation-num-errors-found 0)
  (setq-local compilation-num-warnings-found 0)
  (setq-local compilation-num-infos-found 0)

  ;; Limpiar marcadores y estructuras de compilation
  (setq-local compilation-current-error nil)
  (setq-local compilation-last-error (copy-marker (point-min)))

  (setq-local compilation-messages-start (point-min))

  ;; Limpiar tabla hash de locations (si existe)
  (when (boundp 'compilation-locs)
    (setq-local compilation-locs (make-hash-table :test 'equal :weakness 'value)))

  ;; Limpiar caché de directorios (si existe)
  (when (boundp 'compilation--previous-directory-cache)
    (setq-local compilation--previous-directory-cache nil))

  ;; Resetear marcador de parsing (si existe)
  (when (and (boundp 'compilation--parsed) (markerp compilation--parsed))
    (move-marker compilation--parsed (point-min)))

  ;; Limpiar overlay de flecha (si existe)
  (when (and (boundp 'overlay-arrow-position) overlay-arrow-position)
    (setq overlay-arrow-position nil)))

(defun ghcid--trigger-atomic-scan (buffer)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (setq-local compilation-error-regexp-alist
                  '(ghcid-eval ghcid-no-col-end ghcid-with-col-end ghcid-range ghcid-eval-info))

      (ghcid--clean)

      (compilation-parse-errors (point-min) (point-max))
      (font-lock-flush)
      (font-lock-ensure))))

(defun ghcid-main-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (pcase ghcid--state
          ('loading (ghcid--filter-loading proc string))
          ('reloading (ghcid--filter-reloading proc string)))))))

;; ==================================================
;;  COMPATIBLE: REGEX SPEC SIMPLE FORMAT
;; ==================================================
(defconst ghcid--file-line-col "^\\([^< \t\n\r:]+\\):\\([0-9]+\\):\\([0-9]+\\)")

(defconst ghcid--warning-or-error "\\(?:\\(warning\\)\\|error\\):")

(defconst ghcid--file-line-col-input
  (concat ghcid--file-line-col "\n"
          "\\(?:\\$>.*\n\\)*?"
          "<interactive>:\\(?:[0-9:]+\\(?:-[0-9]+\\)?\\|([0-9,]+)-([0-9,]+)\\): "))

(defconst ghcid-eval (concat ghcid--file-line-col-input ghcid--warning-or-error))
(defconst ghcid-eval-spec
  (list 'ghcid-eval ghcid-eval 1 2 3 (cons 4 nil)))

(defconst ghcid-no-col-end (concat ghcid--file-line-col ": " ghcid--warning-or-error))
(defconst ghcid-spec-no-col-end
  (list 'ghcid-no-col-end ghcid-no-col-end 1 2 (cons 3 3) (cons 4 nil) nil))

(defconst ghcid-with-col-end (concat ghcid--file-line-col "-\\([0-9]+\\): " ghcid--warning-or-error))
(defconst ghcid-spec-with-col-end
  (list 'ghcid-with-col-end ghcid-with-col-end 1 2 (cons 3 4) (cons 5 nil) nil))

(defconst ghcid-range
  (concat "^\\([^< \t\n\r:]+\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\)): " ghcid--warning-or-error))
(defconst ghcid-spec-range
  (list 'ghcid-range ghcid-range 1 (cons 2 4) (cons 3 5) (cons 6 nil)))

(defconst ghcid-eval-info (concat ghcid--file-line-col "\n\\(?:\\$>.*\n\\)*\\($>.*\\)$"))
(defconst ghcid-eval-info-spec
  (list 'ghcid-eval-info ghcid-eval-info 1 2 3 0))

;;;###autoload
(defun ghcid ()
  "Lanza la sesión de GHCid, kill previous sesión if exists."
  (interactive)
  (let* ((buf-name "*ghcid*")
         (root (ghcid-get-project-root))
         (compilation-always-kill t))

    (when (get-buffer buf-name) (kill-buffer buf-name))

    (let* ((default-directory root)
          (outbuf (compilation-start ghcid-command 'compilation-mode (lambda (mode) buf-name))))
        (with-current-buffer outbuf

          (setq next-error-last-buffer (current-buffer))

          (setq-local compilation-error-regexp-alist-alist
                      (list ghcid-eval-spec ghcid-spec-no-col-end ghcid-spec-with-col-end ghcid-spec-range ghcid-eval-info-spec))

          ;; No error detection
          (setq-local compilation-error-regexp-alist nil)

          (setq-local next-error-function #'ghcid-next-error-wrapper)

          (setq ghcid--state 'loading)
          (set-process-filter (get-buffer-process outbuf) #'ghcid-main-filter)))))

(defun ghcid-run-if-not-running ()
  "Lanza la sesión de Ghcid si no hay una activa."
  (interactive)
  (let* ((buf-name "*ghcid*")
         (proc (get-buffer-process (get-buffer buf-name))))

    (unless (and proc (process-live-p proc)) (ghcid))))

(defun ghcid-next-error-wrapper (arg &optional reset)
  "Si RESET es no-nil, intenta primero con el error en la posición actual (0)
(lo correcto cuando GHCid muestra errores de archivos directamente).
Si esto falla  reintenta navegando al siguiente error disponible
(lo correcto cuando empieza con un 'All good' seguido de un error de <interactive>)."
  (interactive "p")
  (let ((n (if (and reset (> arg 0))
               (1- arg)
             arg)))
    (condition-case nil
        (compilation-next-error-function n reset)
      ((error user-error)
       (if (and reset (= arg 1))
           (compilation-next-error-function arg reset)
         (message "GHCid: No hay más errores"))))))

;;; * REPl interaction
(defun ghcid-mark-line-for-eval (&optional custom-prefix)
  "Convierte la línea actual en un comando de Ghcid."
  (interactive)
  (let* ((prefix (or custom-prefix "-- $>"))
         (lprefix (+ (length prefix) 1))
         (line-content (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (indent (if (string-match "^\\([[:space:]]*\\)" line-content)
                     (match-string 1 line-content)
                   ""))
         (trimmed-line (string-trim-left line-content))
         (has-prefix (string-prefix-p prefix trimmed-line))
         (current-col (current-column)))

    (save-excursion
      (delete-region (line-beginning-position) (line-end-position))
      (if has-prefix
          (let ((original-code (string-trim-left (substring trimmed-line (length prefix)))))
            (insert indent prefix " " original-code))

        (insert indent prefix " " trimmed-line)))

    (let ((min-cursor (+ (length indent) lprefix)))
      (if has-prefix
          (move-to-column (max current-col min-cursor) t)
        (move-to-column (max (+ current-col lprefix) min-cursor) t)))
    ))

(defun ghcid-mark-region-for-eval (beg end)
  "Convierte la seleccion actual en un bloque de comandos."
  (interactive "r")
  (let* ((selection (buffer-substring-no-properties beg end))
         (start-offset (string-match "[^ \t\n\r]" selection))
         (end-offset (and start-offset
                          (string-match "[ \t\n\r]*\\'" selection start-offset)))
         (char-beg (+ beg (or start-offset 0)))
         (real-beg (copy-marker (save-excursion
                                  (goto-char char-beg)
                                  (skip-chars-backward " \t")
                                  (point))))
         (real-end (copy-marker (+ beg (or end-offset (length selection))) t))
         (indent-count (save-excursion
                (goto-char real-beg)
                (beginning-of-line)
                (- real-beg (point))))
         (indent-spaces (make-string (max 0 (1- indent-count)) ?\s)))

    (save-excursion
      (goto-char real-end)
      (insert "\n<$ -}")
      (unless (eolp) (insert "\n"))
      (insert indent-spaces)

      (goto-char real-beg)
      (insert "\n{- $>\n" indent-spaces (if (= indent-count 0) "" " ")))

    (set-marker real-beg nil)
    (set-marker real-end nil)))

(provide 'ghcid)

;;; ghcid.el ends here
