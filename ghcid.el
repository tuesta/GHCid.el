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

;;; * Configuration variables
(defcustom ghcid-command "ghcid --no-status --color=always --command \"cabal repl $1--repl-options=-ferror-spans --repl-options=-fdiagnostics-color=always\" --allow-eval"
  "El comando básico para ejecutar ghcid."
  :type 'string
  :group 'ghcid)

;;; * Detect project
(defun ghcid-get-project-root ()
  "Obtiene la raíz del proyecto usando project.el o el directorio actual."
  (if-let ((pr (project-current)))
      (expand-file-name (project-root pr))
    default-directory))

;;; * GHCid
(defvar-local ghcid-state 'loading)

(defun ghcid--filter-loading (proc string)
  (let ((sentinel-regexp "\\(?:Ok\\|Failed\\), \\(?:.*modules? loaded\\|unloaded all modules\\)\\."))
    (if (string-match sentinel-regexp string)
        (let ((post (substring string (match-end 0))))
          (setq ghcid-state 'reloading)
          (erase-buffer)
          (set-marker (process-mark proc) (point-min))

          (unless (string-empty-p (string-trim post))
            (ghcid--filter-reloading proc post)))

      ;; Versión ultra-simple para color
      (let ((start (process-mark proc)))
        (insert (ansi-color-apply string))
        (set-marker (process-mark proc) (point))))))

(defun ghcid--filter-reloading (proc string)
  (let ((inhibit-read-only t))
    ;; Detección de reinicio (OSC 0)
    (when (string-match "\e]0;" string)
      (erase-buffer)

      ;; ========== LIMPIEZA COMPLETA ==========
      ;; Estoy borrando todo lo que se pueda borra
      ;; probablemente muchas cosas no son necesarias
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
        (setq overlay-arrow-position nil))
      ;; ========== LIMPIEZA COMPLETA ==========

      (set-marker (process-mark proc) (point-min)))

    ;; Limpieza de basura de terminal
    (setq string (replace-regexp-in-string "\e].*?[\e\\]" "" string))
    (setq string (replace-regexp-in-string "\\`[ \t\n\r\\]+" "" string))

    (unless (string-empty-p string)
      ;; INSERCIÓN MANUAL (Evitamos compilation-filter para que no rompa el color)
      (save-excursion
        (goto-char (process-mark proc))
        (insert (ansi-color-apply string))
        (set-marker (process-mark proc) (point)))

      ;;; Flycheck interaction
      ;; (dolist (buf (buffer-list))
      ;;   (with-current-buffer buf
      ;;     (when (and (derived-mode-p 'haskell-mode)
      ;;                (fboundp 'flycheck-mode)
      ;;                flycheck-mode)
      ;;       (flycheck-buffer))))

      ;; Forzar scroll y vista arriba
      (save-selected-window
        (let ((win (get-buffer-window (current-buffer) t)))
          (when win
            (with-selected-window win
              (goto-char (point-min))
              (set-window-start win (point-min))
              (set-window-point win (point-min)))))))))

(defun ghcid-main-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (pcase ghcid-state
          ('loading (ghcid--filter-loading proc string))
          ('reloading (ghcid--filter-reloading proc string)))))))

;; ==================================================
;;  COMPATIBLE: REGEX SPEC SIMPLE FORMAT
;; ==================================================

(defconst ghcid--file-line-col "^\\([^< \t\n\r:]+\\):\\([0-9]+\\):\\([0-9]+\\)")

(defconst ghcid--file-line-col-input (concat ghcid--file-line-col "\n\\(?:\\$>.*\n\\)*?<interactive>:[0-9]+:[0-9]+\\(?:-[0-9]+\\)?: "))

(defconst ghcid-eval-error (concat ghcid--file-line-col-input "error:"))
(defconst ghcid-eval-error-spec
  (list 'ghcid-eval-error ghcid-eval-error 1 2 3 2))

(defconst ghcid-eval-warning (concat ghcid--file-line-col-input "warning:"))
(defconst ghcid-eval-warning-spec
  (list 'ghcid-eval-warning ghcid-eval-warning 1 2 3 1))

(defconst ghcid-eval-info (concat ghcid--file-line-col "\n\\(?:\\$>.*\n\\)*\\($>.*\\)$"))
(defconst ghcid-eval-info-spec
  (list 'ghcid-eval-info ghcid-eval-info 1 2 3 0))

(defconst ghcid-error-no-col-end (concat ghcid--file-line-col ": error:"))
(defconst ghcid-spec-error-no-col-end
  (list 'ghcid-error-no-col-end ghcid-error-no-col-end 1 2 (cons 3 3) 2 nil))

(defconst ghcid-error-with-col-end (concat ghcid--file-line-col "-\\([0-9]+\\): error:"))
(defconst ghcid-spec-error-with-col-end
  (list 'ghcid-error-with-col-end ghcid-error-with-col-end 1 2 (cons 3 4) 2 nil))

(defconst ghcid-warning-no-col-end (concat ghcid--file-line-col ": warning:"))
(defconst ghcid-spec-warning-no-col-end
  (list 'ghcid-warning-no-col-end ghcid-warning-no-col-end 1 2 (cons 3 3) 1 nil))

(defconst ghcid-warning-with-col-end (concat ghcid--file-line-col "-\\([0-9]+\\): warning:"))
(defconst ghcid-spec-warning-with-col-end
  (list 'ghcid-warning-with-col-end ghcid-warning-with-col-end 1 2 (cons 3 4) 1 nil))

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

          (setq next-error-last-buffer (current-buffer)) ; Indica que este es el buffer de errores

          (setq-local compilation-error-regexp-alist-alist
                      (list ghcid-eval-error-spec ghcid-eval-warning-spec ghcid-eval-info-spec ghcid-spec-error-no-col-end ghcid-spec-error-with-col-end ghcid-spec-warning-no-col-end ghcid-spec-warning-with-col-end))

          (setq-local compilation-error-regexp-alist
                      '(ghcid-eval-error ghcid-eval-warning ghcid-eval-info ghcid-error-no-col-end ghcid-error-with-col-end ghcid-warning-no-col-end ghcid-warning-with-col-end))

          (setq-local next-error-function #'ghcid-next-error-wrapper)

          (setq ghcid-state 'loading)
          (set-process-filter (get-buffer-process outbuf) #'ghcid-main-filter)))))

(defun ghcid-run-if-not-running ()
  "Lanza la sesión de Ghcid si no hay una activa."
  (interactive)
  (let* ((buf-name "*ghcid*")
         (proc (get-buffer-process (get-buffer buf-name))))

    (unless (and proc (process-live-p proc)) (ghcid))))

(defun ghcid-next-error-wrapper (arg &optional reset)
  "Mueve el punto al error en el buffer de ghcid y salta al código fuente.
Resta 1 solo cuando RESET es activo para evitar saltarse el primer error."
  (interactive "p")
  (let ((n (if (and reset (> arg 0))
               (1- arg)
             arg)))
    (compilation-next-error-function n reset)))

;;; * Flycheck error
;;; ** Es más molesto que útil, aunque funciona correctamente
;;; *** TODO diferenciar entre warnings y errors
;; (defvar ghcid--error-regexp "^\\([^ \t\n\r:]+\\):\\([0-9]+\\):\\([0-9-]+\\):"
;;   "Regex interna para reconocer errores de GHCid.")
;; (flycheck-define-generic-checker 'ghcid-checker
;;   "Un checker de Flycheck que extrae errores del buffer de Ghcid."
;;   :start (lambda (_checker callback)
;;            (let ((errors nil)
;;                  (ghcid-buf (get-buffer "*ghcid*"))
;;                  ;; Capturamos el buffer que Flycheck está analizando ahora
;;                  (current-file-buffer (current-buffer)))
;;              (when (and ghcid-buf (buffer-file-name current-file-buffer))
;;                (with-current-buffer ghcid-buf
;;                  (save-excursion
;;                    (goto-char (point-min))
;;                    (while (re-search-forward ghcid--error-regexp nil t)
;;                      (let* ((file-path (match-string 1))
;;                             (line (string-to-number (match-string 2)))
;;                             (col-str (match-string 3))
;;                             (col (string-to-number (car (split-string col-str "-"))))
;;                             (msg (buffer-substring-no-properties (point) (line-end-position))))
;;
;;                        ;; Comparamos si el error pertenece al buffer que Flycheck está procesando
;;                        (when (string-suffix-p (file-name-nondirectory file-path)
;;                                               (buffer-file-name current-file-buffer))
;;                          (push (flycheck-error-new-at line col 'error (string-trim msg)
;;                                                       :checker 'ghcid-checker
;;                                                       :buffer current-file-buffer)
;;                                errors)))))))
;;              (funcall callback 'finished errors)))
;;
;;   :modes '(haskell-mode haskell-literate-mode))

;;; * REPl interaction
(defun ghcid-send-line (&optional custom-prefix)
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

(defun ghcid-send-selection (beg end)
  "Convierte la seleccion actual en un bloque de comandos."
  (interactive "r")
  (let* ((selection (buffer-substring-no-properties beg end))
         (start-offset (string-match "[^ \t\n\r]" selection))
         (end-offset (and start-offset
                          (string-match "[ \t\n\r]*\\'" selection start-offset))))
    (when start-offset
      (save-excursion
        (goto-char (+ beg end-offset))
        (let ((post-text-point (point)))
          (insert "\n<$ -}")
          (unless (looking-at "[ \t]*$")
            (insert "\n")))

        (goto-char (+ beg start-offset))
        (let ((pre-text-point (point)))
          (insert "{- $>\n")
          (save-excursion
            (goto-char pre-text-point)
            (unless (bolp)
              (insert "\n"))))))))

(provide 'ghcid)

;;; ghcid.el ends here
