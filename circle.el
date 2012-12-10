;;; circle.el --- Tool for manipulating network of Emacs

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; circle.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; circle.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with circle.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'epcs)
(require 'dash)


;;; Utilities

(defun o:next-in-list (list elem)
  (cadr (--drop-while (not (eq it elem)) list)))

(defun o:previous-in-list (list elem)
  (car (last (--take-while (not (eq it elem)) list))))

(defun o:epc-live-p (mngr)
  (and
   (epc:manager-p mngr)
   (process-live-p
    (epc:connection-process (epc:manager-connection mngr)))))


;;; Served methods

(defun o:epc-manager-init (mngr)
  (epc:define-method mngr 'set-input-focus 'o:set-input-focus)
  (epc:define-method mngr 'load 'load))

(defun o:set-input-focus ()
  (select-frame-set-input-focus (selected-frame)))


;;; Server

(defvar o:server-port-file
  (expand-file-name ".circle-port" user-emacs-directory))

(defvar o:server-process nil
  "Process object for EPC server.")

(defvar o:clients nil)

(defun o:gc-clients ()
  "Throw away dead clients and rest `o:clients'."
  ;; If EPCS allows user to set "sentinel" for each connection there
  ;; will be no need for this function.
  (setq o:clients (-select #'o:epc-live-p o:clients)))

(defun o:start-server ()
  (unless (and (processp o:server-process) (process-live-p o:server-process))
    (setq o:server-process
          (epcs:server-start #'o:server-connect))
    (with-temp-buffer
      (erase-buffer)
      (insert (format "%d" (process-contact o:server-process :service)))
      (write-region (point-min) (point-max)
                    (expand-file-name o:server-port-file)))))

(defun o:stop-server ()
  (epcs:server-stop o:server-process)
  (setq o:server-process nil)
  (delete-file o:server-port-file)
  (setq o:clients nil))

(defun o:server-connect (mngr)
  (o:epc-manager-init mngr)
  (epc:define-method mngr 'call-on-next
                     (apply-partially #'o:server-call-on-next mngr))
  (epc:define-method mngr 'call-on-previous
                     (apply-partially #'o:server-call-on-previous mngr))
  (epc:define-method mngr 'call-on-all #'o:call-on-all--server)
  (push mngr o:clients))

(defun o:get-serving-method (mngr name)
  ;; FIXME: `epc:manager-get-method' is not a public function
  (epc:method-task (epc:manager-get-method mngr name)))

(defun o:server-call-on-next (mngr-peer name &optional args)
  (o:gc-clients)
  (let ((next (o:previous-in-list o:clients mngr-peer)))
    (if next
        (epc:call-deferred next name args)
      (apply (o:get-serving-method mngr-peer name) args))))

(defun o:server-call-on-previous (mngr-peer name &optional args)
  (o:gc-clients)
  (let ((prev (o:next-in-list o:clients mngr-peer)))
    (if prev
        (epc:call-deferred prev name args)
      (apply (o:get-serving-method mngr-peer name) args))))

(defun o:call-on-all--server (name &optional args)
  (o:gc-clients)
  (apply #'deferred:parallel
         (deferred:next
           (apply #'apply-partially
                  (o:get-serving-method (car o:clients) name)
                  args))
         (mapcar (lambda (mngr) (epc:call-deferred mngr name args))
                 o:clients)))

(defun o:next-node--server ()
  (o:gc-clients)
  (epc:call-deferred (car o:clients) 'set-input-focus nil))


;;; Client

(defvar o:client-epc nil)

(defun o:start-client ()
  (unless (o:epc-live-p o:client-epc)
    (setq o:client-epc
          (epc:start-epc "cat" (list o:server-port-file)))
    (o:epc-manager-init o:client-epc)))

(defun o:stop-client ()
  (epc:stop-epc o:client-epc)
  (setq o:client-epc nil))

(defun o:call-on-all--client (name &optional args)
  (epc:call-deferred o:client-epc 'call-on-all (list name args)))


;;; Compatible layer

(defun o:start-circle ()
  (if (file-exists-p o:server-port-file)
      (o:start-client)
    (o:start-server)))

(defun o:stop-circle ()
  (if o:client-epc
      (o:stop-client)
    (o:stop-server)))


;;; Public command/API

(defun circle-call-on-next (name &optional args)
  (o:gc-clients)
  (if o:client-epc
      (epc:call-deferred o:client-epc 'call-on-next (list name args))
    (epc:call-deferred (car (last o:clients)) name args)))

(defun circle-call-on-previous (name &optional args)
  (o:gc-clients)
  (if o:client-epc
      (epc:call-deferred o:client-epc 'call-on-previous (list name args))
    (epc:call-deferred (car o:clients) name args)))

(defun circle-call-on-all (name &optional args)
  (if o:client-epc
      (o:call-on-all--client name args)
    (o:call-on-all--server name args)))

(defun circle-focus-next-node ()
  (interactive)
  (circle-call-on-next 'set-input-focus))

(defun circle-focus-previous-node ()
  (interactive)
  (circle-call-on-previous 'set-input-focus))

(defun circle-reload-circle-el ()
  (interactive)
  (circle-call-on-all 'load (list "circle")))


;;; circle-mode

(defvar circle-mode-map (make-sparse-keymap))

(define-minor-mode circle-mode
  "Join to a circle."
  :global t
  :keymap circle-mode-map
  (if circle-mode
      (o:start-circle)
    (o:stop-circle)))

(let ((map circle-mode-map))
  (define-key map (kbd "C-c ' n") 'circle-focus-next-node)
  (define-key map (kbd "C-c ' p") 'circle-focus-previous-node)
  (define-key map (kbd "C-c ' R") 'circle-reload-circle-el))

(provide 'circle)

;;; circle.el ends here
