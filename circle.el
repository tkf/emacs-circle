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

(defun o:start-server ()
  (setq o:server-process
        (epcs:server-start #'o:server-connect))
  (with-temp-buffer
    (erase-buffer)
    (insert (format "%d" (process-contact o:server-process :service)))
    (write-region (point-min) (point-max)
                  (expand-file-name o:server-port-file))))

(defun o:stop-server ()
  (epcs:server-stop o:server-process)
  (setq o:server-process nil)
  (delete-file o:server-port-file)
  (setq o:clients nil))

(defun o:server-connect (mngr)
  (o:epc-manager-init mngr)
  (epc:define-method mngr 'call-on-next
                     (apply-partially #'o:server-call-on-next mngr))
  (epc:define-method mngr 'call-on-all #'o:call-on-all--server)
  (push mngr o:clients))

(defun o:server-call-on-next (mngr-peer name &optional args)
  (let ((next (cadr (--drop-while (not (eq it mngr-peer)) o:clients))))
    (if next
        (epc:call-deferred mngr-peer name args)
      ;; FIXME: `epc:manager-get-method' is not a public function
      (apply (epc:manager-get-method mngr-peer name) args))))

(defun o:call-on-all--server (name &optional args)
  (apply #'deferred:parallel
         (deferred:next
           ;; FIXME: `epc:manager-get-method' is not a public function
           (epc:manager-get-method (car o:clients) name)
           args)
         (mapcar (lambda (mngr) (epc:call-deferred mngr name args))
                 o:clients)))

(defun o:next-node--server ()
  (epc:call-deferred (car o:clients) 'set-input-focus nil))


;;; Client

(defvar o:client-epc nil)

(defun o:start-client ()
  (setq o:client-epc
        (epc:start-epc "cat" (list o:server-port-file)))
  (o:epc-manager-init o:client-epc))

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

(defun o:call-on-next (name &optional args)
  (epc:call-deferred (or o:client-epc (car o:clients)) name args))

(defun o:call-on-all (name &optional args)
  (if o:client-epc
      (o:call-on-all--client name args)
    (o:call-on-all--server name args)))

(defun o:focus-next-node ()
  (interactive)
  (o:call-on-next 'set-input-focus))

(defun o:reload-circle-el ()
  (interactive)
  (o:call-on-all 'load (list "circle")))


;;; circle-mode

(define-minor-mode circle-mode
  "Join to a circle."
  :global t
  (if circle-mode
      (o:start-circle)
    (o:stop-circle)))

(provide 'circle)

;;; circle.el ends here
