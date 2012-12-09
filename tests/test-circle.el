;;; test-circle.el --- Tests for circle

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; test-circle.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; test-circle.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with test-circle.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'circle)

(ert-deftest o:next-in-list ()
  (should (eq (o:next-in-list '(a b c) 'a) 'b))
  (should (eq (o:next-in-list '(a b c) 'b) 'c))
  (should (eq (o:next-in-list '(a b c) 'c) nil)))

(ert-deftest o:previous-in-list ()
  (should (eq (o:previous-in-list '(a b c) 'a) nil))
  (should (eq (o:previous-in-list '(a b c) 'b) 'a))
  (should (eq (o:previous-in-list '(a b c) 'c) 'b)))

(provide 'test-circle)

;;; test-circle.el ends here
