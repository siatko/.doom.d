;;; math.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Simon Attila Weis
;;
;; Author: Simon Attila Weis <https://github.com/siatwe>
;; Maintainer: Simon Attila Weis <me@siatwe.dev>
;; Created: July 31, 2021
;; Modified: July 31, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/siatwe/math
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

;; 3n+1 Problem: https://en.wikipedia.org/wiki/Collatz_conjecture
(defun 3np1 (n)
  (setq iterations 0)
  (while (not (= n 1))
    (setq iterations (1+ iterations))
    (if (cl-oddp n)
        (setq n (1+ (* n 3)))
      (setq n (/ n 2)))) iterations)

(3np1 1)
(3np1 2)
(3np1 4)
(3np1 5)
(3np1 27)
(3np1 99)
(3np1 999)
(3np1 9999)
(3np1 99999)
(3np1 999999)
(3np1 9999999)
(3np1 99999999)

(provide 'math)
;;; math.el ends here
