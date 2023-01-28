;;; org-link-style.el --- An org-link to deal with styles and faces  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alejandro Gallo

;; SPDX-License-Identifier: MIT
;; Author: Alejandro Gallo <aamsgallo@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: hypermedia, tex, faces, extensions
;; URL: https://github.com/alejandrogallo/org-link-style

;;; Commentary:

;; TODO

;;; Code:


(require 'org-protocol)

(defun org-link-style--org-to-html-hex (rgb)
  (apply #'color-rgb-to-hex
         (cl-concatenate 'list
                         (color-name-to-rgb rgb)
                         (list 2))))

(defun org-link-style--id-to-face-plist (style-id)
  (if (string-match "@\\(.+\\)" style-id)
      (let ((face-name (car (read-from-string (match-string 1 style-id))))
            face-plist)
        (cl-flet ((maybe-add (name)
                    (let ((attribute (face-attribute face-name name)))
                      (when (and (not (eq attribute 'unspecified))
                                 attribute)
                        (push attribute face-plist)
                        (push name face-plist)
                        face-plist))))
          (progn
            (maybe-add :background)
            (maybe-add :foreground))))
    (org-protocol-convert-query-to-plist style-id)))


(defun org-link-style--export (style-id text format info)
  (cl-destructuring-bind (&key background foreground &allow-other-keys)
      (org-protocol-convert-query-to-plist style-id)
    (when background
      (setq background (org-link-style--org-to-html-hex background)))
    (when foreground
      (setq foreground (org-link-style--org-to-html-hex foreground)))
    (pcase format
      ((or 'html 'md)
       (let ((html-style (string-join
                          (list (when foreground
                                  (format "color: %s"
                                          (org-link-style--org-to-html-hex
                                           foreground)))
                                (when background
                                  (format "background-color: %s"
                                          (org-link-style--org-to-html-hex
                                           background))))
                          ";")))
         (format "<span style='%s'>%s</span>" html-style text)))
      ((or 'latex 'beamer)
       (string-join (list
                     (when background
                       (format "\\colorbox[HTML]{%s}{"
                               (substring background 1)))
                     (when foreground
                       (format "\\textcolor[HTML]{%s}{"
                               (substring foreground 1)))
                     text
                     (when foreground "}")
                     (when background "}"))))
      (_ text))))

(org-link-set-parameters
 "style"
 :export #'org-link-style--export
 :complete (lambda (&optional arg)
             (let ((fg (read-color "Foreground: ")))
               (format "style:foreground=%s" fg)))
 :face #'org-link-style--id-to-face-plist)


(provide 'org-link-style)
;;; org-link-style.el ends here
