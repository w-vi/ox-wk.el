;;; ox-wk.el --- Wiki Back-End for Org Export Engine

;; Author: Vilibald  <vilibald@wvi.cz>
;; Keywords: org, wp, wiki

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Wiki back-end (dokuwiki and creole flavours) for
;; Org exporter, based on `html' back-end. I have started based on Markdown
;; backend and it deserves to be mentioned here for this reason.
;;
;; It provides two commands for export, depending on the desired
;; output: `org-wk-export-as-wiki' (temporary buffer) and
;; `org-wk-export-to-wiki' ("txt" file).

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-html)

;;; User-Configurable Variables

(defgroup org-export-wiki nil
  "Options specific to Wiki export back-end."
  :tag "Org Wiki"
  :group 'org-export
  :version "24.3"
  :package-version '(Org . "8.3"))

(defcustom org-wk-style 'doku
  "Style used to format different elements to different wiki markups.
This variable can be set to either `doku' or `creole' at the moment."
  :group 'org-export-wk
  :type '(choice
          (const :tag "Use \"Dokuwiki\" style" doku)
          (const :tag "Use \"Wiki Creole\" style" creole)))

(defcustom org-wk-org-verbatim 'monospace
  "Style used to format = and ~ markups in org file. I haven't figured out yet how to distinguish these but prefer to use monospace.
This variable can be set to either `monospace' or `verbatim'."
  :group 'org-export-wk
  :type '(choice
          (const :tag "Use \"Monospace\" markup" monospace)
          (const :tag "Use \"Verbatim\" markup" verbatim)))

;;; Define Back-End

(org-export-define-derived-backend 'wk 'html
  :menu-entry
  '(?w "Export to Wiki"
       ((?W "To temporary buffer"
            (lambda (a s v b) (org-wk-export-as-wiki a s v)))
        (?w "To file" (lambda (a s v b) (org-wk-export-to-wiki a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-wk-export-to-wiki t s v)
                (org-open-file (org-wk-export-to-wiki nil s v)))))))
  :translate-alist '((bold . org-wk-bold)
                     (code . org-wk-code)
                     (src-block . org-wk-src-block)
                     (comment . (lambda (&rest args) ""))
                     (comment-block . (lambda (&rest args) ""))
                     (example-block . org-wk-src-block)
                     (fixed-width . org-wk-fixed-width)
                     (footnote-definition . ignore)
                     (footnote-reference . ignore)
                     (headline . org-wk-headline)
                     (horizontal-rule . org-wk-horizontal-rule)
                     (inline-src-block . org-wk-code)
                     (italic . org-wk-italic)
                     (underline . org-wk-underline)
                     (item . org-wk-item)
                     (line-break . org-wk-line-break)
                     (link . org-wk-link)
                     (table . org-wk-table)
                     (table-cell . org-wk-table-cell)
                     (table-row . org-wk-table-row)
                     (paragraph . org-wk-paragraph)
                     (plain-list . org-wk-plain-list)
                     (plain-text . org-wk-plain-text)
                     (quote-block . org-wk-quote-block)
                     (section . org-wk-section)
                     (template . org-wk-template)
                     (verbatim . org-wk-verbatim)
                     ))

;;; Filters

;;; Transcode Functions

;;; Creole Functions

(defun org-wk--creole-nowiki (object contents info &optional newline)
  "Creole has a limited set of markup very often we
leave it as it is and go to preformatted nowiki style. OBJECT is not used atm,
it just formats the CONTENTS. NEWLINE indicates if the markup
should be on separate lines."
  (concat "{{{" (when newline "\n") contents "}}}"))

;;;; Bold

(defun org-wk-bold (bold contents info)
  "Transcode BOLD object.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "**%s**" contents))

;;;; Underline

(defun org-wk-underline (underline contents info)
  "Transcode UNDERLINE object.
CONTENTS is the text within underline markup.  INFO is a plist used as
a communication channel."
  (format "__%s__" contents))

;;;; Fixed width

(defun org-wk-fixed-width (fixed-width contents info)
  "Transcode FIXED-WIDTH element.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value fixed-width)))
    (cond
     ((eq org-wk-style 'creole) (org-wk--creole-nowiki fixed-width value info))
     (t (format "''%s''" value)))))

;;;; Code and Verbatim

(defun org-wk-code (code contents info)
  "Transcode CODE object.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value code))
        (lang (org-element-property :language code)))
    (cond
     ((eq org-wk-style 'creole) (org-wk--creole-nowiki code value info))
     (t (concat "<code" (if lang (format " %s> " lang) "> ") (format "%s</code>" value))))))

(defun org-wk-verbatim (verbatim contents info)
  "Transcode VERBATIM object.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value verbatim)))
    (cond
     ((eq org-wk-style 'creole) (org-wk--creole-nowiki verbatim value info))
     ((eq org-wk-org-verbatim 'monospace)(org-wk-fixed-width verbatim contents info))
     (t (format "%%%% %s %%%%" value)))))

;;;; Src-Block

(defun org-wk-src-block (src-block contents info)
  "Transcode SRC-BLOCK element.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((lang (org-element-property :language src-block))
        (content (org-export-format-code-default src-block info)))
    (cond
     ((eq org-wk-style 'creole) (org-wk--creole-nowiki src-block content info t))
     (t (concat "<code" (if lang (format " %s>\n" lang) ">\n") (format "%s</code>" content))))))

;;;; Headline

(defun org-wk-headline (headline contents info)
  "Transcode HEADLINE element.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (title (org-export-data (org-element-property :title headline) info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword
                                                        headline)))
                        (and todo (concat (org-export-data todo info) " ")))))
           (tags (and (plist-get info :with-tags)
                      (let ((tag-list (org-export-get-tags headline info)))
                        (and tag-list
                             (format "     :%s:"
                                     (mapconcat 'identity tag-list ":"))))))
           (priority
            (and (plist-get info :with-priority)
                 (let ((char (org-element-property :priority headline)))
                   (and char (format "[#%c] " char)))))
           ;; Headline text without tags.
           (heading (concat todo priority title)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (and (org-export-low-level-p headline info)
                 (> (org-export-low-level-p headline info) 3))
            (and (eq org-wk-style 'doku) (> level 5))
            (and (eq org-wk-style 'creole) (> level 6)))
        (let ((bullet
               (if (not (org-export-numbered-headline-p headline info)) "*" "-" )))
          (concat "  " bullet heading tags "\n\n"
                  (and contents (replace-regexp-in-string "^" "    " contents)))))
       ((eq org-wk-style 'creole)
        (concat (make-string level ?=) " " heading tags "\n\n" contents))
       (t (let ((markup (make-string (- 7 level) ?=)))
            (concat markup " " heading tags " " markup "\n\n" contents)))))))

;;;; Horizontal Rule

(defun org-wk-horizontal-rule (horizontal-rule contents info)
  "Transcode HORIZONTAL-RULE element.
CONTENTS is the horizontal rule contents, none is actually used.  INFO is a plist used
as a communication channel."
  "----")

;;;; Italic

(defun org-wk-italic (italic contents info)
  "Transcode ITALIC object.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "//%s//" contents))

;;;; Item

(defun org-wk-item (item contents info)
  "Transcode ITEM element.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((plain-list (org-export-get-parent item))
         (type (org-element-property :type plain-list))
         (bullet (if (eq org-wk-style 'creole)
                     (if (eq type 'ordered) "#" "*" )
                   (if (eq type 'ordered) "-" "*" )))
         (counter (org-element-property :counter item))
         (checkbox (org-element-property :checkbox item))
         (tag (let ((tag (org-element-property :tag item)))
                (and tag (org-export-data tag info))))
         (level
          ;; Determine level of current item to determine the
          ;; correct indentation or number of bullets to use.
          (let ((parent item) (level 0))
            (while (memq (org-element-type
                          (setq parent (org-export-get-parent parent)))
                         '(plain-list item))
              (when (eq (org-element-type parent) 'plain-list)
                (incf level)))
            level))
         (prefix (if (eq org-wk-style 'creole) (if (eq type 'ordered)?# ?*) ? )))
    (concat
     (if (eq org-wk-style 'doku) (make-string (* 2 level) prefix )
       (make-string (1- level) prefix))
     bullet " "
     (case checkbox
       (on "[X] ")
       (trans "[-] ")
       (off "[ ] "))
     (and tag (format "**%s:** "(org-export-data tag info)))
     (and contents (org-trim contents)))))

;;;; Line Break

(defun org-wk-line-break (line-break contents info)
  "Transcode LINE-BREAK object.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  "  \\\\ ")

;;;; Link

(defun org-wk-link (link contents info)
  "Transcode a LINK object from Org to HTML.

CONTENTS is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See `org-export-data'."
  (let ((--link-org-files-as-html-maybe
         (function
          (lambda (raw-path info)
            ;; Treat links to `file.org' as links to `file.html', if
            ;; needed.  See `org-html-link-org-files-as-html'.
            (cond
             ((and org-html-link-org-files-as-html
                   (string= ".org"
                            (downcase (file-name-extension raw-path "."))))
              (concat (file-name-sans-extension raw-path) "."
                      (plist-get info :html-extension)))
             (t raw-path)))))
        (type (org-element-property :type link)))
    (cond ((member type '("custom-id" "id"))
           (let ((destination (org-export-resolve-id-link link info)))
             (if (stringp destination)  ; External file.
                 (let ((path (funcall --link-org-files-as-html-maybe
                                      destination info)))
                   (if (not contents) (format "<%s>" path)
                     (format "[[%s|%s]]" path contents)))
               (concat
                (and contents (concat contents " "))
                (format "#%s"
                        (format (org-export-translate "See section %s" :html info)
                                (mapconcat 'number-to-string
                                           (org-export-get-headline-number
                                            destination info)
                                           ".")))))))
          ((org-export-inline-image-p link org-html-inline-image-rules)
           (let ((path (let ((raw-path (org-element-property :path link)))
                         (if (not (file-name-absolute-p raw-path)) raw-path
                           (expand-file-name raw-path)))))
             (format "{{%s|%s}}" path
                     (let ((caption (org-export-get-caption
                                     (org-export-get-parent-element link))))
                       (if caption (org-export-data caption info) path))
                     )))
          ((string= type "coderef")
           (let ((ref (org-element-property :path link)))
             (format (org-export-get-coderef-format ref contents)
                     (org-export-resolve-coderef ref info))))
          ((equal type "radio")
           (let ((destination (org-export-resolve-radio-link link info)))
             (org-export-data (org-element-contents destination) info)))
          ((equal type "fuzzy")
           (let ((destination (org-export-resolve-fuzzy-link link info)))
             (if (org-string-nw-p contents) contents
               (when destination
                 (let ((number (org-export-get-ordinal destination info)))
                   (when number
                     (if (atom number) (number-to-string number)
                       (mapconcat 'number-to-string number "."))))))))
          (t (let* ((raw-path (org-element-property :path link))
                    (path (cond
                           ((member type '("http" "https" "ftp"))
                            (concat type ":" raw-path))
                           ((equal type "file")
                            ;; Treat links to ".org" files as ".html",
                            ;; if needed.
                            (setq raw-path
                                  (funcall --link-org-files-as-html-maybe
                                           raw-path info))
                            ;; If file path is absolute, prepend it
                            ;; with protocol component - "file://".
                            (if (not (file-name-absolute-p raw-path)) raw-path
                              (concat "file://" (expand-file-name raw-path))))
                           (t raw-path))))
               (if (not contents) (format "%s" path)
                 (format "[[%s|%s]]" path contents)))))))

;;;; Paragraph

(defun org-wk-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (let ((first-object (car (org-element-contents paragraph))))
    ;; If paragraph starts with a #, protect it.
    (if (and (stringp first-object) (string-match "\\`#" first-object))
        (replace-regexp-in-string "\\`#" "\\#" contents nil t)
      contents)))

;;;; Plain List

(defun org-wk-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST element.
CONTENTS is the plain-list contents.  INFO is a plist used as
a communication channel."
  contents)

;;;; Plain Text

(defun org-wk-plain-text (text info)
  "Transcode a TEXT string.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (when (plist-get info :with-smart-quotes)
    (setq text (org-export-activate-smart-quotes text :html info)))
  ;; Protect ambiguous #.  This will protect # at the beginning of
  ;; a line, but not at the beginning of a paragraph.  See
  ;; `org-wk-paragraph'.
  (setq text (replace-regexp-in-string "\n#" "\n\\\\#" text))
  ;; Protect ambiguous !
  (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
  ;; Protect `, *, _ and \
  (setq text (replace-regexp-in-string "[`*_\\]" "\\\\\\&" text))
  ;; Handle special strings, if required.
  (when (plist-get info :with-special-strings)
    (setq text (org-html-convert-special-strings text)))
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  ;; Return value.
  text)

;;;; Quote Block

(defun org-wk-quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK element.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
  (replace-regexp-in-string
   "^" "> "
   (replace-regexp-in-string "\n\\'" "" contents)))

;;;; Section

(defun org-wk-section (section contents info)
  "Transcode SECTION element.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)

;;;; Template

(defun org-wk-template (contents info)
  "Return complete document string after conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)

;;;; Table

(defun org-wk-table (table contents info)
  "Transcode TABLE element.
CONTENTS is the table contents.  INFO is a plist used
as a communication channel."
  contents)

(defun org-wk-table-row  (table-row contents info)
  "Transcode TABLE-ROW element.
CONTENTS is the row contents.  INFO is a plist used
as a communication channel."
  (cond
   ((eq org-wk-style 'creole)
    (concat
     (if (org-string-nw-p contents) (format "|%s" contents) "")))
   (t (concat
       (if (org-string-nw-p contents) (format "%s" contents)
         "")
       (when (org-export-table-row-ends-header-p table-row info)
         "^")))))

(defun org-wk-table-cell  (table-cell contents info)
  "Transcode TABLE-CELL element.
CONTENTS is the table-cell contents.  INFO is a plist used
as a communication channel. Treat Header cells differently.
FIXME : support also row header cells, now headers are in columns only"
  (let ((table-row (org-export-get-parent table-cell)))
    (cond
     ((org-export-table-row-starts-header-p table-row info)
      (if (eq org-wk-style 'doku)(concat "^ " contents)
        (format "=%s|" contents)))
     ((org-export-table-cell-starts-colgroup-p table-cell info)
      (if (eq org-wk-style 'doku) (concat "|" contents "|")
        (format "%s|" contents)))
     (t (concat contents "|")))))

;;; Interactive function

;;;###autoload

(defun org-wk-export-as-wiki (&optional async subtreep visible-only)
  "Export current buffer to a Wiki buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Wiki Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (if async
      (org-export-async-start
          (lambda (output)
            (with-current-buffer (get-buffer-create "*Org Wiki Export*")
              (erase-buffer)
              (insert output)
              (goto-char (point-min))
              (text-mode)
              (org-export-add-to-stack (current-buffer) 'wk)))
        `(org-export-as 'wk ,subtreep ,visible-only))
    (let ((outbuf (org-export-to-buffer
                      'wk "*Org Wiki Export*" subtreep visible-only)))
      (with-current-buffer outbuf (text-mode))
      (when org-export-show-temporary-export-buffer
        (switch-to-buffer-other-window outbuf)))))

;;;###autoload
(defun org-wk-convert-region-to-wk ()
  "Assume the current region has org-mode syntax, and convert it to Wiki syntax.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in a Wiki sytntax buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'wk))

;;;###autoload
(defun org-wk-export-to-wiki (&optional async subtreep visible-only)
  "Export current buffer to a Wiki sytntax text file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".txt" subtreep)))
    (if async
        (org-export-async-start
            (lambda (f) (org-export-add-to-stack f 'wk))
          `(expand-file-name
            (org-export-to-file 'wk ,outfile ,subtreep ,visible-only)))
      (org-export-to-file 'wk outfile subtreep visible-only))))

(provide 'ox-wk)

;;; ox-wk.el ends here
