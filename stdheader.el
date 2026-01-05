; **************************************************************************** ;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    stdheader.el                                       :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: vitosant <marvin@42.fr>                    +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2026/01/05 16:40:27 by vitosant          #+#    #+#              ;
;    Updated: 2026/01/05 16:40:37 by vitosant         ###    ########.fr       ;
;                                                                              ;
; **************************************************************************** ;

(defvar 42-header-user-login "user" "42 user login")
(defvar 42-header-user-mail "marvin@42.fr" "42 user email")

;; Standard 42 geometry: Each line is exactly 25 chars wide.
(defconst 42-ascii-art
  '("        :::      ::::::::"
    "      :+:      :+:    :+:"
    "    +:+ +:+         +:+  "
    "  +#+  +:+       +#+     "
    "+#+#+#+#+#+   +#+        "
    "     #+#    #+#          "
    "    ###    ########.fr   "))

(defun 42-get-comment-chars ()
  "Returns (START END FILL) strings based on the current major-mode."
  (cond
   ((member major-mode '(c-mode c++-mode java-mode js-mode php-mode go-mode rust-mode))
    '("/*" "*/" "*"))
   ((member major-mode '(html-mode nxml-mode web-mode))
    '("" "*"))
   ((member major-mode '(makefile-mode makefile-gmake-mode python-mode perl-mode ruby-mode sh-mode shell-script-mode dockerfile-mode yaml-mode conf-mode))
    '("#" "#" "*"))
   ((member major-mode '(latex-mode tex-mode))
    '("%" "%" "*"))
   ((member major-mode '(emacs-lisp-mode lisp-mode asm-mode))
    '(";" ";" "*"))
   ((member major-mode '(lua-mode sql-mode))
    '("--" "--" "-"))
   (t '("#" "#" "*"))))

(defun 42-format-line (left right)
  "Generates a formatted line with exactly 80 columns."
  (let* ((chars (42-get-comment-chars))
         (start (nth 0 chars))
         (end (nth 1 chars))
         (margin 5)
         (total-len 80)
         ;; Central content width = 80 - 5 (left) - 5 (right)
         (content-width (- total-len (* 2 margin)))
         
         ;; Ensure left text doesn't overflow the limit (Vim behavior)
         (left-truncated (if (> (+ (length left) (length right)) content-width)
                             (substring left 0 (- content-width (length right)))
                           left))
         
         ;; Padding calculation
         (padding-start (make-string (- margin (length start)) ?\s))
         (spacer (make-string (- content-width (length left-truncated) (length right)) ?\s))
         (padding-end (make-string (- margin (length end)) ?\s)))
    
    (concat start padding-start left-truncated spacer right padding-end end)))

(defun 42-line-content (n)
  "Generates the content for header line N (1-11)."
  (let ((filename (file-name-nondirectory (or (buffer-file-name) "<new>")))
        (date-str (format-time-string "%Y/%m/%d %H:%M:%S")))
    (cond
     ;; Solid lines (1 and 11)
     ((or (= n 1) (= n 11))
      (let* ((chars (42-get-comment-chars))
             (start (nth 0 chars))
             (end (nth 1 chars))
             (fill (string-to-char (nth 2 chars)))
             (fill-len (- 80 (length start) (length end) 2)))
        (concat start " " (make-string fill-len fill) " " end)))
     
     ;; Empty lines (2 and 10)
     ((or (= n 2) (= n 10))
      (42-format-line "" ""))
     
     ;; Lines with ASCII art
     ((or (= n 3) (= n 5) (= n 7))
      (42-format-line "" (nth (- n 3) 42-ascii-art)))
     
     ;; Filename (Line 4)
     ((= n 4)
      (42-format-line filename (nth (- n 3) 42-ascii-art)))
     
     ;; Author (Line 6)
     ((= n 6)
      (42-format-line (format "By: %s <%s>" 42-header-user-login 42-header-user-mail)
                      (nth (- n 3) 42-ascii-art)))
     
     ;; Created (Line 8)
     ((= n 8)
      (42-format-line (format "Created: %s by %s" date-str 42-header-user-login)
                      (nth (- n 3) 42-ascii-art)))
     
     ;; Updated (Line 9)
     ((= n 9)
      (42-format-line (format "Updated: %s by %s" date-str 42-header-user-login)
                      (nth (- n 3) 42-ascii-art))))))

(defun 42-insert ()
  "Inserts the 11 header lines at the top."
  (save-excursion
    (goto-char (point-min))
    (insert "\n") ; Empty line after header (matches original Vim script)
    (goto-char (point-min))
    (let ((n 1))
      (while (<= n 11)
        (insert (42-line-content n) "\n")
        (setq n (1+ n))))))

(defun 42-update ()
  "Analyzes line 9. If it's a valid header, updates it and returns t. Otherwise nil."
  (save-excursion
    (goto-char (point-min))
    (if (< (count-lines (point-min) (point-max)) 9)
        nil ; File has fewer than 9 lines, impossible to have a valid header
      (forward-line 8) ; Go to line 9 (0-indexed + 8)
      
      (let* ((chars (42-get-comment-chars))
             (start (nth 0 chars))
             (margin 5)
             ;; Build regex: Start char + Margin spaces + "Updated: "
             (prefix (concat (regexp-quote start)
                             (make-string (- margin (length start)) ?\s)
                             "Updated: ")))
        
        (if (looking-at prefix)
            (progn
              ;; Update line 9 (Date) only if buffer is modified (Vim logic)
              (when (buffer-modified-p) 
                 (delete-region (point-at-bol) (point-at-eol))
                 (insert (42-line-content 9)))
              
              ;; Update line 4 (Filename) - Vim always does this
              (goto-char (point-min))
              (forward-line 3)
              (delete-region (point-at-bol) (point-at-eol))
              (insert (42-line-content 4))
              t) ; Return success
          nil))))) ; Return failure

(defun 42-stdheader ()
  "Main command: Attempts to update. If it fails (no header found), inserts a new one."
  (interactive)
  (unless (42-update)
    (42-insert)))

;; Bindings and Hooks
(global-set-key (kbd "<f1>") '42-stdheader)
(global-set-key (kbd "C-c h") '42-stdheader)
(add-hook 'write-file-hooks (lambda () (42-update) nil))
