;; BBS quote optimizer
;; License: GNU GPL v2
;; Usage:
;; > (optimize-quotes ">>20>>21>>90>>22>>23>>24>>25>>26>>27>>28>>29")
;; ">>20-29,90"

(defun parse-post-nums (str)
  (cond ((string= "" str)
         '())
        ((not (eq (string-to-number str)
                  0))
         (let* ((n (string-to-number str))
                (idx (1+ (floor (log10 n)))))
           (cons n
                 (parse-post-nums (substring str idx)))))
        (t (parse-post-nums (substring str 1)))))

(defun adjacent-nums (lst last-num)
  (cond ((eq (1+ last-num)
             (car lst))
         (cons (car lst)
               (adjacent-nums (cdr lst)
                              (car lst))))
        (t '())))

(defun partition-adjacents (lst)
  (if (null lst) '()
      (let* ((mn (1- (car lst)))
             (adjs (adjacent-nums lst mn))
             (adjs-cnt (length adjs)))
        (cond ((> (length lst)
                  adjs-cnt)
               (cons adjs
                     (partition-adjacents (cl-subseq lst adjs-cnt))))
              ((eq 1 adjs-cnt)
               (cons (car adjs)
                     (partition-adjacents (cl-subseq lst adjs-cnt))))
              (t (list adjs))))))

(defun partitions-to-ranges (lst)
  (cond ((null lst) '())
        ((and (listp (car lst))
              (eq 1 (length (car lst))))
         (cons (car lst)
               (partitions-to-ranges (cdr lst))))
        ((listp (car lst))
         (cons (list (caar lst)
                     (car (last (car lst))))
               (partitions-to-ranges (cdr lst))))
        (t (cons (car lst)
                 (partitions-to-ranges (cdr lst))))))

(defun textualize (lst)
  (cond ((null lst) '())
        ((and (listp (car lst))
              (eq 1 (length (car lst))))
         (cons (list (number-to-string (caar lst)))
               (textualize (cdr lst))))
        ((listp (car lst))
         (cons (list (number-to-string (caar lst))
                     "-"
                     (number-to-string (car (cdar lst))))
               (textualize (cdr lst))))
        (t (cons (list (number-to-string (car lst)))
                 (textualize (cdr lst))))))

(defun optimize-quotes (raw)
  (concat ">>"
          (mapconcat (lambda (x) (apply #'concat x))
                     (textualize (partitions-to-ranges (partition-adjacents (sort (parse-post-nums raw) #'<))))
                     ",")))
