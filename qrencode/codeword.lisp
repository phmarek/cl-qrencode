;;;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; bit stream to codeword conversion

(in-package #:cl-qrencode)

(defun padding-bits (bstream)
  "add padding bits so that BSTREAM ends at a codeword boundary"
  (multiple-value-bind (quot rem) (ceiling (length bstream) 8)
    (declare (ignore quot))
    (make-list (- rem) :initial-element 0)))

(defun pad-codewords (bstream version level)
  "add pad codewords (after adding padding-bits) to fill data codeword capacity"
  (let ((pad-word1 '(1 1 1 0 1 1 0 0))
        (pad-word2 '(0 0 0 1 0 0 0 1))
        (pad-len (- (data-words-capacity version level)
                    (/ (length bstream) 8)))
        (ret nil))
    (tagbody
      :start
      (when (minusp pad-len) 
        (go :end))
      (append-bits bstream pad-word1)
      (decf pad-len)
      (when (minusp pad-len) 
        (go :end))
      (append-bits bstream pad-word1)
      (decf pad-len)
      (go :start)
      :end
      nil)
    bstream))

(defun bstream->codewords (bstream)
  ;; convert to vector here
  ;; (ldb 
  "convert bstream into codewords, as coefficients of the terms of a polynomial"
  (loop for i below (length bstream) by 8
        collect (bstream->decimal bstream i 8))
  #+(or)
  (loop for b on bstream by (alexandria:curry #'nthcdr 8)
        collect (bstream->decimal b 8))
  #+(or)
  (do ((b bstream (nthcdr 8 b))
       (codewords nil))
      ((null b) codewords)
    (setf codewords (append codewords (list (bstream->decimal b 8))))))

(defun take-in-turn (blks)
  "taking codewords from each block (bound by minimum length) in turn"
  (reduce #'append (apply #'mapcar #'list blks)))

(defun take-data-in-turn (blocks blk1 data1 blk2 data2)
  "taking data words from each block (might have different length) in turn"
  (let ((data-final nil)
        (left-blks nil))
    (setf data-final (take-in-turn blocks))
    (cond
      ((or (= blk1 0) (= blk2 0))
       ;; only one kind of block exists
       (setf left-blks nil))
      ((> data1 data2)
       ;; block 1 has more elements left
       (setf left-blks (mapcar #'(lambda (blk)
                                   (nthcdr data2 blk))
                               (subseq blocks 0 blk1))))
      ((> data2 data1)
       ;; block 2 has more elements left
       (setf left-blks (mapcar #'(lambda (blk)
                                   (nthcdr data1 blk))
                               (subseq blocks blk1 (+ blk1 blk2))))))
    (if left-blks
        (append data-final (take-in-turn left-blks))
        data-final)))
