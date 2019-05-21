
(ql:quickload '(:cl-base64 
                :ironclad
                :cl-qrencode))
(require :sb-sprof)


(qrcode:encode-png "http://oofoosadfas:182/asgag?id=15rtgsgwgtre3reaewgweg"
                   :fpath #P"/tmp/a.png")


(defun inline-image (data)
  (format nil "data:image/png;base64,~a"
          (base64:usb8-array-to-base64-string  
            (ironclad:with-octet-output-stream (output)
              (qrcode:encode-png-stream
                data
                output
                :pixsize 3)))))

; 
; (defun do-something (x)
;   (1+ (aref x 0)))
; 
; (defun foo (x)
;   (let ((arr (make-array x 
;                          :element-type 'fixnum
;                          :initial-element 0
;                          :fill-pointer 0)))
;     (vector-push-extend 0 arr)
;     (do-something arr)
;     (loop for i below x
;           do (incf (aref arr (1- (fill-pointer arr)))))
;     arr))
; 
; (foo 6)

#+(or)
(time 
  (dotimes (i 1000)
    (inline-image "http://oofoosadfas:182/asgag?id=15rtgsgwgtre3reaewgweg")))

#+(or)
(sb-aprof:aprof-run 
  (lambda ()
    (inline-image "http://oofoosadfas:182/asgag?id=15rtgsgwgtre3reaewgweg")))

#+(or)
(sb-sprof:with-profiling (:report :graph
                                  :sample-interval 0.001)
  (dotimes (i 1000)
    (inline-image "http://oofoosadfas:182/asgag?id=15rtgsgwgtre3reaewgweg")))

;; -- before, loop over 1000:
;; Evaluation took:
;;   6.688 seconds of real time
;;   6.703777 seconds of total run time (6.687839 user, 0.015938 system)
;;   [ Run times consist of 0.149 seconds GC time, and 6.555 seconds non-GC time. ]
;;   100.24% CPU
;;   18,681,105,340 processor cycles
;;   3,893,419,072 bytes consed


;; -- now
;; Evaluation took:
;;   6.070 seconds of real time
;;   6.077617 seconds of total run time (6.059445 user, 0.018172 system)
;;   [ Run times consist of 0.102 seconds GC time, and 5.976 seconds non-GC time. ]
;;   100.13% CPU
;;   552 lambdas converted
;;   16,956,197,638 processor cycles
;;   1,752,741,488 bytes consed

;; -- DARK-MODULE-P
;; Evaluation took:
;;   5.625 seconds of real time
;;   5.632086 seconds of total run time (5.628095 user, 0.003991 system)
;;   [ Run times consist of 0.070 seconds GC time, and 5.563 seconds non-GC time. ]
;;   100.12% CPU
;;   15,712,000,890 processor cycles
;;   1,728,484,944 bytes consed
;;  
;; -- More changes
;; Evaluation took:
;;   5.154 seconds of real time
;;   5.162030 seconds of total run time (5.150070 user, 0.011960 system)
;;   [ Run times consist of 0.079 seconds GC time, and 5.084 seconds non-GC time. ]
;;   100.16% CPU
;;   101 lambdas converted
;;   14,397,713,080 processor cycles
;;   1,731,186,336 bytes consed
;;  
;; -- TRUNCATE down
;; Evaluation took:
;;   4.864 seconds of real time
;;   4.871911 seconds of total run time (4.844105 user, 0.027806 system)
;;   [ Run times consist of 0.082 seconds GC time, and 4.790 seconds non-GC time. ]
;;   100.16% CPU
;;   13,588,748,045 processor cycles
;;   1,728,453,744 bytes consed
;; 
;; --
;; Evaluation took:
;;   3.234 seconds of real time
;;   3.241055 seconds of total run time (3.229031 user, 0.012024 system)
;;   [ Run times consist of 0.081 seconds GC time, and 3.161 seconds non-GC time. ]
;;   100.22% CPU
;;   9,032,915,656 processor cycles
;;   1,636,603,616 bytes consed

