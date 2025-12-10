;; hol.lisp

(defun seconds-until-next-midnight ()
  "Return number of seconds until the next local midnight."
  (multiple-value-bind (sec min hour date month year dow dst-p tz)
      (get-decoded-time)
    (declare (ignore date month year dow dst-p tz))
    (let* ((seconds-per-day 86400)
           (seconds-today (+ sec (* 60 (+ min (* 60 hour))))))
      (- seconds-per-day seconds-today))))

(defun compute-cycle-day-and-content ()
  "Return (values cycle-day content-string) for the custom 365-day cycle.
Aug 4, 2025 is Day 1."
  (multiple-value-bind (sec min hour date month year &rest ignore)
      (get-decoded-time)
    (declare (ignore sec min hour ignore))
    (let* ((today-midnight (encode-universal-time 0 0 0 date month year))
           (epoch-midnight (encode-universal-time 0 0 0 4 8 2025))
           (seconds-diff (- today-midnight epoch-midnight)))
      (multiple-value-bind (days-since-epoch rem)
          (truncate seconds-diff 86400)
        (declare (ignore rem))
        (let* ((cycle-day (1+ (mod days-since-epoch 365)))
               (content (if (and (>= cycle-day 1) (<= cycle-day 360))
                            (format nil "Day ~D" cycle-day)
                            (let ((k (- cycle-day 360)))
                              (format nil "OUT OF TIME ~D/5" k)))))
          (values cycle-day content))))))

(defun send-notification (content)
  "Send/update the ongoing Termux notification with given CONTENT."
  (sb-ext:run-program
   "termux-notification"
   (list "--id" "lisp-ongoing-cycle"
         "--title" "Cycle"
         "--content" content
         "--priority" "high"
         "--ongoing"
         "--icon" "today")
   :search t :wait t))

(defun update-notification ()
  "Compute current cycle day, send notification, and log to stdout."
  (multiple-value-bind (cycle-day content)
      (compute-cycle-day-and-content)
    (send-notification content)
    (if (and (>= cycle-day 1) (<= cycle-day 360))
        (format t "[ONGOING UPDATED] Day ~D~%" cycle-day)
        (let ((k (- cycle-day 360)))
          (format t "[ONGOING UPDATED] OUT OF TIME ~D/5~%" k)))
    (finish-output)))

(defun main-loop ()
  "Update immediately, then sleep until next midnight and repeat forever."
  (loop
    (update-notification)
    (sleep (seconds-until-next-midnight))))

(main-loop)