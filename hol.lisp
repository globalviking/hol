;; hol.lisp



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

(defun get-ordinal-suffix (n)
  "Return string suffix st, nd, rd, th for integer N."
  (let ((remainder10 (mod n 10))
        (remainder100 (mod n 100)))
    (if (and (>= remainder100 11) (<= remainder100 13))
        "th"
        (case remainder10
          (1 "st")
          (2 "nd")
          (3 "rd")
          (t "th")))))

(defun get-cycle-title (cycle-day)
  "Return the notification title based on CYCLE-DAY."
  (cond
    ((and (>= cycle-day 1) (<= cycle-day 360))
     (let* ((zodiac-signs '("Leo" "Virgo" "Libra" "Scorpio" "Sagittarius" 
                            "Capricorn" "Aquarius" "Pisces" "Aries" "Taurus" 
                            "Gemini" "Cancer"))
            (sign-index (floor (1- cycle-day) 30))
            (sign (nth sign-index zodiac-signs))
            (cycle-num (1+ sign-index))
            (suffix (get-ordinal-suffix cycle-num)))
       (format nil "~A (~D~A cycle)" sign cycle-num suffix)))
    ((> cycle-day 360)
     (let ((k (- cycle-day 360)))
       (format nil "Day ~D Out of Time" k)))
    (t "Cycle")))

(defun send-notification (title content)
  "Send/update the ongoing Termux notification with given TITLE and CONTENT."
  (sb-ext:run-program
   "termux-notification"
   (list "--id" "lisp-ongoing-cycle"
         "--title" title
         "--content" content
         "--priority" "high"
         "--ongoing"
         "--alert-once"
         "--icon" "today")
   :search t :wait t))

(defun update-notification ()
  "Compute current cycle day, send notification, and log to stdout."
  (multiple-value-bind (cycle-day content)
      (compute-cycle-day-and-content)
    (let ((title (get-cycle-title cycle-day)))
      (send-notification title content)
      (format t "[ONGOING UPDATED] Title: ~A | Content: ~A~%" title content)
      (finish-output))))

(defun main-loop ()
  "Update immediately, then sleep for 1 minute and repeat forever."
  (loop
    (update-notification)
    (sleep 60)))

(main-loop)