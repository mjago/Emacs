(require 'xtide)

(ert-deftest test-xtide-location-select ()
  (should (equal "Press `RET' to select location" (xtide-location-select)))
  )

(ert-deftest test-xtide-location-value ()
  (should (equal "Bournemouth, England" (xtide-location-value)))
  )

(ert-deftest test-encode-time-decode-time ()
  ;; input SECOND MINUTE HOUR DAY MONTH YEAR &optional ZONE
  (should (equal '(20223 50472) (encode-time 0 30 2 1 1 2012 0 nil 0)))
  (should (equal '(0 30 2 1 1 2012 0 nil 0) (decode-time '(20223 50472))))
  (set (make-local-variable 'xtide-time) '(20223 50472))
  (with-temp-buffer
;;    (should (equal '(20223 50472) xtide-time))
    (should (equal 0 (xtide-run "-ft" (current-buffer))))
;;    (should (equal "" (buffer-string)))
    )
;;  (xtide)
;;  (should (equal '(20382 2859 218179) xtide-time))
  (should (equal '(51 46 4 30 4 2012 1 t 3600) (decode-time '(20382 2859 218179))))
  )
