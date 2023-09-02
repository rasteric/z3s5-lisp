;;; run selftest and exit

(run-selftest)

(if (< (get *testinfo* 'success) (get *testinfo* 'count))
    (exit 1)
    (exit 0))
