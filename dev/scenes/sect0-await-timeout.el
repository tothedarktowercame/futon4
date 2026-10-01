;;; sect0-await-timeout.el --- §0 acceptance B: await timeout -*- lexical-binding: t; -*-

;; A scene whose await never becomes true must report "timed out"
;; within its timeout, and the runner must continue to the next scene.

(require 'arxana-dramaturge-scene)

(arxana-dramaturge-scene await-timeout-reports
  "An await that never comes true reports timed-out; runner continues.
EXPECTED TO FAIL (timed-out step) — acceptance B for §0."
  :expected-fail t
  (open-buffer "*scene: timeout*")
  (type "waiting for something that never happens")
  (await (lambda () nil) :timeout 1)
  (assert (lambda () t) "this step is never reached in spirit, but the runner survives"))

(arxana-dramaturge-scene after-timeout-runs
  "Runs after await-timeout-reports; proves the runner did not hang."
  (open-buffer "*scene: after timeout*")
  (type "still alive")
  (assert-buffer "still alive")
  (snapshot "sect0-after-timeout"))

;;; sect0-await-timeout.el ends here
