;;; sect0-stepper-reload-keys.el --- §0 acceptance A: r/R keymap bug -*- lexical-binding: t; -*-

;; Re-enacts futon3c 1253a8cc: before the fix, turn-stepper's keys were
;; bound inside the defvar init, so a live reload of turn-stepper.el
;; never added r/R to the map Joe's Emacs already held.
;;
;; stepper-reload-keys-fixed: loads the real (fixed) turn-stepper.el
;; twice, as a live reload does; `r' must reach turn-stepper-rewind.
;;
;; stepper-reload-keys-planted-bug: loads planted/stepper-keys-inside-
;; defvar.el (the pre-1253a8cc shape) over a map left from an earlier
;; load; the same key step must FAIL.  This scene is expected to fail —
;; it exists to prove the driver sees the bug.  The runner labels it
;; via :expected-fail so the batch verdict stays honest.

(require 'arxana-dramaturge-scene)

;; Supplied by futon3c/emacs/turn-stepper.el, loaded by the scenes below.
(defvar turn-stepper-mode-map)
(declare-function turn-stepper-mode "turn-stepper")
(declare-function turn-stepper-rewind "turn-stepper")
(declare-function turn-stepper-next "turn-stepper")

(defun sect0--stepper-file ()
  (locate-library "turn-stepper.el"))

(defvar sect0--scenes-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory holding the §0 scene files (captured at load time).")

(defvar sect0--rewind-called nil)

(defun sect0--stub-rewind ()
  "Advise `turn-stepper-rewind' to only set `sect0--rewind-called'."
  (setq sect0--rewind-called nil)
  (advice-add 'turn-stepper-rewind :override
              (lambda (&rest _) (interactive) (setq sect0--rewind-called t))
              '((name . sect0-rewind-spy))))

(defun sect0--unstub-rewind ()
  (advice-remove 'turn-stepper-rewind 'sect0-rewind-spy))

(arxana-dramaturge-scene stepper-reload-keys-fixed
  "Reload the fixed stepper twice, press r, assert rewind was called."
  (let ((file (sect0--stepper-file)))
    (assert (lambda () file) "turn-stepper.el is on the load-path")
    (load file nil t)
    (load file nil t))                     ; the live reload
  (sect0--stub-rewind)
  (unwind-protect
      (progn
        (open-buffer "*scene: stepper fixed*")
        (with-current-buffer arxana-dramaturge-scene--buffer
          (turn-stepper-mode))
        (key "r")
        (await (lambda () sect0--rewind-called) :timeout 2)
        (assert (lambda () sect0--rewind-called)
                "r in turn-stepper-mode calls turn-stepper-rewind")
        (snapshot "sect0-stepper-fixed"))
    (sect0--unstub-rewind)))

(arxana-dramaturge-scene stepper-reload-keys-planted-bug
  "Keys inside the defvar (pre-1253a8cc): after a reload r is undefined.
EXPECTED TO FAIL at the key step — proves the driver sees the bug."
  :expected-fail t
  (let ((planted (expand-file-name
                  "planted/stepper-keys-inside-defvar.el"
                  sect0--scenes-dir)))
    (assert (lambda () (file-exists-p planted)) "planted fixture exists")
    (load planted nil t)
    ;; Simulate the map left over from an earlier load of the OLD file:
    ;; it has n/p/g/q/RET but no r.
    (setq turn-stepper-mode-map (make-sparse-keymap))
    (define-key turn-stepper-mode-map (kbd "n") #'turn-stepper-next)
    ;; Reload the planted file.  Its defvar is a no-op now, and because
    ;; the key bindings live inside the init form, r never arrives.
    (load planted nil t)
    (sect0--stub-rewind)
    (unwind-protect
        (progn
          (open-buffer "*scene: stepper planted*")
          (with-current-buffer arxana-dramaturge-scene--buffer
            (turn-stepper-mode))
          (key "r")
          (assert (lambda () sect0--rewind-called)
                  "r reaches turn-stepper-rewind (expected to fail here)")
          (snapshot "sect0-stepper-planted"))
      (sect0--unstub-rewind))))

;;; sect0-stepper-reload-keys.el ends here
