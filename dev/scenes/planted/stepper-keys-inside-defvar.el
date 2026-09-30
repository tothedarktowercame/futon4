;;; stepper-keys-inside-defvar.el --- planted pre-1253a8cc keymap shape -*- lexical-binding: t; -*-

;; Scratch copy of the turn-stepper keymap pattern BEFORE futon3c
;; 1253a8cc: the keys are bound inside the defvar init form, so once
;; the variable is set, reloading this file adds nothing.  Loaded by
;; the stepper-reload-keys-planted-bug scene after the real
;; turn-stepper.el has supplied the command symbols.

;; turn-stepper.el has supplied the command symbols.
(declare-function turn-stepper-next "turn-stepper")
(declare-function turn-stepper-previous "turn-stepper")
(declare-function turn-stepper-refresh "turn-stepper")
(declare-function turn-stepper-visit-turn "turn-stepper")
(declare-function turn-stepper-rewind "turn-stepper")

(defvar turn-stepper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'turn-stepper-next)
    (define-key map (kbd "p") #'turn-stepper-previous)
    (define-key map (kbd "g") #'turn-stepper-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'turn-stepper-visit-turn)
    (define-key map (kbd "r") #'turn-stepper-rewind)
    map)
  "Keymap for `turn-stepper-mode' (planted pre-1253a8cc shape).")

;;; stepper-keys-inside-defvar.el ends here
