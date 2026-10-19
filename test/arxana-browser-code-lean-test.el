;;; arxana-browser-code-lean-test.el --- Lean transclusion in code docs -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (load-file (expand-file-name "dev/arxana-docbook-core.el" root))
  (load-file (expand-file-name "dev/arxana-browser-code.el" root)))

(defconst arxana-browser-code-lean-test--code
  "(defn route! [opts] nil)\n(defn production-readiness [opts] nil)\n")

(defconst arxana-browser-code-lean-test--lean
  (concat "/-! Module docstring citing `route!`; not a declaration. -/\n\n"
          "/-- The transition table. -/\n"
          "def transitions : List Nat :=\n"
          "  [ -- `route!` refuses here\n    1 ]\n\n"
          "/-- No path falls back, by `route!` and `production-readiness`. More text. -/\n"
          "theorem noFallback : True := trivial\n\n"
          "/-- Mentions `route` and `not-in-file!` only. -/\n"
          "def other : Nat := 0\n"))

(defconst arxana-browser-code-lean-test--entry
  (list :doc-id "lean-fixture" :version "def-doc"
        :summary-raw "- route!: Routes the click.\n- production-readiness: Checks inputs.\n"))

(defun arxana-browser-code-lean-test--render (dir lean-sources)
  (let* ((path (expand-file-name "meta_self_heal_production.clj" dir))
         (arxana-browser-code-lean-sources lean-sources)
         (arxana-browser-code-docs-buffer "*arxana-lean-test-docs*")
         (arxana-browser-code--symbol-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'arxana-docbook--entry-content) (lambda (_) nil)))
      (with-current-buffer (arxana-browser-code--render-docs path (list arxana-browser-code-lean-test--entry))
        (prog1 (cons (buffer-string) (copy-sequence arxana-browser-code--doc-symbol-map))
          (kill-buffer))))))

(defun arxana-browser-code-lean-test--chunk (text symbol)
  "Return the context chunk for SYMBOL in rendered TEXT."
  (when (string-match (concat "^- " (regexp-quote symbol) "\n\\(\\(?:  .*\n\\)*\\)") text)
    (match-string 1 text)))

(ert-deftest arxana-browser-code-lean-transcludes-citing-declarations ()
  (let* ((dir (make-temp-file "arxana-lean-" t))
         (lean (expand-file-name "SelfHealRoute.lean" dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "meta_self_heal_production.clj" dir)
            (insert arxana-browser-code-lean-test--code))
          (with-temp-file lean (insert arxana-browser-code-lean-test--lean))
          (let* ((rendered (arxana-browser-code-lean-test--render
                            dir (list (cons "meta_self_heal_production\\.clj\\'" (list lean)))))
                 (text (car rendered))
                 (route (arxana-browser-code-lean-test--chunk text "route!"))
                 (ready (arxana-browser-code-lean-test--chunk text "production-readiness")))
            (should (string-match-p "^  Lean transitions: The transition table\\.$" route))
            (should (string-match-p "^  Lean noFallback: No path falls back, by route! and production-readiness\\.$" route))
            (should (string-match-p "^  Lean noFallback" ready))
            (should-not (string-match-p "Lean transitions" ready))
            ;; Bad cases: a near-miss name (`route') and a symbol the file does
            ;; not define (`not-in-file!') cite nothing; a module docstring is
            ;; not a declaration.
            (should-not (string-match-p "Lean other" text))
            (should-not (string-match-p "Module docstring" text))
            ;; The sync map still points each symbol at its own bullet, even
            ;; though `route!' appears earlier inside a transcluded Lean line.
            (let ((bounds (cdr (assoc "production-readiness" (cdr rendered)))))
              (should bounds)
              (should (string-prefix-p "- production-readiness"
                                       (substring text (1- (car bounds))))))))
      (delete-directory dir t))))

(ert-deftest arxana-browser-code-lean-face-only-on-lean-lines ()
  (let* ((dir (make-temp-file "arxana-lean-" t))
         (lean (expand-file-name "SelfHealRoute.lean" dir))
         (path (expand-file-name "meta_self_heal_production.clj" dir)))
    (unwind-protect
        (progn
          (with-temp-file path (insert arxana-browser-code-lean-test--code))
          (with-temp-file lean (insert arxana-browser-code-lean-test--lean))
          (let ((arxana-browser-code-lean-sources (list (cons "\\.clj\\'" (list lean))))
                (arxana-browser-code--symbol-cache (make-hash-table :test 'equal)))
            (with-temp-buffer
              (arxana-browser-code--insert-context-items
               path (arxana-browser-code--entry-context-items arxana-browser-code-lean-test--entry))
              (goto-char (point-min))
              (search-forward "  Lean noFallback")
              (should (eq 'arxana-browser-code-docs-lean-face (get-text-property (point) 'face)))
              (should (equal lean (get-text-property (point) 'arxana-lean-file)))
              (goto-char (point-min))
              (search-forward "Routes the click")
              (should-not (get-text-property (point) 'face)))))
      (delete-directory dir t))))

(ert-deftest arxana-browser-code-lean-off-renders-as-before ()
  (let ((dir (make-temp-file "arxana-lean-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "meta_self_heal_production.clj" dir)
            (insert arxana-browser-code-lean-test--code))
          (let ((text (car (arxana-browser-code-lean-test--render dir nil))))
            (should (string-match-p
                     (regexp-quote "* Context\n- route!\n  Routes the click.\n\n- production-readiness\n  Checks inputs.\n\n")
                     text))
            (should-not (string-match-p "Lean " text))))
      (delete-directory dir t))))

(provide 'arxana-browser-code-lean-test)
;;; arxana-browser-code-lean-test.el ends here
