;;; arxana-vsatarcs-essay-revise-test.el --- Tests for :essay-revise action class -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-essay-revise.el' (v0.5.29 write-half).
;; Covers:
;;
;;   - Action constructor: required-key assertions; well-formed shape
;;   - Per-content metrics (mirrors queue's per-essay subset on content)
;;   - G-proxy monotonicity (stale-hits up → G down; days up → G down)
;;   - Forward-model: predict-effects plist shape + delta computation
;;   - can-propose?: presence, readability, no-op detection (the
;;     v0.5.22 recursion-safety predicate — proposing current content
;;     as its own revision returns nil; this is the recursive-self-
;;     landing equivalent for this action class)
;;   - can-execute?: stricter check including git-clean (defensive
;;     test: outside-git-repo path returns nil)

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-essay-revise)

;; ---------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------

(defmacro arxana-vsatarcs-essay-revise-test--with-essay
    (initial-content &rest body)
  "Write INITIAL-CONTENT into a temp .html file; bind `file' to its path; run BODY."
  (declare (indent 1))
  `(let* ((file (make-temp-file "vsatarcs-essay-revise-" nil ".html")))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,initial-content))
           ,@body)
       (ignore-errors (delete-file file)))))

;; ---------------------------------------------------------------------
;; Action constructor
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-essay-revise-action-shape ()
  (let ((a (arxana-vsatarcs-essay-revise-make-action
            :target-file "/tmp/x.html"
            :proposed-content "<html>new</html>"
            :match-type :clean
            :source-evidence-id "queue-snapshot:0"
            :rationale "test")))
    (should (eq :essay-revise (plist-get a :type)))
    (should (string= "/tmp/x.html" (plist-get a :target-file)))
    (should (string= "<html>new</html>" (plist-get a :proposed-content)))
    (should (eq :clean (plist-get a :match-type)))))

(ert-deftest arxana-vsatarcs-essay-revise-action-rejects-bad-match-type ()
  (should-error
   (arxana-vsatarcs-essay-revise-make-action
    :target-file "/tmp/x.html"
    :proposed-content "<html>new</html>"
    :match-type :rogue
    :source-evidence-id nil
    :rationale nil)))

(ert-deftest arxana-vsatarcs-essay-revise-action-rejects-relative-path ()
  (should-error
   (arxana-vsatarcs-essay-revise-make-action
    :target-file "x.html"
    :proposed-content "<html>new</html>"
    :match-type :clean
    :source-evidence-id nil
    :rationale nil)))

;; ---------------------------------------------------------------------
;; --summarise-content
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-essay-revise-summarise-counts-xlinks ()
  (let* ((content "<a href=\"a.html\">a</a><a href=\"b.html\">b</a>")
         (m (arxana-vsatarcs-essay-revise--summarise-content content)))
    (should (= 2 (plist-get m :cross-link-density)))))

(ert-deftest arxana-vsatarcs-essay-revise-summarise-counts-stale-hits ()
  (let* ((content "Codota and DeepCode mentioned together; Andela also.")
         (m (arxana-vsatarcs-essay-revise--summarise-content content)))
    (should (= 3 (plist-get m :stale-pattern-hits)))))

(ert-deftest arxana-vsatarcs-essay-revise-summarise-detects-pkd-marker ()
  (let* ((content "How to Build a Universe — Philip K. Dick.")
         (m (arxana-vsatarcs-essay-revise--summarise-content content)))
    (should (plist-get m :has-pkd-epigraph?))))

(ert-deftest arxana-vsatarcs-essay-revise-summarise-records-size ()
  (let* ((content "abcdef")
         (m (arxana-vsatarcs-essay-revise--summarise-content content)))
    (should (= 6 (plist-get m :size-bytes)))))

;; ---------------------------------------------------------------------
;; --g-proxy monotonicity
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-essay-revise-g-proxy-stale-hits-lowers-g ()
  (let* ((low (list :cross-link-density 2 :stale-pattern-hits 0))
         (hi  (list :cross-link-density 2 :stale-pattern-hits 5))
         (g-low (arxana-vsatarcs-essay-revise--g-proxy low 10.0))
         (g-hi  (arxana-vsatarcs-essay-revise--g-proxy hi 10.0)))
    ;; More stale-hits = more demand = lower (more negative) G.
    (should (< g-hi g-low))))

(ert-deftest arxana-vsatarcs-essay-revise-g-proxy-days-lowers-g-with-xlinks ()
  (let* ((m (list :cross-link-density 5 :stale-pattern-hits 1))
         (g-fresh (arxana-vsatarcs-essay-revise--g-proxy m 0.0))
         (g-stale (arxana-vsatarcs-essay-revise--g-proxy m 60.0)))
    (should (< g-stale g-fresh))))

(ert-deftest arxana-vsatarcs-essay-revise-g-proxy-zero-when-clean ()
  (let* ((m (list :cross-link-density 0 :stale-pattern-hits 0))
         (g (arxana-vsatarcs-essay-revise--g-proxy m 100.0)))
    (should (= 0.0 g))))

;; ---------------------------------------------------------------------
;; predict-effects
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-essay-revise-predict-effects-shape ()
  (arxana-vsatarcs-essay-revise-test--with-essay
      "Codota and DeepCode era prose."
    (let* ((action (arxana-vsatarcs-essay-revise-make-action
                    :target-file file
                    :proposed-content "Modern prose, no stale refs."
                    :match-type :clean
                    :source-evidence-id nil
                    :rationale nil))
           (eff (arxana-vsatarcs-essay-revise-predict-effects action)))
      (should (plist-member eff :pre-state))
      (should (plist-member eff :predicted-post-state))
      (should (plist-member eff :predicted-delta)))))

(ert-deftest arxana-vsatarcs-essay-revise-predict-effects-well-spent-revision ()
  "Removing stale-pattern hits = G-proxy rises (less demand) = positive delta."
  (arxana-vsatarcs-essay-revise-test--with-essay
      "Codota DeepCode Andela — pre-LLM-era references."
    (let* ((action (arxana-vsatarcs-essay-revise-make-action
                    :target-file file
                    :proposed-content "Updated prose with current refs."
                    :match-type :clean
                    :source-evidence-id nil
                    :rationale nil))
           (eff (arxana-vsatarcs-essay-revise-predict-effects action))
           (delta (plist-get eff :predicted-delta)))
      ;; Pre had 3 stale hits, post has 0; delta negative (count drops).
      (should (= -3 (plist-get delta :stale-pattern-hits-delta)))
      ;; G-proxy rises toward 0 — well-spent.
      (should (> (plist-get delta :G-proxy-delta) 0)))))

(ert-deftest arxana-vsatarcs-essay-revise-predict-effects-content-size-delta ()
  (arxana-vsatarcs-essay-revise-test--with-essay
      "abc"
    (let* ((action (arxana-vsatarcs-essay-revise-make-action
                    :target-file file
                    :proposed-content "abcdef"
                    :match-type :clean
                    :source-evidence-id nil
                    :rationale nil))
           (eff (arxana-vsatarcs-essay-revise-predict-effects action))
           (delta (plist-get eff :predicted-delta)))
      (should (= 3 (plist-get delta :size-bytes-delta))))))

;; ---------------------------------------------------------------------
;; can-propose?
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-essay-revise-can-propose-distinct-content ()
  (arxana-vsatarcs-essay-revise-test--with-essay
      "<html>old</html>"
    (let ((action (arxana-vsatarcs-essay-revise-make-action
                   :target-file file
                   :proposed-content "<html>new</html>"
                   :match-type :clean
                   :source-evidence-id nil
                   :rationale nil)))
      (should (arxana-vsatarcs-essay-revise-can-propose? action)))))

(ert-deftest arxana-vsatarcs-essay-revise-can-propose-rejects-noop ()
  "Recursive-self-landing equivalent: proposing the CURRENT content as
its own revision returns nil — the structural-admissibility predicate
IS the recursion-safety property per v0.5.22's safety-property family."
  (arxana-vsatarcs-essay-revise-test--with-essay
      "<html>unchanged</html>"
    (let ((action (arxana-vsatarcs-essay-revise-make-action
                   :target-file file
                   :proposed-content "<html>unchanged</html>"
                   :match-type :clean
                   :source-evidence-id nil
                   :rationale nil)))
      (should-not (arxana-vsatarcs-essay-revise-can-propose? action)))))

(ert-deftest arxana-vsatarcs-essay-revise-can-propose-rejects-missing-target ()
  (let ((action (arxana-vsatarcs-essay-revise-make-action
                 :target-file "/tmp/does-not-exist-vsatarcs-essay.html"
                 :proposed-content "<html>new</html>"
                 :match-type :clean
                 :source-evidence-id nil
                 :rationale nil)))
    (should-not (arxana-vsatarcs-essay-revise-can-propose? action))))

(ert-deftest arxana-vsatarcs-essay-revise-can-propose-rejects-empty-content ()
  (arxana-vsatarcs-essay-revise-test--with-essay
      "<html>existing</html>"
    (let ((action (arxana-vsatarcs-essay-revise-make-action
                   :target-file file
                   :proposed-content ""
                   :match-type :clean
                   :source-evidence-id nil
                   :rationale nil)))
      (should-not (arxana-vsatarcs-essay-revise-can-propose? action)))))

;; ---------------------------------------------------------------------
;; can-execute? — stricter than can-propose?
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-essay-revise-can-execute-rejects-noop ()
  (arxana-vsatarcs-essay-revise-test--with-essay
      "<html>unchanged</html>"
    (let ((action (arxana-vsatarcs-essay-revise-make-action
                   :target-file file
                   :proposed-content "<html>unchanged</html>"
                   :match-type :clean
                   :source-evidence-id nil
                   :rationale nil)))
      (should-not (arxana-vsatarcs-essay-revise-can-execute? action)))))

(ert-deftest arxana-vsatarcs-essay-revise-can-execute-rejects-non-git-target ()
  "Temp-file targets live outside a git repo; can-execute? must say nil
defensively (don't write outside version control)."
  (arxana-vsatarcs-essay-revise-test--with-essay
      "<html>old</html>"
    (let ((action (arxana-vsatarcs-essay-revise-make-action
                   :target-file file
                   :proposed-content "<html>new</html>"
                   :match-type :clean
                   :source-evidence-id nil
                   :rationale nil)))
      (should-not (arxana-vsatarcs-essay-revise-can-execute? action)))))

;; ---------------------------------------------------------------------
;; EFE wiring — :essay-revise dispatches to per-class pragmatic helper
;; ---------------------------------------------------------------------

(require 'arxana-vsatarcs-efe)

(ert-deftest arxana-vsatarcs-essay-revise-efe-dispatch-on-essay-revise ()
  "v0.5.29 wiring: :essay-revise routes to its per-class pragmatic helper
rather than the fallback 0.0.  Fixture essay carries 3 stale-pattern
hits so the pragmatic term is strictly negative — proving the dispatch
arm fires."
  (arxana-vsatarcs-essay-revise-test--with-essay
      "Codota DeepCode Andela <a href=\"a.html\">a</a>"
    (let* ((action (arxana-vsatarcs-essay-revise-make-action
                    :target-file file
                    :proposed-content "Modern <a href=\"a.html\">a</a>"
                    :match-type :clean
                    :source-evidence-id nil
                    :rationale nil))
           (g (arxana-vsatarcs-efe-compute action)))
      (should (numberp (plist-get g :G-pragmatic)))
      (should (< (plist-get g :G-pragmatic) 0.0)))))

(provide 'arxana-vsatarcs-essay-revise-test)
;;; arxana-vsatarcs-essay-revise-test.el ends here
