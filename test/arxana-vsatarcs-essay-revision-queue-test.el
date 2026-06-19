;;; arxana-vsatarcs-essay-revision-queue-test.el --- Tests for essay revision queue -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-essay-revision-queue.el'.  Covers:
;;
;;   - File filtering via excluded-basenames
;;   - Per-essay metric computation (mtime, size, cross-link, stale-hits, PKD)
;;   - Snapshot integration with temp corpus directory
;;   - G-proxy sort order (highest-priority first)
;;   - Edge cases: empty corpus; unreadable directory; all-excluded

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-essay-revision-queue)

;; ---------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------

(defmacro arxana-vsatarcs-essay-revision-queue-test--with-corpus
    (essays &rest body)
  "Create a temp corpus dir + write ESSAYS as (BASENAME . CONTENT) pairs."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "vsatarcs-erq-" t))
          (arxana-vsatarcs-essay-revision-queue-corpus-directory
           (file-name-as-directory dir))
          ;; Tests override excludes to operate on just the fixture files.
          (arxana-vsatarcs-essay-revision-queue-excluded-basenames '()))
     (unwind-protect
         (progn
           (dolist (e ,essays)
             (with-temp-file (expand-file-name (car e) dir)
               (insert (cdr e))))
           ,@body)
       (dolist (f (directory-files dir t "\\.html\\'"))
         (when (file-exists-p f) (delete-file f)))
       (ignore-errors (delete-directory dir)))))

;; ---------------------------------------------------------------------
;; --cross-link-count
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-essay-revision-queue-cross-link-counts-html-refs ()
  (let ((text "<a href=\"about.html\">about</a> and <a href=\"faq.html\">faq</a>"))
    (should (= 2 (arxana-vsatarcs-essay-revision-queue--cross-link-count text)))))

(ert-deftest arxana-vsatarcs-essay-revision-queue-cross-link-ignores-external ()
  (let ((text "<a href=\"https://example.com/page.html\">external</a>"))
    (should (= 0 (arxana-vsatarcs-essay-revision-queue--cross-link-count text)))))

(ert-deftest arxana-vsatarcs-essay-revision-queue-cross-link-counts-hyperreal-domain ()
  (let ((text "<a href=\"https://hyperreal.enterprises/about.html\">about</a>"))
    (should (= 1 (arxana-vsatarcs-essay-revision-queue--cross-link-count text)))))

(ert-deftest arxana-vsatarcs-essay-revision-queue-cross-link-empty-text ()
  (should (= 0 (arxana-vsatarcs-essay-revision-queue--cross-link-count "")))
  (should (null (arxana-vsatarcs-essay-revision-queue--cross-link-count nil))))

;; ---------------------------------------------------------------------
;; --stale-pattern-hits
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-essay-revision-queue-stale-pattern-techcrunch-funding ()
  (let ((text "see https://techcrunch.com/2020/04/27/codota-picks-up-12m-funding/"))
    (should (>= (arxana-vsatarcs-essay-revision-queue--stale-pattern-hits text)
                1))))

(ert-deftest arxana-vsatarcs-essay-revision-queue-stale-pattern-companies ()
  (let ((text "Codota recently raised; DeepCode similarly; Andela in Africa."))
    ;; Three matches for the company name pattern.
    (should (= 3 (arxana-vsatarcs-essay-revision-queue--stale-pattern-hits text)))))

(ert-deftest arxana-vsatarcs-essay-revision-queue-stale-pattern-no-hits ()
  (let ((text "Modern essay with no stale-era markers."))
    (should (= 0 (arxana-vsatarcs-essay-revision-queue--stale-pattern-hits text)))))

(ert-deftest arxana-vsatarcs-essay-revision-queue-stale-pattern-count-stable ()
  ;; Cardinality guard — adding/removing patterns is a closure-worthy
  ;; schema move.
  (should (= 4 (length arxana-vsatarcs-essay-revision-queue--stale-patterns))))

;; ---------------------------------------------------------------------
;; --has-pkd-marker?
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-essay-revision-queue-pkd-marker-quoted ()
  (should (arxana-vsatarcs-essay-revision-queue--has-pkd-marker?
           "Philip K. Dick, 1978, How to Build a Universe That Doesn't Fall Apart Two Days Later.")))

(ert-deftest arxana-vsatarcs-essay-revision-queue-pkd-marker-disneyland-form ()
  (should (arxana-vsatarcs-essay-revision-queue--has-pkd-marker?
           "Imagine the horror the Disneyland officials would feel")))

(ert-deftest arxana-vsatarcs-essay-revision-queue-pkd-marker-absent ()
  (should-not (arxana-vsatarcs-essay-revision-queue--has-pkd-marker?
               "Some other text entirely.")))

;; ---------------------------------------------------------------------
;; --summarise-essay
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-essay-revision-queue-summarise-fresh-essay ()
  (arxana-vsatarcs-essay-revision-queue-test--with-corpus
      '(("about.html" . "<h1>About</h1><p>fresh content</p>"))
    (let* ((path (expand-file-name "about.html"
                                   arxana-vsatarcs-essay-revision-queue-corpus-directory))
           (s (arxana-vsatarcs-essay-revision-queue--summarise-essay path)))
      (should (equal "about.html" (plist-get s :essay-basename)))
      (should (numberp (plist-get s :days-since-mtime)))
      (should (numberp (plist-get s :size-bytes)))
      (should (= 0 (plist-get s :cross-link-density)))
      (should (= 0 (plist-get s :stale-pattern-hits)))
      (should-not (plist-get s :has-pkd-epigraph?))
      (should (numberp (plist-get s :G-proxy))))))

(ert-deftest arxana-vsatarcs-essay-revision-queue-summarise-pkd-anchored ()
  (arxana-vsatarcs-essay-revision-queue-test--with-corpus
      '(("about.html" . "<h1>About</h1><p>...Philip K. Dick, 1978...</p>"))
    (let* ((path (expand-file-name "about.html"
                                   arxana-vsatarcs-essay-revision-queue-corpus-directory))
           (s (arxana-vsatarcs-essay-revision-queue--summarise-essay path)))
      (should (plist-get s :has-pkd-epigraph?)))))

(ert-deftest arxana-vsatarcs-essay-revision-queue-summarise-stale-essay ()
  (arxana-vsatarcs-essay-revision-queue-test--with-corpus
      '(("faq.html" .
         "Codota recently raised; <a href=\"about.html\">see about</a>"))
    (let* ((path (expand-file-name "faq.html"
                                   arxana-vsatarcs-essay-revision-queue-corpus-directory))
           (s (arxana-vsatarcs-essay-revision-queue--summarise-essay path)))
      (should (>= (plist-get s :stale-pattern-hits) 1))
      (should (= 1 (plist-get s :cross-link-density)))
      ;; G-proxy = -(stale-hits × (1+xlinks) × (1 + days/30) × 0.1).
      ;; With stale=1, xlinks=1, days≈0 → G = -0.2.
      (should (< (plist-get s :G-proxy) 0.0)))))

;; ---------------------------------------------------------------------
;; Snapshot integration
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-essay-revision-queue-snapshot-shape ()
  (arxana-vsatarcs-essay-revision-queue-test--with-corpus
      '(("about.html" . "<h1>About</h1><p>Philip K. Dick.</p>")
        ("faq.html" . "<a href=\"about.html\">x</a> Codota Codota"))
    (let ((snap (arxana-vsatarcs-essay-revision-queue-snapshot)))
      (should (plist-get snap :corpus-loaded?))
      (should (= 2 (plist-get snap :total-essays)))
      (should (= 1 (plist-get snap :pkd-anchored)))
      (should (= 2 (length (plist-get snap :essays))))
      (should (stringp (plist-get snap :digest-line))))))

(ert-deftest arxana-vsatarcs-essay-revision-queue-snapshot-sort-by-g-proxy ()
  ;; Stale + cross-linked essay (lower G) should sort BEFORE fresh
  ;; essay (G near 0).
  (arxana-vsatarcs-essay-revision-queue-test--with-corpus
      '(("fresh.html" . "<h1>Fresh</h1><p>nothing stale</p>")
        ("stale.html" .
         "<a href=\"a.html\">x</a> Codota Codota Codota Andela"))
    (let* ((snap (arxana-vsatarcs-essay-revision-queue-snapshot))
           (essays (plist-get snap :essays))
           (top (plist-get (car essays) :essay-basename)))
      ;; Stale one sorts first because more negative G.
      (should (equal "stale.html" top)))))

(ert-deftest arxana-vsatarcs-essay-revision-queue-snapshot-excludes-named ()
  (arxana-vsatarcs-essay-revision-queue-test--with-corpus
      '(("essay.html" . "<h1>Essay</h1>")
        ("excluded.html" . "<h1>Excluded</h1>"))
    (let ((arxana-vsatarcs-essay-revision-queue-excluded-basenames
           '("excluded.html")))
      (let ((snap (arxana-vsatarcs-essay-revision-queue-snapshot)))
        (should (= 1 (plist-get snap :total-essays)))
        (should (equal "essay.html"
                       (plist-get (car (plist-get snap :essays))
                                  :essay-basename)))))))

(ert-deftest arxana-vsatarcs-essay-revision-queue-snapshot-empty-corpus ()
  (let ((arxana-vsatarcs-essay-revision-queue-corpus-directory "/nonexistent-corpus/"))
    (let ((snap (arxana-vsatarcs-essay-revision-queue-snapshot)))
      (should-not (plist-get snap :corpus-loaded?))
      (should (= 0 (plist-get snap :total-essays))))))

;; ---------------------------------------------------------------------
;; Live smoke (defensive — only when futon7a exists)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-essay-revision-queue-live-smoke ()
  ;; When futon7a/ is on-disk: verify the queue picks faq.html or one
  ;; of the grant essays as top-priority (those have known stale-era
  ;; content per the 2026-05-20 audit).
  (when (file-directory-p (expand-file-name "~/code/futon7a/"))
    (let* ((snap (arxana-vsatarcs-essay-revision-queue-snapshot))
           (top (and (plist-get snap :essays)
                     (plist-get (car (plist-get snap :essays))
                                :essay-basename))))
      (should (plist-get snap :corpus-loaded?))
      (should (> (plist-get snap :total-essays) 0))
      ;; About Us carries the PKD epigraph today; the pkd-anchored
      ;; count should be >= 1 (could be 2 once the Operator's Foreword
      ;; lands and references PKD as well).
      (should (>= (plist-get snap :pkd-anchored) 1))
      (should (stringp top)))))

(provide 'arxana-vsatarcs-essay-revision-queue-test)
;;; arxana-vsatarcs-essay-revision-queue-test.el ends here
