;;; arxana-vsatarcs-effective-sign-test.el --- Tests for effective-sign projection -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-effective-sign.el' (R3 effective-sign
;; projection per the v0.5.13 audit-row forward-pointer).  Covers:
;;
;;   - --project-table for healthy-dominant / unhealthy-dominant / tied
;;   - VSATARCs-channels iteration against the shipped weight tables
;;   - --sign-counts aggregation
;;   - Full snapshot integration
;;   - WM-side defcustom shape stability (2026-05-20 baseline)

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-effective-sign)

;; ---------------------------------------------------------------------
;; --project-table
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-effective-sign-project-healthy-dominant ()
  ;; strengthened/addressed high, foreclosed/falsified low → +1.
  (let ((table '((strengthened . 0.9) (addressed . 0.8)
                 (refined . 0.5) (spawned . 0.4) (reopened . 0.5)
                 (foreclosed . 0.1) (falsified . 0.05))))
    (should (= 1 (arxana-vsatarcs-effective-sign--project-table table)))))

(ert-deftest arxana-vsatarcs-effective-sign-project-unhealthy-dominant ()
  ;; foreclosed/falsified high, strengthened/addressed low → -1.
  ;; Mirrors WM's :sorry-count-norm shape.
  (let ((table '((strengthened . 0.1) (addressed . 0.1)
                 (refined . 0.2) (spawned . 0.3) (reopened . 0.2)
                 (foreclosed . 0.8) (falsified . 0.7))))
    (should (= -1 (arxana-vsatarcs-effective-sign--project-table table)))))

(ert-deftest arxana-vsatarcs-effective-sign-project-tied ()
  ;; healthy and unhealthy max values are equal → 0 (ambiguous).
  (let ((table '((strengthened . 0.5) (addressed . 0.3)
                 (foreclosed . 0.5) (falsified . 0.4))))
    (should (= 0 (arxana-vsatarcs-effective-sign--project-table table)))))

(ert-deftest arxana-vsatarcs-effective-sign-project-missing-status-treated-as-zero ()
  ;; Table missing one of healthy/unhealthy statuses → weight 0 for it.
  (let ((healthy-only-table '((strengthened . 0.5) (addressed . 0.3))))
    (should (= 1 (arxana-vsatarcs-effective-sign--project-table
                  healthy-only-table))))
  (let ((unhealthy-only-table '((foreclosed . 0.5) (falsified . 0.3))))
    (should (= -1 (arxana-vsatarcs-effective-sign--project-table
                   unhealthy-only-table)))))

(ert-deftest arxana-vsatarcs-effective-sign-project-takes-max-not-sum ()
  ;; Healthy max = 0.6, unhealthy max = 0.5 → +1 (despite unhealthy
  ;; sum being higher if both unhealthy entries sum to 0.5+0.5=1.0).
  (let ((table '((strengthened . 0.6) (addressed . 0.3)
                 (foreclosed . 0.5) (falsified . 0.5))))
    (should (= 1 (arxana-vsatarcs-effective-sign--project-table table)))))

;; ---------------------------------------------------------------------
;; --weight-of defensiveness
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-effective-sign-weight-of-present ()
  (should (= 0.7 (arxana-vsatarcs-effective-sign--weight-of
                  '((strengthened . 0.7) (addressed . 0.3))
                  'strengthened))))

(ert-deftest arxana-vsatarcs-effective-sign-weight-of-missing-zero ()
  (should (= 0.0 (arxana-vsatarcs-effective-sign--weight-of
                  '((strengthened . 0.7))
                  'foreclosed))))

;; ---------------------------------------------------------------------
;; VSATARCs-channels iteration against shipped tables
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-effective-sign-vsatarcs-channels-count ()
  ;; The likelihood module ships 3 channels with weight tables;
  ;; the other 2 (`:scene-density', `:link-density') are :prototyping-forward
  ;; with no table → omitted.
  (should (= 3 (length (arxana-vsatarcs-effective-sign-vsatarcs-channels)))))

(ert-deftest arxana-vsatarcs-effective-sign-vsatarcs-all-positive ()
  ;; All 3 shipped VSATARCs weight tables encode the "more observation
  ;; → healthier" intent (strengthened > foreclosed); every channel
  ;; projects to +1.  Tests the directional contract of the shipped
  ;; weight tables themselves — if a future learned-weights pass
  ;; produces a different sign, this test surfaces it as a flagged
  ;; directional shift.
  (let ((chs (arxana-vsatarcs-effective-sign-vsatarcs-channels)))
    (dolist (entry chs)
      (should (= 1 (cdr entry))))))

(ert-deftest arxana-vsatarcs-effective-sign-vsatarcs-channels-named ()
  ;; Channel keys are the keyword form (matches likelihood module).
  (let ((ks (mapcar #'car (arxana-vsatarcs-effective-sign-vsatarcs-channels))))
    (dolist (k '(:story-coverage :lift-freshness :annotation-overlay-presence))
      (should (member k ks)))))

;; ---------------------------------------------------------------------
;; Declared-vs-derived consistency
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-effective-sign-declared-vs-derived-all-match ()
  ;; Today every shipped channel has matching declared (+1) and
  ;; derived (+1 from weight tables) signs.  Surfaces any future
  ;; drift between the author-declared `channel-health-signs' and the
  ;; weight tables.
  (let ((rows (arxana-vsatarcs-effective-sign-vsatarcs-declared-vs-derived)))
    (dolist (row rows)
      (should (plist-get row :match?)))))

(ert-deftest arxana-vsatarcs-effective-sign-declared-vs-derived-shape ()
  (let ((rows (arxana-vsatarcs-effective-sign-vsatarcs-declared-vs-derived)))
    (should (= 3 (length rows)))
    (dolist (row rows)
      (should (keywordp (plist-get row :channel)))
      (should (numberp (plist-get row :declared)))
      (should (numberp (plist-get row :derived)))
      (should (booleanp (plist-get row :match?))))))

;; ---------------------------------------------------------------------
;; --sign-counts
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-effective-sign-sign-counts-mix ()
  (let ((c (arxana-vsatarcs-effective-sign--sign-counts
            '((a . 1) (b . 1) (c . -1) (d . 0)))))
    (should (= 2 (plist-get c :pos)))
    (should (= 1 (plist-get c :neg)))
    (should (= 1 (plist-get c :zero)))
    (should (= 4 (plist-get c :total)))))

(ert-deftest arxana-vsatarcs-effective-sign-sign-counts-empty ()
  (let ((c (arxana-vsatarcs-effective-sign--sign-counts '())))
    (should (= 0 (plist-get c :total)))
    (should (= 0 (plist-get c :pos)))))

;; ---------------------------------------------------------------------
;; WM-side defcustom shape
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-effective-sign-wm-default-includes-sorry-count-inverted ()
  ;; The doc-vocabulary point claude-2 surfaced 2026-05-20: WM's
  ;; :sorry-count-norm is the canonical -1 example (more sorrys =
  ;; unhealthier observation).  Guard that this stays in the default.
  (should (= -1 (cdr (assoc :sorry-count-norm
                            arxana-vsatarcs-effective-sign-wm-channels)))))

(ert-deftest arxana-vsatarcs-effective-sign-wm-default-mostly-positive ()
  ;; All WM channels except :sorry-count-norm should be +1 in the
  ;; default (the "natural" health-as-positive direction).
  (let ((non-sorry (cl-remove-if (lambda (e)
                                   (eq (car e) :sorry-count-norm))
                                 arxana-vsatarcs-effective-sign-wm-channels)))
    (dolist (entry non-sorry)
      (should (= 1 (cdr entry))))))

;; ---------------------------------------------------------------------
;; Snapshot integration
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-effective-sign-snapshot-shape ()
  (let* ((snap (arxana-vsatarcs-effective-sign-snapshot))
         (v-chs (plist-get snap :vsatarcs-channels))
         (w-chs (plist-get snap :wm-channels)))
    (should (= 3 (length v-chs)))
    (should (>= (length w-chs) 9))  ; 10 default WM channels declared
    (should (plist-get (plist-get snap :vsatarcs-counts) :pos))
    (should (plist-get (plist-get snap :wm-counts) :pos))
    (should (stringp (plist-get snap :digest-line)))))

(ert-deftest arxana-vsatarcs-effective-sign-snapshot-digest-line ()
  (let ((digest (plist-get (arxana-vsatarcs-effective-sign-snapshot)
                           :digest-line)))
    ;; VSATARCs side: 3 channels all +1.
    (should (string-match-p "VSATARCs: 3\\+" digest))
    ;; WM side: has 1 -1 channel (sorry-count-norm).
    (should (string-match-p "1-" digest))))

(ert-deftest arxana-vsatarcs-effective-sign-snapshot-honors-custom-wm-config ()
  ;; Operator can override the WM-side channels (e.g., when claude-2
  ;; ships a refreshed set); the snapshot reflects the override.
  (let ((arxana-vsatarcs-effective-sign-wm-channels
         '((:foo . +1) (:bar . -1))))
    (let ((snap (arxana-vsatarcs-effective-sign-snapshot)))
      (should (= 2 (plist-get (plist-get snap :wm-counts) :total)))
      (should (= 1 (plist-get (plist-get snap :wm-counts) :pos)))
      (should (= 1 (plist-get (plist-get snap :wm-counts) :neg))))))

(provide 'arxana-vsatarcs-effective-sign-test)
;;; arxana-vsatarcs-effective-sign-test.el ends here
