;;; arxana-browser-annotations-test.el --- Tests for shared annotation helpers -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'arxana-browser-annotations)

(ert-deftest arxana-browser-annotations-value-name-normalizes-values ()
  (should (equal "annotation/supports"
                 (arxana-browser-annotations-value-name :annotation/supports)))
  (should (equal "annotated"
                 (arxana-browser-annotations-value-name 'annotated)))
  (should (equal "plain"
                 (arxana-browser-annotations-value-name "plain")))
  (should-not (arxana-browser-annotations-value-name nil)))

(ert-deftest arxana-browser-annotations-endpoint-accessors-handle-keywords ()
  (let ((endpoint '((:role . :annotated)
                    (:entity-id . "song:abi")
                    (:passage . "line 7: Heavy, the sleep of butterflies"))))
    (should (equal "annotated"
                   (arxana-browser-annotations-endpoint-role endpoint)))
    (should (equal "song:abi"
                   (arxana-browser-annotations-endpoint-entity-id endpoint)))
    (should (equal "line 7: Heavy, the sleep of butterflies"
                   (arxana-browser-annotations-endpoint-passage endpoint)))))

(ert-deftest arxana-browser-annotations-line-bounds-parse-single-and-range ()
  (should (equal '(7 . 7)
                 (arxana-browser-annotations-line-bounds-from-passage
                  "line 7: Heavy, the sleep of butterflies")))
  (should (equal '(1 . 4)
                 (arxana-browser-annotations-line-bounds-from-passage
                  "lines 1-4: Blue carnations, in the soil")))
  (should-not (arxana-browser-annotations-line-bounds-from-passage
               "the butterfly craves release to the open sky")))

(ert-deftest arxana-browser-annotations-passage-search-text-strips-line-prefix ()
  (should (equal "Blue carnations, in the soil"
                 (arxana-browser-annotations-passage-search-text
                  "lines 1-4: Blue carnations, in the soil")))
  (should (equal "the butterfly craves release to the open sky"
                 (arxana-browser-annotations-passage-search-text
                  "the butterfly craves release to the open sky"))))

(ert-deftest arxana-browser-annotations-passage-line-count-uses-range-width ()
  (should (= 1
             (arxana-browser-annotations-passage-line-count
              "line 7: Heavy, the sleep of butterflies")))
  (should (= 4
             (arxana-browser-annotations-passage-line-count
              "lines 1-4: Blue carnations, in the soil")))
  (should-not (arxana-browser-annotations-passage-line-count
               "the butterfly craves release to the open sky")))

;;; arxana-browser-annotations-test.el ends here
