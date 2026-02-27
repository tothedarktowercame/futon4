;;; arxana-browser-hypergraph-test.el --- Tests for hypergraph browser -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'arxana-browser-core)
(require 'arxana-browser-hypergraph)

(defun arxana-browser-hypergraph-test--sample-json (&optional use-hyperedges)
  (if use-hyperedges
      "{\"thread_id\":633512,\"nodes\":[{\"id\":\"n1\",\"type\":\"term\"}],\"hyperedges\":[{\"type\":\"cooccurs\",\"ends\":[\"n1\",\"n2\",\"n3\"]}]}"
    "{\"thread_id\":633512,\"nodes\":[{\"id\":\"n1\",\"type\":\"term\"},{\"id\":\"n2\",\"type\":\"expression\",\"attrs\":{\"latex\":\"x+y\"}}],\"edges\":[{\"type\":\"iatc\",\"ends\":[\"n1\",\"n2\"]},{\"type\":\"supports\",\"ends\":[\"n2\"]}]}"))

(defun arxana-browser-hypergraph-test--lens-json ()
  "{\"thread_id\":633512,\
\"nodes\":[\
{\"id\":\"p1\",\"type\":\"post\",\"subtype\":\"question\",\"attrs\":{\"score\":0.9}},\
{\"id\":\"p2\",\"type\":\"post\",\"subtype\":\"answer\",\"attrs\":{\"is_accepted\":true}},\
{\"id\":\"e1\",\"type\":\"expression\",\"attrs\":{\"latex\":\"\\\\forall x \\\\to y\",\"sexp\":\"(forall x (-> x y))\"}},\
{\"id\":\"s1\",\"type\":\"scope\",\"subtype\":\"scope/let\",\"attrs\":{\"ends\":[{\"role\":\"symbol\",\"latex\":\"x\"},{\"role\":\"description\",\"text\":\"Nat\"}]}},\
{\"id\":\"t1\",\"type\":\"term\",\"attrs\":{\"name\":\"Graph\"}}\
],\
\"edges\":[\
{\"type\":\"scope\",\"ends\":[\"s1\",\"p1\"],\"attrs\":{\"binding_type\":\"scope/let\"}},\
{\"type\":\"discourse\",\"ends\":[\"p1\",\"p2\"],\"attrs\":{\"role\":\"wire\",\"dtype\":\"disc/reason\",\"match\":\"because\"}},\
{\"type\":\"categorical\",\"ends\":[\"p1\"],\"attrs\":{\"concept\":\"cat/proof\",\"score\":0.91}},\
{\"type\":\"surface\",\"ends\":[\"e1\",\"p1\"]},\
{\"type\":\"mention\",\"ends\":[\"p1\",\"t1\"]},\
{\"type\":\"iatc\",\"ends\":[\"p1\",\"p2\"],\"attrs\":{\"act\":\"challenge\"}}\
]}")

(ert-deftest arxana-browser-hypergraph-items-read-source ()
  (let* ((path (make-temp-file "arxana-hg-" nil ".json"
                               (arxana-browser-hypergraph-test--sample-json)))
         (arxana-browser-hypergraph-sources (list path))
         (items (arxana-browser-hypergraph-items))
         (item (car items)))
    (unwind-protect
        (progn
          (should (= (length items) 1))
          (should (eq (plist-get item :type) 'hypergraph-source))
          (should (string= (plist-get item :status) "ok"))
          (should (equal (plist-get item :thread) "633512"))
          (should (= (plist-get item :nodes) 2))
          (should (= (plist-get item :edges) 2)))
      (ignore-errors (delete-file path)))))

(ert-deftest arxana-browser-hypergraph-items-support-hyperedges-key ()
  (let* ((path (make-temp-file "arxana-hg-" nil ".json"
                               (arxana-browser-hypergraph-test--sample-json t)))
         (arxana-browser-hypergraph-sources (list path))
         (item (car (arxana-browser-hypergraph-items))))
    (unwind-protect
        (progn
          (should (string= (plist-get item :status) "ok"))
          (should (= (plist-get item :edges) 1)))
      (ignore-errors (delete-file path)))))

(ert-deftest arxana-browser-hypergraph-items-handle-missing-path ()
  (let* ((missing (make-temp-name "/tmp/arxana-hg-missing-"))
         (arxana-browser-hypergraph-sources (list missing))
         (item (car (arxana-browser-hypergraph-items))))
    (should (eq (plist-get item :type) 'hypergraph-source))
    (should (string= (plist-get item :status) "missing"))
    (should (= (plist-get item :nodes) 0))
    (should (= (plist-get item :edges) 0))))

(ert-deftest arxana-browser-hypergraph-open-renders-summary ()
  (let* ((path (make-temp-file "arxana-hg-" nil ".json"
                               (arxana-browser-hypergraph-test--sample-json)))
         (arxana-browser-hypergraph-sources (list path))
         (arxana-browser-hypergraph-open-style 'summary)
         (item (car (arxana-browser-hypergraph-items)))
         (buffer-name "*Arxana Hypergraph*"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
            (arxana-browser-hypergraph-open item))
          (with-current-buffer buffer-name
            (should (string-match-p "Thread ID: 633512"
                                    (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "Node Types"
                                    (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "Thread Lenses (Emacs Native)"
                                    (buffer-substring-no-properties (point-min) (point-max))))))
      (ignore-errors (delete-file path))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest arxana-browser-hypergraph-default-open-style-is-summary ()
  (should (eq arxana-browser-hypergraph-open-style 'summary)))

(ert-deftest arxana-browser-hypergraph-summary-renders-native-thread-lenses ()
  (let* ((path (make-temp-file "arxana-hg-" nil ".json"
                               (arxana-browser-hypergraph-test--lens-json)))
         (arxana-browser-hypergraph-sources (list path))
         (arxana-browser-hypergraph-open-style 'summary)
         (item (car (arxana-browser-hypergraph-items)))
         (buffer-name "*Arxana Hypergraph*"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
            (arxana-browser-hypergraph-open item))
          (with-current-buffer buffer-name
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "Thread Lenses (Emacs Native)" content))
              (should (string-match-p "p1 \\[question\\]" content))
              (should (string-match-p "categories: proof@0.91" content))
              (should (string-match-p "scope let" content))
              (should (string-match-p "discourse wire" content))
              (should (string-match-p "expressions: .*∀ x → y" content))
              (should (string-match-p "mentions: Graph" content))
              (should (string-match-p "iatc-out p1 -> p2 (challenge)" content)))))
      (ignore-errors (delete-file path))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest arxana-browser-hypergraph-write-html-generates-atlas ()
  (let* ((path (make-temp-file "arxana-hg-" nil ".json"
                               (arxana-browser-hypergraph-test--sample-json)))
         (export-dir (make-temp-file "arxana-hg-html-" t))
         (arxana-browser-hypergraph-export-directory export-dir)
         (arxana-browser-hypergraph-sources (list path))
         (item (car (arxana-browser-hypergraph-items)))
         (html-path nil))
    (unwind-protect
        (progn
          (setq html-path (arxana-browser-hypergraph-write-html item))
          (should (file-exists-p html-path))
          (with-temp-buffer
            (insert-file-contents html-path)
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "Arxana Hypergraph Atlas" content))
              (should (string-match-p "Thread Lenses" content))
              (should (string-match-p "Hypergraph Glasses" content))
              (should (string-match-p "const DATA = JSON.parse(RAW_JSON);" content))
              (should (string-match-p "const RAW_THREAD_JSON =" content))
              (should (string-match-p "function texLite" content))
              (should (string-match-p "function renderGlasses" content))
              (should (string-match-p "function scopeRoleClass" content))
              (should (string-match-p "function discourseRoleClass" content))
              (should (string-match-p "function iatcActClass" content))
              (should (string-match-p "function postKindClass" content))
              (should (string-match-p "expr-sexp-mini" content))
              (should (string-match-p "attachSexpTips" content)))))
      (ignore-errors (delete-file path))
      (when (and html-path (file-exists-p html-path))
        (ignore-errors (delete-file html-path)))
      (when (file-directory-p export-dir)
        (ignore-errors (delete-directory export-dir t))))))

(ert-deftest arxana-browser-core-menu-includes-hypergraphs ()
  (let ((labels (mapcar (lambda (item) (plist-get item :label))
                        (arxana-browser--menu-items))))
    (should (member "Hypergraphs" labels))))

(ert-deftest arxana-browser-hypergraph-peer-summary-uses-source-buffer ()
  (with-temp-buffer
    (insert "x y x")
    (add-text-properties 1 2 (list 'arxana-peer-key "subexpr:var:x"
                                   'arxana-hypergraph-post-id "p1"))
    (add-text-properties 5 6 (list 'arxana-peer-key "subexpr:var:x"
                                   'arxana-hypergraph-post-id "p2"))
    (let ((source (current-buffer)))
      (with-temp-buffer
        (let* ((summary (arxana-browser-hypergraph--peer-summary "subexpr:var:x" source))
               (rows (plist-get summary :posts)))
          (should (= (plist-get summary :count) 2))
          (should (equal rows '(("p1" . 1) ("p2" . 1)))))))))

(ert-deftest arxana-browser-visit-dispatches-evidence-entry-types ()
  (dolist (spec '((evidence-session . session)
                  (evidence-thread . thread)
                  (evidence-entry . entry)
                  (evidence-turn . entry)))
    (let* ((item (list :type (car spec) :id "demo"))
           (expected (cdr spec))
           seen)
      (cl-letf (((symbol-function 'arxana-browser--item-at-point)
                 (lambda () item))
                ((symbol-function 'arxana-browser-evidence-open-session)
                 (lambda (arg) (setq seen (cons 'session arg))))
                ((symbol-function 'arxana-browser-evidence-open-thread)
                 (lambda (arg) (setq seen (cons 'thread arg))))
                ((symbol-function 'arxana-browser-evidence-open-entry)
                 (lambda (arg) (setq seen (cons 'entry arg)))))
        (arxana-browser--visit))
      (should (eq (car seen) expected))
      (should (equal (cdr seen) item)))))

(ert-deftest arxana-browser-visit-dispatches-lab-tension-and-devmap-items ()
  (dolist (spec '((tension-entry . tension)
                  (devmap-entry . devmap)))
    (let* ((item (list :type (car spec) :id "demo"))
           (expected (cdr spec))
           seen)
      (cl-letf (((symbol-function 'arxana-browser--item-at-point)
                 (lambda () item))
                ((symbol-function 'arxana-browser-tension-open-entry)
                 (lambda (arg) (setq seen (cons 'tension arg))))
                ((symbol-function 'arxana-browser-devmap-open-entry)
                 (lambda (arg) (setq seen (cons 'devmap arg)))))
        (arxana-browser--visit))
      (should (eq (car seen) expected))
      (should (equal (cdr seen) item)))))

(provide 'arxana-browser-hypergraph-test)
;;; arxana-browser-hypergraph-test.el ends here
