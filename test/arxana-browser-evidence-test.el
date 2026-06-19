;;; arxana-browser-evidence-test.el --- Tests for evidence browser -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'arxana-browser-evidence)
(require 'arxana-browser-core)

(defvar arxana-lab-futon1-server nil)
(defvar agent-chat-agency-base-url nil)

(ert-deftest arxana-evidence-default-server-prefers-futon1 ()
  (let ((arxana-lab-futon1-server "http://lab.example/api/alpha")
        (agent-chat-agency-base-url "http://agency.example:47070"))
    (cl-letf (((symbol-function 'getenv)
               (lambda (name)
                 (pcase name
                   ("FUTON1_API_BASE" "http://futon1.example/api/alpha")
                   (_ nil)))))
      (should (equal (arxana-evidence--default-server)
                     "http://futon1.example/api/alpha")))))

(ert-deftest arxana-evidence-default-server-falls-back-to-lab-setting ()
  (let ((arxana-lab-futon1-server "http://linode.example:7071/api/alpha")
        (agent-chat-agency-base-url "http://agency.example:47070"))
    (cl-letf (((symbol-function 'getenv) (lambda (_name) nil)))
      (should (equal (arxana-evidence--default-server)
                     "http://linode.example:7071/api/alpha")))))

(ert-deftest arxana-evidence-default-server-does-not-use-agent-chat-agency ()
  (let ((arxana-lab-futon1-server nil)
        (agent-chat-agency-base-url "http://agency.example:47070"))
    (cl-letf (((symbol-function 'getenv) (lambda (_name) nil)))
      (should (equal (arxana-evidence--default-server)
                     "http://localhost:8080/api/alpha")))))

(ert-deftest arxana-evidence-normalize-base-accepts-evidence-endpoints ()
  (should (equal (arxana-evidence--normalize-base
                  "http://127.0.0.1:7070/api/alpha/evidence")
                 "http://127.0.0.1:7070"))
  (should (equal (arxana-evidence--normalize-base
                  "http://127.0.0.1:7070/api/alpha")
                 "http://127.0.0.1:7070")))

(ert-deftest arxana-browser-core-menu-includes-open-sessions ()
  (should
   (member "Sessions"
           (mapcar (lambda (item) (plist-get item :label))
                   (arxana-browser--menu-items)))))

(ert-deftest arxana-evidence-open-sessions-format-shows-turn-count ()
  (should
   (member "Turns"
           (mapcar #'car
                   (append (arxana-browser--evidence-open-sessions-format)
                           nil)))))

(ert-deftest arxana-evidence-buffer-session-id-prefers-current-session-over-last-emitted ()
  (with-temp-buffer
    (setq-local major-mode 'codex-repl-mode)
    (setq-local codex-repl--last-emitted-session-id "sid-stale")
    (setq-local agent-chat--session-id "sid-current")
    (should (equal (arxana-evidence--buffer-session-id)
                   "sid-current"))))

(ert-deftest arxana-evidence-buffer-session-id-prefers-evidence-session-over-last-emitted ()
  (with-temp-buffer
    (setq-local major-mode 'codex-repl-mode)
    (setq-local codex-repl--last-emitted-session-id "sid-stale")
    (setq-local codex-repl--evidence-session-id "sid-evidence")
    (should (equal (arxana-evidence--buffer-session-id)
                   "sid-evidence"))))

(ert-deftest arxana-evidence-buffer-session-id-falls-back-to-last-emitted ()
  (with-temp-buffer
    (setq-local major-mode 'codex-repl-mode)
    (setq-local codex-repl--last-emitted-session-id "sid-fallback")
    (setq-local agent-chat--session-id "pending")
    (should (equal (arxana-evidence--buffer-session-id)
                   "sid-fallback"))))

(ert-deftest arxana-evidence-open-sessions-row-shows-mark ()
  (let ((arxana-evidence--open-session-marks (make-hash-table :test 'equal))
        (item (list :type 'evidence-open-session
                    :buffer "*codex-repl:marked*"
                    :agent "codex"
                    :state "idle"
                    :count 12
                    :latest "2026-04-22T12:00:00Z"
                    :missions '("M-repl-wins-over-cli")
                    :about "Testing marks")))
    (should (equal (aref (arxana-browser--evidence-open-sessions-row item) 0)
                   " "))
    (puthash "*codex-repl:marked*" t arxana-evidence--open-session-marks)
    (should (equal (aref (arxana-browser--evidence-open-sessions-row item) 0)
                   "*"))))

(ert-deftest arxana-evidence-open-sessions-toggle-mark-rerenders ()
  (let ((arxana-evidence--open-session-marks (make-hash-table :test 'equal))
        (rendered nil)
        (goto-row nil)
        (set-col-called nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id)
               (lambda ()
                 (list :type 'evidence-open-session
                       :buffer "*codex-repl:mark-safe*")))
              ((symbol-function 'arxana-browser--current-row)
               (lambda () 2))
              ((symbol-function 'arxana-browser--render)
               (lambda () (setq rendered t)))
              ((symbol-function 'arxana-browser--goto-row)
               (lambda (row) (setq goto-row row)))
              ((symbol-function 'tabulated-list-set-col)
               (lambda (&rest _)
                 (setq set-col-called t)
                 (error "tabulated-list-set-col should not be used"))))
      (arxana-evidence-toggle-open-session-mark-at-point)
      (should (gethash "*codex-repl:mark-safe*"
                       arxana-evidence--open-session-marks))
      (should rendered)
      (should (= goto-row 3))
      (should-not set-col-called))))

(ert-deftest arxana-evidence-open-sessions-cleanup-kills-buffer-and-process ()
  (let ((buf (get-buffer-create "*codex-repl:cleanup-test*"))
        (invoke-buf (get-buffer-create "*invoke: cleanup-test*"))
        (proc nil))
    (unwind-protect
        (progn
          (setq proc (start-process "arxana-session-cleanup-test" nil "sleep" "10"))
          (with-current-buffer buf
            (setq-local major-mode 'codex-repl-mode)
            (setq-local agent-chat--pending-process proc)
            (setq-local codex-repl-invoke-buffer-name "*invoke: cleanup-test*"))
          (should
           (arxana-evidence--cleanup-open-session-item
            (list :type 'evidence-open-session
                  :buffer "*codex-repl:cleanup-test*")))
          (accept-process-output proc 0.1)
          (should-not (buffer-live-p buf))
          (should-not (buffer-live-p invoke-buf))
          (should-not (process-live-p proc)))
      (when (process-live-p proc)
        (kill-process proc))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (buffer-live-p invoke-buf)
        (kill-buffer invoke-buf)))))

(ert-deftest arxana-evidence-open-sessions-summarize-from-evidence ()
  (let ((buf (get-buffer-create "*codex-repl:codex-8*"))
        (arxana-evidence-open-session-llm-summaries nil)
        fetched-params
        fetched-server
        (entries (list
                  (list :evidence/id "e2"
                        :evidence/at "2026-04-22T12:02:00Z"
                        :evidence/session-id "sid-1"
                        :evidence/author "codex"
                        :evidence/body
                        (list :event "chat-turn"
                              :role "assistant"
                              :text "Implemented futon4/dev/arxana-browser-evidence.el."))
                  (list :evidence/id "e1"
                        :evidence/at "2026-04-22T12:00:00Z"
                        :evidence/session-id "sid-1"
                        :evidence/author "joe"
                        :evidence/body
                        (list :event "chat-turn"
                              :role "user"
                              :text "Build an Arxana Browser sessions feature for M-repl-wins-over-cli.")))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local major-mode 'codex-repl-mode)
            (setq-local codex-repl-session-id "wrong-global-style-sid")
            (setq-local agent-chat--session-id "pending")
            (setq-local codex-repl--last-emitted-session-id "sid-1")
            (setq-local agent-chat--agent-name "codex")
            (setq-local agent-chat--evidence-url
                        "http://127.0.0.1:7070/api/alpha/evidence"))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda () (list buf)))
                    ((symbol-function 'arxana-evidence--fetch-evidence)
                     (lambda (params)
                       (setq fetched-params params)
                       (setq fetched-server arxana-evidence-server)
                       entries))
                    ((symbol-function 'arxana-evidence--count-open-session-turns)
                     (lambda (_session) 37)))
            (let ((items (arxana-browser--evidence-open-sessions-items)))
              (should (= (length items) 1))
              (let ((item (car items)))
                (should (eq (plist-get item :type) 'evidence-open-session))
                (should (equal (plist-get item :session-id) "sid-1"))
                (should (= (plist-get item :count) 37))
                (should (equal (plist-get item :agent) "codex"))
                (should (equal (plist-get item :missions) '("M-repl-wins-over-cli")))
                (should (string-match-p "Arxana Browser sessions"
                                        (plist-get item :about)))
                (should (member "futon4/dev/arxana-browser-evidence.el"
                                (plist-get item :artifacts)))))
            (should (equal (cdr (assoc "session-id" fetched-params)) "sid-1"))
            (should (equal fetched-server "http://127.0.0.1:7070"))
            (should (equal (cdr (assoc "limit" fetched-params))
                           arxana-evidence-open-session-entry-limit))))
      (when (get-buffer buf)
        (kill-buffer buf)))))

(ert-deftest arxana-evidence-open-sessions-queues-batched-llm-summary ()
  (let ((arxana-evidence-open-session-llm-summaries t)
        (arxana-evidence-open-session-summary-model "haiku-test")
        (arxana-evidence--open-session-summary-cache
         (make-hash-table :test 'equal))
        (arxana-evidence--open-session-summary-pending
         (make-hash-table :test 'equal))
        (arxana-evidence--open-session-summary-inflight
         (make-hash-table :test 'equal))
        worker-started)
    (cl-letf (((symbol-function 'arxana-evidence--start-open-session-summary-worker)
               (lambda () (setq worker-started t))))
      (let* ((item (list :type 'evidence-open-session
                         :buffer "*codex-repl:codex-8*"
                         :agent "codex"
                         :state "idle"
                         :session-id "sid-1"
                         :latest-id "e1"
                         :about "raw user text"
                         :missions '("M-repl-wins-over-cli")
                         :artifacts '("dev/arxana-browser-vsatarcs.el")
                         :entries nil))
             (items (arxana-evidence--apply-open-session-llm-summaries
                     (list item))))
        (should worker-started)
        (should (equal (plist-get (car items) :about)
                       "raw user text"))
        (should (equal
                 (gethash "sid-1|e1|haiku-test"
                          arxana-evidence--open-session-summary-pending)
                 (arxana-evidence--open-session-capsule item)))))))

(ert-deftest arxana-evidence-session-items-refetch-full-session-slice ()
  (let (fetched-params counted-session)
    (cl-letf (((symbol-function 'arxana-evidence--count-session-entries)
               (lambda (session-id)
                 (setq counted-session session-id)
                 714))
              ((symbol-function 'arxana-evidence--fetch-evidence)
               (lambda (params)
                 (setq fetched-params params)
                 (list (list :evidence/id "e1"
                             :evidence/type :coordination
                             :evidence/author "joe"
                             :evidence/at "2026-05-19T12:00:00Z"
                             :evidence/body
                             (list :event "chat-turn"
                                   :role "user"
                                   :text "hello"))))))
      (let ((items (arxana-browser--evidence-session-items
                    (list :session-id "sid-1"
                          :entries (list (list :evidence/id "stale"))))))
        (should (equal counted-session "sid-1"))
        (should (equal (cdr (assoc "session-id" fetched-params)) "sid-1"))
        (should (equal (cdr (assoc "limit" fetched-params)) "714"))
        (should (= 1 (length items)))
        (should (eq (plist-get (car items) :type) 'evidence-chat-turn))
        (should (equal (plist-get (car items) :text) "hello"))))))

(ert-deftest arxana-evidence-open-sessions-applies-cached-llm-summary ()
  (let ((arxana-evidence-open-session-llm-summaries t)
        (arxana-evidence-open-session-summary-model "haiku-test")
        (arxana-evidence--open-session-summary-cache
         (make-hash-table :test 'equal)))
    (puthash "sid-1|e1|haiku-test"
             "Fixing VSATARCS navigation"
             arxana-evidence--open-session-summary-cache)
    (let* ((item (list :type 'evidence-open-session
                       :buffer "*codex-repl:codex-8*"
                       :agent "codex"
                       :state "idle"
                       :session-id "sid-1"
                       :latest-id "e1"
                       :about "raw user text"
                       :missions '("M-repl-wins-over-cli")
                       :artifacts '("dev/arxana-browser-vsatarcs.el")
                       :entries nil))
           (items (arxana-evidence--apply-open-session-llm-summaries
                   (list item))))
      (should (equal (plist-get (car items) :about)
                     "Fixing VSATARCS navigation"))
      (should (plist-get (car items) :summary-ready)))))

(ert-deftest arxana-evidence-open-sessions-finishes-async-summary-batch ()
  (let ((arxana-evidence--open-session-summary-cache
         (make-hash-table :test 'equal))
        (arxana-evidence--open-session-summary-inflight
         (make-hash-table :test 'equal))
        (arxana-evidence--open-session-summary-process nil)
        (arxana-evidence--open-session-summary-timeout-timer nil)
        rerendered)
    (puthash "sid-1|e1|haiku-test" t
             arxana-evidence--open-session-summary-inflight)
    (let* ((out (generate-new-buffer " *arxana-summary-finish-test*"))
           (proc (make-process :name "arxana-summary-finish-test"
                               :buffer out
                               :command '("sh" "-c" "exit 0")
                               :connection-type 'pipe
                               :noquery t)))
      (unwind-protect
          (progn
            (with-current-buffer out
              (insert "S1\tFixing VSATARCS navigation\n"))
            (while (eq (process-status proc) 'run)
              (accept-process-output proc 0.01))
            (setq arxana-evidence--open-session-summary-process proc)
            (process-put proc :id->key '(("S1" . "sid-1|e1|haiku-test")))
            (process-put proc :started-at (float-time))
            (cl-letf (((symbol-function 'arxana-evidence--maybe-rerender-open-sessions)
                       (lambda () (setq rerendered t)))
                      ((symbol-function 'arxana-evidence--start-open-session-summary-worker)
                       (lambda () nil)))
              (arxana-evidence--finish-open-session-summary-process proc))
            (should (equal
                     (gethash "sid-1|e1|haiku-test"
                              arxana-evidence--open-session-summary-cache)
                     "Fixing VSATARCS navigation"))
            (should rerendered)
            (should-not (gethash "sid-1|e1|haiku-test"
                                 arxana-evidence--open-session-summary-inflight)))
        (when (buffer-live-p out)
          (kill-buffer out))))))

(ert-deftest arxana-evidence-open-sessions-reuses-row-cache-from-latest-id ()
  (let ((buf (get-buffer-create "*codex-repl:cached*"))
        (arxana-evidence-open-session-llm-summaries nil)
        (arxana-evidence-open-session-summary-model "haiku-cache-test")
        (arxana-evidence--open-session-item-cache
         (make-hash-table :test 'equal))
        (fetch-count 0)
        (entries (list
                  (list :evidence/id "e-cache"
                        :evidence/at "2026-04-22T12:00:00Z"
                        :evidence/session-id "sid-cache"
                        :evidence/author "joe"
                        :evidence/body
                        (list :event "chat-turn"
                              :role "user"
                              :text "Summarize the cached Sessions row.")))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local major-mode 'codex-repl-mode)
            (setq-local codex-repl--last-emitted-session-id "sid-cache")
            (setq-local codex-repl--last-evidence-id "e-buffer-hint")
            (setq-local agent-chat--agent-name "codex")
            (setq-local agent-chat--evidence-url
                        "http://127.0.0.1:7070/api/alpha/evidence"))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda () (list buf)))
                    ((symbol-function 'arxana-evidence--fetch-evidence)
                     (lambda (_params)
                       (cl-incf fetch-count)
                       entries))
                    ((symbol-function 'arxana-evidence--count-open-session-turns)
                     (lambda (_session) 43)))
            (let ((first (arxana-browser--evidence-open-sessions-items))
                  (second (arxana-browser--evidence-open-sessions-items)))
              (should (= fetch-count 1))
              (should (= (plist-get (car second) :count) 43))
              (should (equal (plist-get (car first) :latest-id) "e-cache"))
              (should (equal (plist-get (car second) :latest-id) "e-cache"))
              (should (equal (plist-get (car second) :about)
                             "Summarize the cached Sessions row.")))))
      (when (get-buffer buf)
        (kill-buffer buf)))))

(ert-deftest arxana-evidence-open-sessions-shows-placeholder-without-evidence ()
  (let ((buf (get-buffer-create "*claude-repl:test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local major-mode 'claude-repl-mode)
            (setq-local agent-chat--session-id "sid-missing")
            (setq-local agent-chat--agent-name "claude")
            (setq-local agent-chat--evidence-url
                        "http://127.0.0.1:7070/api/alpha/evidence"))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda () (list buf)))
                    ((symbol-function 'arxana-evidence--fetch-evidence)
                     (lambda (_params) nil)))
            (let ((items (arxana-browser--evidence-open-sessions-items)))
              (should (= (length items) 1))
              (let ((item (car items)))
                (should (eq (plist-get item :type) 'evidence-open-session))
                (should (equal (plist-get item :session-id) "sid-missing"))
                (should (= (plist-get item :count) 0))
                (should (equal (plist-get item :about) "No Evidence yet"))
                (should-not (plist-get item :entries))))))
      (when (get-buffer buf)
        (kill-buffer buf)))))

(ert-deftest arxana-evidence-open-sessions-error-without-evidence-endpoint ()
  (let ((buf (get-buffer-create "*claude-repl:no-evidence-endpoint*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local major-mode 'claude-repl-mode)
            (setq-local agent-chat--session-id "sid-missing")
            (setq-local agent-chat--agent-name "claude")
            (setq-local agent-chat--evidence-url nil))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda () (list buf))))
            (should-error (arxana-browser--evidence-open-sessions-items)
                          :type 'user-error)))
      (when (get-buffer buf)
        (kill-buffer buf)))))

(ert-deftest arxana-evidence-open-sessions-skips-idle-buffer-without-session-id ()
  (let ((buf (get-buffer-create "*codex-repl:no-session*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local major-mode 'codex-repl-mode)
            (setq-local codex-repl-session-id "pending"))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda () (list buf))))
            (let ((items (arxana-browser--evidence-open-sessions-items)))
              (should (= (length items) 1))
              (should (eq (plist-get (car items) :type) 'info)))))
      (when (get-buffer buf)
        (kill-buffer buf)))))

(ert-deftest arxana-evidence-open-sessions-error-running-without-session-id ()
  (let ((buf (get-buffer-create "*codex-repl:no-session-running*"))
        (proc nil))
    (unwind-protect
        (progn
          (setq proc (start-process "arxana-evidence-test-sleep" nil "sleep" "10"))
          (with-current-buffer buf
            (setq-local major-mode 'codex-repl-mode)
            (setq-local codex-repl-session-id "pending")
            (setq-local agent-chat--pending-process proc))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda () (list buf))))
            (should-error (arxana-browser--evidence-open-sessions-items)
                          :type 'user-error)))
      (when (process-live-p proc)
        (kill-process proc))
      (when (get-buffer buf)
        (kill-buffer buf)))))

(provide 'arxana-browser-evidence-test)
;;; arxana-browser-evidence-test.el ends here
