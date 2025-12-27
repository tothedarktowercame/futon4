;;; arxana-media-lyrics-test.el --- Tests for chorded lyric faces -*- lexical-binding: t; -*-

(require 'ert)

(load-file (expand-file-name "dev/arxana-media.el" default-directory))
(require 'arxana-media)

(defconst arxana-media-lyrics-test--sample
  (concat
   "[A\u266f] Generally alone and the stars shone brighter, except in the city\n"
   "[the] red glow takes over\n"
   "[C] there's a direct connection between every stimulus and every action\n"
   "[F\u266f] recall when machines were wonderful\n"
   "[E] especially including the knee-jerk reactions\n"
   "          [B] the childhood home\n"
   "[F\u266f] other machines strong sailing along\n"
   "          [C\u266f] The current detritus\n"
   "[D] to enable anyone who can build with blocks to say something\n"
   "[D] 'cause you hear some words that start to resonate\n"
   "[A\u266f] Here we grow extinct among the concrete barriers\n"
   "[G\u266f] Would it be reasonable to assume that you like interesting activities?\n"))

(ert-deftest arxana-media-lyrics-apply-chord-faces ()
  (with-temp-buffer
    (insert arxana-media-lyrics-test--sample)
    (arxana-media--lyrics-apply-chord-faces (point-min) (point-max))
    (goto-char (point-min))
    (let ((seen nil))
      (while (re-search-forward arxana-media--lyrics-chord-regexp nil t)
        (let* ((chord (match-string-no-properties 1))
               (face (arxana-media--lyrics-face-for-chord chord))
               (body-start (match-beginning 2))
               (body-face (and body-start (get-text-property body-start 'face))))
          (push chord seen)
          (if face
              (should (eq body-face face))
            (should (null body-face)))
          (should (null (get-text-property (match-beginning 1) 'face)))))
      (dolist (chord '("A\u266f" "C" "F\u266f" "E" "B" "C\u266f" "D" "G\u266f" "the"))
        (should (member chord seen))))))
