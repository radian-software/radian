;; -*- lexical-binding: t -*-

(require 'radian-bind-key)
(require 'radian-os)
(require 'radian-windowed)

;; When you run Emacs in windowed mode, the mouse works fine, but when
;; you run it in the clipboard some extra operating-system-specific
;; contortions are required.

(radian-with-terminal-emacs
  ;; This code is based on [1].
  ;;
  ;; [1]: Based on http://stackoverflow.com/a/8859057/3538165
  (radian-with-operating-system macOS
    ;; This enables basic mouse support.
    (xterm-mouse-mode t)

    ;; Next we define some functions that will be used for scrolling.

    (defun radian-scroll-down ()
      "Scroll down one line."
      (interactive)
      (scroll-down 1))

    (defun radian-scroll-up ()
      "Scroll up one line."
      (interactive)
      (scroll-up 1))

    ;; This binds the scrolling to the mouse wheel.
    (bind-keys ("<mouse-4>" . radian-scroll-down)
               ("<mouse-5>" . radian-scroll-up))))

(provide 'radian-mouse)
