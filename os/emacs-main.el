; Copyright (c) 2012 Doug Rogers under the terms of the MIT License.
; See http://www.opensource.org/licenses/mit-license.html..
; $Id$

; For Posix, copy this file to ~/.emacs.
; For Windows copy it to ~/AppData/Roaming/.emacs.

(defun dr-home ()
  "Returns the home directory based on system."
  "Unfortunately, Windows emacs uses $HOME/AppData/Roaming, not $HOME."
  (if (eq 'windows-nt system-type)
      (concat "c:/Users/" (getenv "USERNAME"))   ; Works on Win7 and later.
    (getenv "HOME")))

(defun dr-load (dr-os-filename)
  "Loads a file from the ~/dr/os directory."
  (load (concat (dr-home) "/dr/os/" dr-os-filename)))

(dr-load "emacs.el")
