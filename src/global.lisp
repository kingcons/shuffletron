(in-package :shuffletron)

;;;; Global state, related macros, whatever.

(defparameter *shuffletron-version* "0.0.5")

(defvar *argv* nil)
(defvar *debug-mode* nil)

(defvar *next-hook* nil
  "A list of lambdas to be funcalled whenever next/skip is called.")

(defvar *seek-hook* nil
  "A list of lambdas to be funcalled whenever a seek is performed.")

(defvar *song-end-hook* nil
  "A list of lambdas to be funcalled immediately before a song finishes.")

(defvar *song-begin-hook* nil
  "A list of lambdas to be funcalled immediately after a song begins playing.")
