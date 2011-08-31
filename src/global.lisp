(in-package :shuffletron)

;;;; Global state, related macros, whatever.

(defparameter *shuffletron-version* "0.0.5")

(defvar *argv* nil)
(defvar *debug-mode* nil)

(defvar *seek-hook* nil
  "A list where the first element is a function or function designator and rest
is the args to that function. Apply is called on each function and its args
whenever a seek is performed.")

(defvar *play-song-hook* nil
  "A list where the first element is a function or function designator and rest
is the args to that function. Apply is called on each function and its args
whenever a song begins to play.")

(defvar *next-song-hook* nil
  "A list where the first element is a function or function designator and rest
is the args to that function. Apply is called on each function and its args
whenever a song finishes, before the next song plays.")
