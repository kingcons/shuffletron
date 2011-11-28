(in-package :shuffletron)

(defvar *eval-support* 'smart
  "Whether Lisp evaluation from the Shuffletron prompt is allowed.
  May be NIL, T or 'SMART.")

(defun generate-completions ()
  ;; TODO: Also complete commands.
  (let ((results nil))
    (loop for song across *library* do
      (when (song-id3 song)
        (pushnew (getf (song-id3 song) :artist) results :test #'string=)
        (pushnew (getf (song-id3 song) :album) results :test #'string=)))
    (nconc results (list-playlists))))

(defun init ()
  (parse-command-line-args)
  (format t "~&This is Shuffletron ~A~%" *shuffletron-version*)
  (setf *random-state* (make-random-state t))
  (when (probe-file (prefpath "shuffletronrc"))
    (load (prefpath "shuffletronrc")))
  (loop do
        (init-library)
        (unless *library-base*
          (format t "~&Enter library path: ")
          (setf *library-base* (dfn (getline) "")))
        (when (not (library-scan *library-base*))
          (format t "Unable to scan \"~A\"~%" *library-base*)
          (setf *library-base* nil))
        (when (emptyp *library*)
          (format t "No playable files found in \"~A\"~%" *library-base*)
          (setf *library-base* nil))
        until *library-base*)
  (format t "~CLibrary contains ~:D files.        ~%"
          (code-char 13) (length *library*))
  (load-tags)
  (compute-filtered-library)
  (load-id3-cache)
  (load-playlists)
  (reset-query)
  (setf (pref "completions") (generate-completions))
  ;; Scan tags of new files automatically, unless there's a ton of them.
  (let ((need (songs-needing-id3-scan)))
    (cond
      ((zerop need))
      ((> need 1000)
       (format t "~:D new songs need to be scanned for ID3 tags. To do this now,
type \"scanid3\". It may take a moment.~%"
               (songs-needing-id3-scan)))
      (t (scan-id3-tags :verbose t :adjective "new ")))))

(defun spooky-init ()
  (let ((stream #+sbcl (sb-sys:make-fd-stream 1 :external-format :latin1 :output t :input nil)
                #+ccl (ccl::make-fd-stream 1 :direction :io :sharing :lock :encoding :iso-8859-1)))
    (setf *standard-output* stream)
    (setf *error-output*    stream)
    #+sbcl
    (sb-sys:enable-interrupt sb-unix:sigint
      (lambda (&rest args) (declare (ignore args)) (sb-ext:quit)))))

(defun quit ()
;;  (format t "Bye.~%")
  (finish-output)
  #+sbcl (sb-ext:quit)
  #+ccl (ccl:quit))

(defun eval* (string)
  "Read a form from STRING, evaluate it in the Shuffletron
  package and print the result."
  (print (eval (read-from-string string)))
  (terpri))

(defun show-current-query ()
  (if (emptyp *selection*)
      (format t "  Nothing matches the current query.~%")
      (show-song-matches *selection* :mode :query :highlight-queue t)))

(defun do-seek (args)
  (let* ((current *current-stream*)
         (mode-char (and args (find (elt args 0) "+-")))
         (time-arg (if mode-char
                       (string-trim " " (subseq args 1))
                       args))
         (seconds (and args (parse-timespec time-arg)))
         (samples (and seconds (* (mixer-rate *mixer*) seconds)))
         (base (if (not mode-char)
                   samples
                   (and current (streamer-position current))))
         (offset (cond ((eql mode-char #\+) samples)
                       ((eql mode-char #\-) (- samples))
                       (t 0)))
         (time (and base offset (+ base offset))))
    (cond
      ((null current) (format t "No song is playing.~%"))
      ((null time) (format t "Seek to where?~%"))
      (time
       (streamer-seek current *mixer* (max time 0))
       ;; KLUDGE
       ;; Sleep is unfortunate but seems to be needed to avoid a race.
       (sleep .1)
       (mapcar #'funcall *seek-hook*))
      (t nil))))

(defun parse-and-execute (line)
 (let* ((sepidx (position #\Space line))
        (command (subseq line 0 sepidx))
        (sepidx2 (and sepidx (position #\Space line :start (+ sepidx 1))))
        (subcommand (and sepidx (subseq line (+ sepidx 1) sepidx2)))
        (args (and sepidx (string-trim " " (subseq line sepidx)))))
   (loop for (pred . command) in shuffletron-commands:*commands*
         when (pred sepidx command sepidx2 subcommand args)
           do (apply command args))))

(defun mainloop ()
  (loop
   ;; Show the current query, if there aren't too many items:
   (when (and *selection-changed* (<= (length *selection*) *max-query-results*))
     (show-current-query))
   (setf *selection-changed* nil)
   ;;(update-status-bar)
   ;; Prompt
   (with-output ()
     (format t "~A> " (if (querying-library-p)
                          "library"
                          (format nil "~:D matches" (length *selection*))))
     (force-output))
   ;; Input
   (let ((line (getline)))
     (flet ((cmd ()
              (with-output ()
                (update-terminal-size)
                (parse-and-execute (string-trim " " line)))))
       (if *debug-mode*
           (cmd)
           (handler-case (cmd)
             (error (c)
               (with-output ()
                 (format t "~&Oops! ~A~%" c)))))))))

(defun run ()
  (spooky-init)
  (mixalot:main-thread-init)
  ;; (Don't) Clear the screen first:
  #+ONSECONDTHOUGHT (format t "~C[2J~C[1;1H" #\Esc #\Esc)
  #+SBCL (setf *argv* (rest sb-ext:*posix-argv*))
  #-SBCL (warn "*argv* not implemented for this CL implementation.")
  (init)
  (audio-init)
  (mainloop))
