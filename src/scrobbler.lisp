(in-package :shuffletron)

(let ((path (prefpath '("cl-scrobbler" "config"))))
  (ensure-directories-exist path)
  (setf cl-scrobbler:*config-dir* (directory-namestring path)))

(defun song-position ()
  (round (or (streamer-position *current-stream*) 0) (mixer-rate *mixer*)))

(defun song-metadata ()
  (when (current-song-playing)
    (let ((id3 (song-id3 (current-song-playing))))
      ;; Use (pathname-name (song-local-path (current-song-playing))) instead?
      (list (or (getf id3 :title) "Unknown Song")
            (or (getf id3 :artist) "Unknown Artist")
            (round (streamer-length *current-stream*) (mixer-rate *mixer*))))))

;;; Raw, unrepentant glue.
(setf cl-scrobbler:*song-info-fn* #'song-metadata
      cl-scrobbler:*song-time-fn* #'song-position
      *seek-hook* (list #'cl-scrobbler:update-last-seek)
      *next-hook* (list #'cl-scrobbler:update-skipped)
      *song-end-hook* (list #'cl-scrobbler:maybe-queue-scrobble)
      *song-begin-hook* (list #'cl-scrobbler:update-song-info
                              #'cl-scrobbler:set-now-playing))

(bordeaux-threads:make-thread (lambda ()
                                (cl-scrobbler:scrobbler-init)
                                (cl-scrobbler:scrobbler-loop))
                              :name "Scrobbler thread")
