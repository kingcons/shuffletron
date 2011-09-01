(in-package :shuffletron)

(let ((path (prefpath '("cl-scrobbler" "config"))))
  (ensure-directories-exist path)
  (setf cl-scrobbler:*config-dir* (directory-namestring path)))

(defun track-timing ()
  (list
   (round (streamer-length *current-stream* *mixer*) (mixer-rate *mixer*))
   (round (streamer-position *current-stream* *mixer*) (mixer-rate *mixer*))))

(defun track-metadata ()
  (let ((id3 (song-id3 (song-of *current-stream*))))
    (list (getf id3 :title) (getf id3 :artist))))

(setf cl-scrobbler:*track-info-fn* #'track-metadata
      cl-scrobbler:*track-time-fn* #'track-timing)

(setf *seek-hook* (list (lambda ()
                          (cl-scrobbler:set-last-seek (second (track-timing)))))
      *play-song-hook* (list #'cl-scrobbler:set-last-seek)
      *next-song-hook* (list #'cl-scrobbler:maybe-queue-scrobble))

(bordeaux-threads:make-thread #'cl-scrobbler:scrobbler-init
                              :name "Scrobbler thread")
