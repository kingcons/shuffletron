(in-package :shuffletron)

(defvar *playlists* nil)

(defun save-playlists ()
  (setf (pref "playlists.db") *playlists*)
  (values))

(defun load-playlists ()
  (loop for (name . songs) in (pref "playlists.db" '())
     do (set-playlist name songs)))

(defun playlist-path (name)
  (prefpath `("playlists" ,(format nil "~a.m3u" name))))

(defun get-playlist (name)
  (cdr (assoc name *playlists* :test #'equal)))

(defun delete-playlist (name)
  (setf *playlists* (remove name *playlists* :key #'car :test #'equal))
  (save-playlists))

(defun list-playlists ()
  (loop for (name . songs) in *playlists* collecting name))

(defun set-playlist (name songs)
  (if (get-playlist name)
      (setf (cdr (assoc name *playlists* :test #'equal)) songs)
      (setf *playlists* (acons name songs *playlists*)))
  (let ((path (playlist-path name)))
    (unless (and (probe-file path)
                 (equalp (md5:md5sum-file path)
                         (md5:md5sum-sequence
                          (format nil "~{~A~%~}"
                                  (loop for song across (get-playlist name)
                                     collecting (song-local-path song))))))
      (playlist-to-m3u name)
      (save-playlists))))

(defun playlist-to-m3u (playlist)
  "Export a playlist to m3u with the given name in ~/.shuffletron/playlists/.
If there is an existing file with the same name, it will be overwritten."
  (let ((path (playlist-path playlist)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (loop for song across (get-playlist playlist)
         do (format out "~a~%" (song-local-path song))))))

(defun playlist-from-m3u (path &key (name (pathname-name path)))
  "Given a path and a name, create a playlist with the given name from the
m3u at path. If a song cannot be found in the library, warn the user."
  (let ((file (if (position #\/ path) path (prefpath path)))
        (playlist (make-array 8 :fill-pointer 0 :adjustable t)))
    (with-open-file (in file)
      (loop for line = (read-line in nil) while line
         do (unless (string= "" line)
              (let ((result (query line)))
                (if (zerop (length result))
                    (format t "Warning - Couldn't find song: ~s~%" line)
                    (vector-push-extend (aref result 0) playlist))))))
    (set-playlist name playlist)))
