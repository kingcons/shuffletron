(in-package :shuffletron)

(defvar *playlists* nil)

(defun save-playlists ()
  (setf (pref "playlists") *playlists*)
  (values))

(defun load-playlists ()
  (loop for (name playlist) in (pref "playlists" '())
     do (set-playlist name playlist)))

(defun get-playlist (name)
  (cdr (assoc name *playlists* :test #'equal)))

(defun delete-playlist (name)
  (setf *playlists* (remove name *playlists* :key #'car :test #'equal)))

(defun list-playlists ()
  (loop for (name . songs) in *playlists* collecting name))

(defun set-playlist (name songs)
  (if (get-playlist name)
      (setf (cdr (assoc name *playlists* :test #'equal)) songs)
      (setf *playlists* (acons name songs *playlists*))))

(defun playlist-to-m3u (playlist &key (name playlist))
  "Export a playlist to m3u with the given name in ~/.shuffletron/playlists/.
If there is an existing file with the same name, it will be overwritten."
  (let ((path (prefpath (format nil "~a.m3u" name))))
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
