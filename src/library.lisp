(in-package :shuffletron)

(defvar *library* nil)
(defvar *filtered-library* nil "The library, excluding songs tagged 'ignore'")
(defvar *local-path->song* (make-hash-table :test 'equal))
(define-symbol-macro *library-base* (pref "library-base"))

(defstruct song full-path local-path tags smashed properties matchprops id3 id3-p)

(defun init-library ()
  (setf *library* (make-array 0 :fill-pointer 0 :adjustable t)))

(defun ext-p (filename extension)
  "Test a filename to see if it matches the given extension."
  ;; KLUDGE: sadly, pathname-type barfs on various files. hack!
  (let ((ext (and (position #\. filename :from-end t)
                  (subseq filename (1+ (position #\. filename :from-end t))))))
    (string-equal ext extension)))

(defvar *library-progress* 0)

(defun smash-string (string)
  (substitute #\Space #\_ (string-downcase string)))

(defun carriage-return () (format t "~C" (code-char 13)))

(defun add-to-library (full-filename relative-filename)
  (let ((song (make-song :full-path full-filename
                         :local-path relative-filename
                         :smashed (smash-string relative-filename)
                         :tags nil)))
    (vector-push-extend song *library*)
    (setf (gethash (song-local-path song) *local-path->song*) song)))

(defun library-scan (path)
  (let ((*library-progress* 0))
    (clrhash *local-path->song*)
    (when (probe-file path)
      (walk path
            (lambda (filename)
              (when (or (ext-p filename "mp3")
                        #+linux (ext-p filename "ogg")
                        #+linux (ext-p filename "flac"))
                (incf *library-progress*)
                (when (zerop (mod *library-progress* 10))
                  (carriage-return)
                  (format t "Scanning. ~:D files.." *library-progress*)
                  (force-output))
                (add-to-library filename (rel path filename)))))
      t)))

(defun songs-needing-id3-scan () (count-if-not #'song-id3-p *library*))

(defun save-id3-cache ()
  (setf (pref "id3-cache")
        (map 'vector (lambda (song) (list (song-local-path song)
                                          (song-id3-p song)
                                          (song-id3 song)))
             *library*))
  (values))

(defun load-id3-cache ()
  (loop for (name id3-p id3) across (pref "id3-cache" #())
        as song = (gethash name *local-path->song*)
        when (and song id3-p)
        do (setf (song-id3-p song) t
                 (song-id3 song) id3)))

(defun get-tags-for-song (absolute-path)
  (cond ((ext-p absolute-path "mp3")
         ; FIXME: Investigate :no-utf8.
         (mpg123:get-tags-from-file absolute-path :no-utf8 t))
        #+linux
        ((ext-p absolute-path "ogg")
         (vorbisfile:get-vorbis-tags-from-file absolute-path))
        #+linux
        ((ext-p absolute-path "flac")
         (flac:get-flac-tags-from-file absolute-path))))

(defun scan-id3-tags (&key verbose adjective)
  (format t "~&Scanning ID3 tags (~D).~%" (songs-needing-id3-scan))
  (when verbose (fresh-line))
  (loop with pending = (and verbose (songs-needing-id3-scan))
        with n = 1
        for song across *library*
        unless (song-id3-p song) do
        (when verbose
          (carriage-return)
          (format t "Reading ~Atags: ~:D of ~:D" (or adjective "") n pending)
          (force-output))
        (setf (song-id3 song) (get-tags-for-song (song-full-path song))
              (song-matchprops song) nil
              (song-id3-p song) t)
        (incf n)
        finally
        (when (and pending (not (zerop pending))) (terpri)))
  (save-id3-cache))

(defun build-sequence-table (seq &optional (key #'identity) (test #'equal))
  (let ((table (make-hash-table :test test)))
    (map nil (lambda (elt) (setf (gethash (funcall key elt) table) elt)) seq)
    table))

(defun compute-filtered-library ()
  (setf *filtered-library* (remove-if (lambda (song) (find "ignore" (song-tags song) :test #'string=)) *library*)))
