(in-package :shuffletron)

;;; Profile support (for multiple libraries in different
;;; locations). The "default" profile stores its settings directly
;;; under ~/.shuffletron/ for backward compatibility with existing
;;; settings. Alternate profiles store them under
;;; ~/.shuffletron/profiles/<ProfileName>/.

(defvar *profile* "default")

(defun profile-path-component ()
  (if (equal *profile* "default")
      nil
      (list "profiles" *profile*)))

(defun subpath (list) (subseq list 0 (1- (length list))))

(defun prefpath (prefname &key (profile *profile*))
  (flet ((find-type (path)
           (let* ((path (if (listp path) (car (last path)) path))
                  (separator (position #\. path)))
             (if (and separator (plusp separator) (= (count #\. path) 1))
                 (list (subseq path 0 separator) (subseq path (+ separator 1)))
                 (list path)))))
    (let ((name-type (find-type prefname))
          (subpath (if (listp prefname) (subpath prefname) nil))
          (*profile* profile))
      (merge-pathnames
       (make-pathname :directory `(:relative ".shuffletron"
                                             ,@(profile-path-component)
                                             ,@(mapcar #'string subpath))
                      :name (and (car name-type) (string (car name-type)))
                      :type (second name-type))
       (user-homedir-pathname)))))

(defun pref (name &optional default)
  (handler-case (values (file (prefpath name)) t)
    (file-error (c)
      (when (probe-file (prefpath name))
        (format t "Problem reading ~A:~%~A~%" (prefpath name) c))
      (values default nil))
    (reader-error (c)
      (format t "Error parsing contents of ~A:~%~A~%" (prefpath name) c)
      (values default nil))))

(defun (setf pref) (value name)
  (ensure-directories-exist (prefpath name))
  (setf (file (prefpath name)) value))

(defun all-profiles ()
  (append
   (and (probe-file (prefpath "library-base" :profile "default")) '("default"))
   (mapcar (lambda (x) (car (last (pathname-directory x))))
           (directory
            (merge-pathnames
             (make-pathname :directory '(:relative ".shuffletron" "profiles" :wild-inferiors)
                            :name "library-base")
             (user-homedir-pathname))))))

(defun get-profile-base (profile-name)
  (let ((*profile* profile-name))
    (pref "library-base")))

