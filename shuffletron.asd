(asdf:defsystem :shuffletron
  :name "Shuffletron"
  :description "An MP3 player"
  :version "0.0.5"
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:mixalot :mixalot-mp3 :mixalot-vorbis
               :osicat :md5 :cl-scrobbler)
  :components ((:module src
                :serial t
                :components ((:file "packages")
                             (:file "util")
                             (:file "global")
                             (:file "help")
                             (:file "profiles")
                             (:file "library")
                             (:file "query")
                             (:file "tags")
                             (:file "audio")
                             (:file "ui")
                             (:file "alarms")
                             (:file "playlists")
                             (:file "scrobbler")
                             (:file "main")
                             (:file "status-bar")))))
