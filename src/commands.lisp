(in-package :shuffletron-commands)

(defvar *commands* nil
  "A list of (predicate . package:symbol) conses denoting REPL commands.")

#|

  /[query]       Search library for [query].
  show           Print search matches, highlighting songs in queue.
  back           Undo last search.
  [songs]        Play list of songs.
  all            Play all songs in selection (equivalent to \"0-\")
  +[songs]       Append list of songs to queue.
  pre[songs]     Prepend list of songs to queue.
  random         Play a random song from the current selection.
  random QUERY   Play a random song matching QUERY
  shuffle SONGS  Play songs in random order.

  queue          Print queue contents and current song playing.
  queue names    List any existing playlists.
  queue show X   Print the numbered tracklist of X.
  queue load X   Load the playlist (X) if it exists or the absolute M3U path X.
  queue save X   Save the queue under the given name (X), overwriting if needed.
                 Also exports an M3U of the queue to ~/.shuffletron/playlists/.
  queue del X    Delete the playlist X.
  queue stash    Stash the playqueue, overwriting any former stash.
  queue unstash  Restore the playqueue from stash, losing unsaved playlists.
  shuffle        Randomize order of songs in queue.
  clear          Clear the queue (current song continues playing)
  loop           Toggle loop mode (loop through songs in queue)
  qdrop          Remove last song from queue
  qdrop RANGES   Remove songs from queue
  qtag TAGS      Apply tags to all songs in queue
  fromqueue      Transfer queue to selection
  toqueue        Replace queue with selection

  now            Print name of song currently playing.
  play           Resume playing
  stop           Stop playing (current song pushed to head of queue)
  pause          Toggle paused/unpaused.
  skip           Skip currently playing song. If looping is enabled, this
                 song won't played again.
  next           Advance to next song. If looping is enabled, the current
                 song will be enqueued.
  repeat N       Add N repetitions of currently playing song to head of queue.
  seek TIME      Seek to time (in [h:]m:ss format, or a number in seconds)
  seek +TIME     Seek forward
  seek -TIME     Seek backward
  startat TIME   Always start playback at a given time (to skip long intros)

  tag            List tags of currently playing song.
  tag TAGS       Add one or more textual tags to the current song.
  untag TAGS     Remove the given tags from the currently playing song.
  tagged TAGS    Search for files having any of specified tags.
  tags           List all tags (and # occurrences) within current query.
  killtag TAGS   Remove all occurances of the given tags
  tagall TAGS    Apply tags to all selected songs
  untagall TAGS  Remove given tags from all selected songs

  time           Print current time
  alarm          Set alarm (see \"help alarms\")

  scrobble toggle      Toggle queueing of songs for scrobbling to last.fm.
  scrobble nowplaying  Toggle updating the nowplaying status on last.fm.
  scrobble queue       Show the current songs queued for scrobbling.

  scanid3        Scan new files for ID3 tags
  prescan        Toggle file prescanning (useful if file IO is slow)
  exit           Exit the program.

  help [topic]   Help

|#

(defmacro defcommand (name (args &key docs help pred)
                      &body body)
  ;; create pred as a flet or lambda here? FROB FROB FROB
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     `(defun shuffletron-commands:blah (args)
        ,@(when docs (list docs))
        ,@body)
     (push (cons ,pred 'shuffletron-commands:blah) *commands*)
     (setf (get 'shuffletron-commands:blah :help) ,help)))

    ;; Back: restore previous selection.
    ;; A blank input line is a synonym for the "back" command.
    ((or (emptyp line) (string= line "back"))
     (cond
       (*selection-history* (setf *selection* (pop *selection-history*)))
       (t (reset-query))))

    ;; Lisp evaluation
    ((and *eval-support* (string= command "eval"))
     (eval* args))

    ((and (eq *eval-support* 'smart) (char= (aref line 0) #\())
     (eval* line))

    ;; Input starting with a forward slash refines the current query.
    ((char= (aref line 0) #\/) (refine-query (subseq line 1)))

    ;; Show all matches
    ((or (string= line "show") (string= line "ls"))
     (show-current-query))

    ;; Quit
    ((or (string= line "quit") (string= line "exit")) (quit))

    ;; Play songs now (first is played, subsequent are added to queue
    ((digit-char-p (aref line 0))
     (play-songs (selection-songs line)))

    ;; Play all songs now
    ((string= line "all")
     (play-songs *selection*))

    ;; Append songs and end of playqueue
    ((and (> (length line) 1) (char= #\+ (aref line 0)))
     (add-songs (selection-songs (subseq line 1))))

    ;; Prepend songs to playqueue
    ((and (>= (length line) 4)
          (string= "pre" (subseq line 0 3))
          (or (digit-char-p (aref line 3))
              (char= #\Space (aref line 3))))
     (with-playqueue ()
       (setf *playqueue* (concatenate 'list
                                      (selection-songs (subseq line 3))
                                      *playqueue*)))
     (unless (current-song-playing) (play-next-song)))

    ;; Skip current song. If looping, don't play this again.
    ((string= line "skip")
     (mapcar #'funcall *next-hook*)
     (skip-song)
     (show-current-song))

    ;; Advance to next song in queue. Differs from 'skip' only if
    ;; looping is enabled: whereas 'skip' drops the song from the
    ;; queue, 'next' puts it at the end of the queue.
    ((string= line "next")
     (mapcar #'funcall *next-hook*)
     (play-next-song)
     (show-current-song))

    ;; Pause playback
    ((string= line "pause")
     (toggle-pause)
     (update-status-bar))

    ;; Stop
    ((string= line "stop")
     (stop-command))

    ;; Play
    ((string= line "play")
     (play-command))

    ;; Seek
    ((string= command "seek") (do-seek args))

    ;; Start at
    ((string= command "startat")
     (let* ((time (and args (parse-timespec args)))
            (playing (current-song-playing))
            (cur-start (and playing (song-start-time playing))))
       (cond
         ((and cur-start (null time))
          (format t "Start time for the current song is ~A~%"
                  (time->string cur-start)))
         ((and playing (null time))
          (format t "No start time for this song is set.~%"))
         ((not playing) (format t "No song is playing.~%"))
         ((null time) (format t "Set start time to when?~%"))
         (t (setf (song-start-time playing) time)))))

    ;; Random
    ((string= line "random")
     (cond
       ((emptyp *filtered-library*) (format t "The library is empty.~%"))
       ((emptyp *library*) (format t "All songs in the library are ignored.~%"))
       (t (play-song (alexandria:random-elt (if (emptyp *selection*) *filtered-library* *selection*)))))
     (show-current-song))

    ;; Random from query
    ((string= command "random")
     (let ((matches (query args)))
       (cond
         ((emptyp matches)
          (format t "No songs match query.~%"))
         (t (play-song (alexandria:random-elt matches))
            (show-current-song)))))

    ;; Show playqueue
    ((string= line "queue") (show-playqueue))

    ;; Stash the Playqueue
    ((string= line "queue stash")
     (with-playqueue ()
       (set-playlist "shuffletron-stash" (coerce *playqueue* 'vector))
       (setf *playqueue* nil)))

    ;; Restore the Playqueue
    ((string= line "queue unstash")
     (with-playqueue ()
       (setf *playqueue* (coerce (get-playlist "shuffletron-stash") 'list))))

    ;; Save a Playlist (and export to M3U)
    ((and (string= command "queue")
          (equalp subcommand "save"))
     (with-playqueue ()
       (set-playlist (subseq line (+ sepidx2 1))
                     (coerce *playqueue* 'vector))))

    ;; Load a Playlist (or import from M3U)
    ((and (string= command "queue")
          (equalp subcommand "load"))
     (let ((name (subseq line (+ sepidx2 1))))
       (cond ((get-playlist name)
              (with-playqueue ()
                (setf *playqueue* (coerce (get-playlist name) 'list))))
             ((and (probe-file name) (string= "m3u" (pathname-type name)))
              (playlist-from-m3u name)
              (with-playqueue ()
                (setf *playqueue* (coerce (get-playlist (pathname-name name))
                                          'list))))
             (t
              (format t "Couldn't find playlist: ~s~%" name)))))

    ;; Delete a Playlist (and any corresponding M3U)
    ((and (string= command "queue")
          (equalp subcommand "del"))
     (let* ((name (subseq line (+ sepidx2 1)))
            (playlist (get-playlist name)))
       (if playlist
           (delete-playlist name)
           (format t "Couldn't find playlist: ~s~%" name))))

    ;; Show available playlists
    ((string= line "queue names")
     (let ((playlists (list-playlists)))
       (if playlists
           (format t "Available playlists are: ~{~s~^,~}~%" playlists)
           (format t "No playlists found. Add some!~%"))))

    ;; Show the tracks on a given playlist
    ;; BUG: "queue show" by itself whines about name being NIL.
    ((and (string= command "queue")
          (equalp subcommand "show"))
     (let* ((name (subseq line (+ sepidx2 1)))
            (playlist (coerce (get-playlist name) 'list)))
       (if playlist
           (let ((temp *playqueue*))
             (with-playqueue ()
               (setf *playqueue* playlist))
             (show-playqueue)
             (with-playqueue ()
               (setf *playqueue* temp)))
           (format t "Couldn't find playlist: ~s~%" name))))

    ;; Last.fm/Scrobbling plugin toggle
    ((string= command "scrobble")
     (cond ((equalp subcommand "toggle")
            (format t "Scrobbling is ~:[disabled~;enabled~].~%"
                    (cl-scrobbler:toggle-scrobbling)))
           ((equalp subcommand "nowplaying")
            (format t "Now Playing status updates are ~:[disabled~;enabled~].~%"
                    (cl-scrobbler:toggle-now-playing)))
           ((equalp subcommand "queue")
            (format t "Queued scrobbles are:~%")
            (loop for (song x artist) in (cl-scrobbler:cache-contents)
               do (format t "~A -- ~A~%" artist song)))))

    ;; Show current song
    ((string= line "now") (show-current-song))

    ;; Add tags to current file
    ((and (string= command "tag") args)
     (tag-current-song args))

    ;; Show tags
    ((and (string= command "tag") (null args))
     (show-current-song-tags))

    ;; Add tag to all songs in selection
    ((string= command "tagall")
     (let ((tags (parse-tag-list args)))
       (cond
         ((null tags) (format t "~&Apply which tags to selected songs?~%"))
         (t (dolist (tag tags) (tag-songs *selection* tag))))))

    ;; Remove tag from all songs in selection
    ((string= command "untagall")
     (let ((tags (parse-tag-list args)))
       (cond
         ((null tags) (format t "~&Remove which tags from selected songs?~%"))
         (t (dolist (tag tags) (untag-songs *selection* tag))))))

    ;; Remove tags from current file
    ((string= command "untag")
     (untag-current-song args))

    ;; Show songs matching tag(s)
    ((string= command "tagged")
     (if args
         (set-selection (songs-matching-tags (parse-tag-list args)))
         (format t "Search for which tags?~%")))

    ;; List all tags. Also check for dirty tag files, in case another
    ;; process modified them.
    ((string= line "tags")
     (load-tags)
     (show-all-tags))

    ;; Remove all occurances of tag(s)
    ((string= command "killtag")
     (load-tags)
     (map nil #'kill-tag (parse-tag-list args)))

    ;; Clear the queue
    ((string= line "clear")
     (with-playqueue ()
       (setf *playqueue* nil)))

    ;; Remove songs from the queue
    ((string= line "qdrop")
     (queue-remove-indices (list (1- (length *playqueue*)))))

    ((string= command "qdrop")
     (queue-remove-indices
      (expand-ranges (parse-ranges args 0 (1- (length *playqueue*))))))

    ;; Tag all songs in queue
    ((string= command "qtag")
     (with-playqueue ()
       (dolist (tag (parse-tag-list args))
         (dolist (song *playqueue*)
           (tag-song song tag)))
       (format t "Tagged ~:D songs in queue: ~{~A~^, ~}~%"
               (length *playqueue*) (mapcar #'decode-as-filename (parse-tag-list args)))))

    ;; Queue to selection
    ((string= line "fromqueue")
     (set-selection (coerce *playqueue* 'vector)))

    ;; Selection to queue
    ((string= line "toqueue")
     (with-playqueue ()
       (setf *playqueue* (coerce *selection* 'list))))


    ;; Randomize queue
    ((string= line "shuffle")
     (with-playqueue ()
       (setf *playqueue* (alexandria:shuffle *playqueue*))))

    ;; Add/play in random order
    ((and (string= command "shuffle") args)
     (add-songs (alexandria:shuffle (copy-seq (selection-songs args)))))

    ;; Repeat the current song
    ((= 6 (or (mismatch "repeat" line) 6))
     (let ((song (current-song-playing))
           (num (or (parse-integer (subseq line 6) :junk-allowed t) 1)))
       (cond
         ((< num 0) (format t "  A negative repeat count doesn't make sense.~%"))
         ((not song) (format t "  No song is playing!~%"))
         (song
          (format t "  Repeating ~D time~:P: ~A~%" num (song-local-path song))
          (with-playqueue ()
            (setf *playqueue* (nconc (make-list num :initial-element song)
                                     *playqueue*)))))))
    ;; Toggle loop mode
    ((string= line "loop")
     (setf *loop-mode* (not *loop-mode*))
     (format t "Loop mode ~A~%" (if *loop-mode* "enabled" "disabled")))

    ;; Print current time
    ((string= line "time") (print-time))

    ;; Set alarm clock
    ((string= command "alarm")
     (do-set-alarm args))

    ;; Help
    ((string= line "help") (print-help))

    ;; Help: Commands
    ((and (string= command "help")
          (equalp args "commands")) (print-commands))

    ;; Help: Examples
    ((and (string= command "help")
          (equalp args "examples")) (print-examples))

    ;; Help: Alarms
    ((and (string= command "help")
          (equalp args "alarms")) (print-alarm-help))

    ((string= command "help")
     (format t "Unknown help topic ~W~%" args))

    ;; Scan new ID3 tags
    ((string= line "scanid3")
     (scan-id3-tags :verbose t))

    ;; Clear and rescan ID3 tags
    ((string= line "rescanid3")
     (loop for song across *library* do (setf (song-id3-p song) nil))
     (scan-id3-tags :verbose t))

    ;; Attempt to start swank server, for development.
    ((string= line "swankme")
     ;; Work around an SBCL feature(?) in embedded cores:
     #+SBCL (cffi:foreign-funcall "setenv" :string "SBCL_HOME" :string "/usr/local/lib/sbcl/" :int 0 :int)
     (asdf:oos 'asdf:load-op :swank)
     (eval (read-from-string "(swank:create-server :port 0 :coding-system \"utf-8-unix\")")))

    ;; Toggle file prescanning
    ((string= line "prescan")
     (setf (pref "prescan") (not (pref "prescan" t)))
     (if (pref "prescan")
         (format t "~&Prescanning enabled. This ensures track lengths and seeks are accurate.~%")
         (format t "~&Prescanning disabled. This eliminates the delay when initially starting
playback, and is useful for slow disks or network file systems.~%")))

    ;; ???
    (t (format t "Unknown command ~W. Try 'help'.~%" line))
