# Shuffletron

Shuffletron is a Common Lisp music player with support for mp3, ogg, and flac files. It is primarily developed on Linux but should work trivially on Mac if an appropriate Lisp implementation is installed (i.e. [CCL](http://ccl.clozure.com/)) as well as packages for libao and mpg123. These packages are trivially installable through [rudix](http://rudix.org/). ogg and flac support is not yet available for mac. Scrobbling support is provided by [cl-scrobbler](http://github.com/redline6561/cl-scrobbler/).

## Install
You could git clone this library and make sure it's path on your computer is on your ```asdf:*central-registry*``` but I _strongly recommend_ that you instead use Zach Beane's positively delightful [quicklisp](http://quicklisp.org/).

* A word on dependencies
Regardless of platform, mpg123 ([rudix/mac binary](http://code.google.com/p/rudix/downloads/detail?name=mpg123-1.9.2-0.dmg&can=2&q=label%3AAudio)) should be present. On Mac, you should install libao ([rudix/mac binary](http://code.google.com/p/rudix/downloads/detail?name=libao-1.1.0-0.pkg&can=2&q=)). On Linux, you should also install libflac and libvorbisfile. Mac users may want to try the binaries for libao and libmpg123 provided by rudix. Rlwrap is recommended for command history and tab completion.

* Using quicklisp: Just start your lisp and run "(progn (ql:quickload :shuffletron) (shuffletron:run))"
* For comfort, just alias it or create a shell script that runs '$lisp --eval ...' where ... is the above line.
* rlwrap -f .shuffletron/completions is required as a prefix for command history and tab completion. You'll need to touch ~/.shuffletron/completions if it doesn't exist. (Sorry! Will fix this when I have time.)
** I do: 'rlwrap -f .shuffletron/completions sbcl --eval "(progn (ql:quickload :shuffletron) (shuffletron:run))"'.

## Getting Started
 * Install shuffletron.
 * Give it a path to your music library so your files can be indexed.
 * Type "help" and "help commands" to get more info on how to use shuffletron.
 * Are you a lisper? You can swank in with "swankme" or just use "eval _expr_".
 * Shuffletron also knows any line that begins with ( is an expression to be evaluated.
 * The original author, ahefner's, shuffletron homepage is [here](http://vintage-digital.com/hefner/software/shuffletron/).

## More?
That's pretty much it. Once it's installed just give it the path to your files, start it and use it. Let us know what features you'd like to see or hack them yourself. It's a pretty pleasant interactive environment. For an example of me fixing a bug with stream cleanup and song queues while the player ran see http://redlinernotes.com/docs/best_debugging_experience_evar.txt
