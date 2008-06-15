; I am running this program like this:
; mzscheme -r hello-world.scm

(display "bla")    ; this shows up
(newline)
(module hello-world   ; this is the name of the module
        mzscheme      ; this is the language used
   (display "Hello world!")   ; but this does not show up!
   (newline)
   )