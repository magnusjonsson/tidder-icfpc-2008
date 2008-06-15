; Run this program like this:
; mzscheme -r hello-world.scm

;; declare a module
(module hello-world   ; this is the name of the module
        mzscheme      ; this is the language used

   ;; the module body begins here.

   ;; definitions and actions to perform on loading
   ;; go into the module body.

   (display "Hello world!")
   (newline)

   )


;; require the module so that the module body
;; gets executed.
(require hello-world) 