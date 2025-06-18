;;; -*- lexical-binding: t; -*-

;;; (load-file "test.el")
;;;(setq nix-variant 'tvix)
(setq nix-variant 'nix)

(defun forward-nix-block (&optional n)
  (interactive)
  (unless n (setq n 1))
  (let ((do-search (if (> n 0) 'search-forward 'search-backward))
        (target (if (> n 0) "</nix>" "<nix>")))
    (dotimes (_ (abs n)) (funcall do-search target))))

(defun strip-nix-block (block)
  (interactive)
  (replace-regexp-in-string "<nix>" ""
                            (replace-regexp-in-string "</nix>" ""
                                                      block)))

(defun current-nix-block ()
  (interactive)
  (strip-nix-block (thing-at-point 'nix-block t)))

(defun current-nix-expr ()
  (interactive)
  (replace-regexp-in-string "\n" " "
                            (replace-regexp-in-string "#.*\n" "\n"
                                                      (current-nix-block))))

(defun verbosity-string (v trace)
  (unless v (setq v 0))
  (concat
   (cond ((= v 0) "")
         ((= v 1) "v")
         ((= v 2) "vv")
         ((= v 3) "vvv")
         (t ))
   (if trace "t" "")))

(defun tvix-repl-name (v trace)
  (format "*Tvix-REPL-%s*" (verbosity-string v trace)))

(defun run-nix (&optional v trace)
  (interactive)
  (let ((expr (current-nix-expr)))
    (cond ((eq nix-variant 'nix) (shell-command (concat "./test.sh '" (verbosity-string v trace) " " expr "'")))
          ((eq nix-variant 'tvix) (tvix-repl-eval expr v trace nil nil nil))
          (t (error "Unknown nix-variant: %s" nix-variant)))))

(defun tvix-repl-buffer (v trace)
  (interactive)
  (get-buffer-create (tvix-repl-name v trace)))

(defun tvix-repl-process (v trace)
  (interactive)
  (get-buffer-process (tvix-repl-buffer v trace)))

(defun tvix-repl-wait-for-output (v trace &optional timeout)
  (unless timeout (setq timeout 10.0))
  (accept-process-output (tvix-repl-process v trace) timeout))

(defun tvix-repl-print-fn (v trace)
  (cond ((>= v 2) "x: with collective-lib; with functions; strict (log.vprint (strict x))")
        ((= v 1) "x: with collective-lib; with functions; strict (log.print (strict x))")
        (t "x: with collective-lib; with functions; strict x")))

(defun tvix-repl-run-preamble (v trace)
  "Run the Tvix REPL preamble."
  (interactive)
  (let ((trace-level (if (null v) "null" (format "%d" v)))
        (enable-partial-trace (if (and (not (null (v))) (>= v 1)) "true" "false"))
        (enable-verbose-trace (if (and (not (null (v))) (>= v 2)) "true" "false")))
    (tvix-repl-eval "\
pkgs = import <nixpkgs> {}"
                    v trace t t nil)
    (tvix-repl-eval "\
lib = pkgs.lib"
                    v trace t t nil)
    (tvix-repl-eval (format "\
__ctx = _:\
  let\
    collective-lib =\
      import ~/collective/collective-public/pkgs/collective-lib {\
        inherit pkgs lib;\
        traceLevel = %s;\
        enablePartialTrace = %s;\
        enableVerboseTrace = %s;\
      };\
    self = lib // collective-lib // {\
      __replPrint = %s;\
    };\
  in self"
                            trace-level
                            enable-partial-trace
                            enable-verbose-trace
                            (tvix-repl-print-fn v trace))
                    v trace t t nil)
    ))

(defun tvix-repl-load (v trace &optional no-init)
  (unless (comint-check-proc (tvix-repl-buffer v trace))
    (pop-to-buffer (tvix-repl-buffer v trace) nil t)
    (tvix--make-repl-in-buffer v trace (current-buffer))
    (nix-repl-mode)
    (unless no-init (tvix-repl-run-preamble v trace))))

(defun with-tvix-repl-buffer (v trace no-init body)
  (tvix-repl-load v trace no-init)
  (with-current-buffer (tvix-repl-buffer v trace) body))

(defun tvix-repl (&optional v trace no-init)
  "Load the Tvix-REPL."
  (interactive)
  (tvix-repl-load v trace no-init))

(defun tvix-format-expr (expr v trace)
  "Format the given expr for the Tvix REPL."
  (interactive)
  (format "with __ctx {}; __replPrint (%s)" expr))

(defun tvix-repl-eval (expr v trace &optional no-init no-wrap no-wait)
  "Run the given expr (or current nix block) in the Tvix REPL."
  (interactive)
  (tvix-repl-load v trace no-init)
  (with-current-buffer (tvix-repl-buffer v trace)
    (comint-kill-input)
    (insert (if no-wrap expr (tvix-format-expr expr v trace)))
    (comint-send-input)
    (unless no-wait (tvix-repl-wait-for-output v trace))
    (comint-kill-input)
    ))

(defun tvix--make-repl-in-buffer (v trace buffer)
  "Make Tvix Repl in BUFFER."
  (let
      (
       (tvix-executable "~/code/tvix/target/debug/tvix")
       (tvix-executable-args
        (append `("--extra-nix-path"
                  ,(format "nixpkgs=%s"
                           (replace-regexp-in-string "\n" ""
                                                     (shell-command-to-string "nix eval --impure --expr '<nixpkgs>'")))
                  )
                (if trace
                    (cond ((null v) '())
                          ((= v 0) '())
                          ((= v 1) '("--trace-runtime"))
                          ((= v 2) '("--trace-runtime"))
                          ((= v 3) '("--trace-runtime" "--trace-runtime-timing"))
                          (t '()))
                  '()
                  )
                (cond ((null v) '())
                      ((= v 0) '())
                      ((= v 1) '())
                      ((= v 2) '())
                      ((= v 3) '("--display-ast" "--dump-bytecode"))
                      (t '()))
                )
        )
       )
    (apply
     'make-comint-in-buffer
     (append `("Tvix-REPL" ,buffer ,tvix-executable nil) tvix-executable-args)
     )
    )
  )

(defun run-nix-0 () (interactive) (run-nix 0 nil))
(defun run-nix-v () (interactive) (run-nix 1 nil))
(defun run-nix-vv () (interactive) (run-nix 2 nil))
(defun run-nix-vvv () (interactive) (run-nix 3 nil))
(defun run-nix-vt () (interactive) (run-nix 1 t))
(defun run-nix-vvt () (interactive) (run-nix 2 t))
(defun run-nix-vvvt () (interactive) (run-nix 3 t))

(map!
 :map global-map
 :n "SPC c n" #'run-nix-0
 :n "SPC c v n" #'run-nix-v
 :n "SPC c v v n" #'run-nix-vv
 :n "SPC c v v v n" #'run-nix-vvv
 :n "SPC c v t n" #'run-nix-vt
 :n "SPC c v v t n" #'run-nix-vvt
 :n "SPC c v v v t n" #'run-nix-vvvt
 )
