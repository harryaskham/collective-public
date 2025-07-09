;;; -*- lexical-binding: t; -*-

;;; (load-file "nixlike.el")

(defcustom
  collective-dir
  "~/collective"
  ;;"~/collective/collective-public"
  "The absolute path of the directory of the collective or collective-public repository.")

(defcustom
  nixlike-nix-executable
  "nix"
  "The nix executable, either absolute or as discoverable in the PATH.")

(defcustom
  nixlike-tvix-executable
  "~/code/tvix/target/debug/tvix"
  "The tvix executable, either absolute or as discoverable in the PATH.")

(defcustom
  nixlike-nix-eval-strategy
  'eval
  "The Nix evaluation strategy ('eval or 'instantiate)")

(defcustom
  nixlike-nix-variant
  'nix
  "The Nix variant to use ('nix or 'tvix)")

(defcustom
  nixlike-kill-repl-before-eval
  t
  "Whether to kill any existing REPL before evaluating. Useful for rapid iterations that require reload due to REPL caching.")

(defcustom
  nixlike-mode
  'repl
  "The default mode ('repl or 'shell) to use in run-nix when none is specified.")

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

(defun verbosity-string (v trace raw)
  (cond ((eq nixlike-nix-variant 'nix) "")  ; Nix does not accept a "--raw" REPL flag; only affects print-fn
        ((eq nixlike-nix-variant 'tvix) (cond (raw "x") (trace "t") (t "n")))
        (t (error "Unknown nixlike-nix-variant: %s" nixlike-nix-variant))))

(defun shell-command-buffer ()
  (interactive)
  (get-buffer shell-command-buffer-name))

(defun nixlike-log (v trace raw expr msg &rest args)
  (apply 'message
         (format "\n\
  /--------\n\
  / nixlike\n\
  /----------\n\
  |  nix-variant: %%s\n\
  |  mode: %%s\n\
  |  verbosity: %%d\n\
  |  trace: %%s\n\
  |  raw: %%s\n\
  |  expr: %%s\n\
  |----------\n\
  | %s\n\
  \\---------" msg)
         nixlike-nix-variant
         nixlike-mode
         v
         (if trace "yes" "no")
         (if raw "yes" "no")
         expr
         args))

(defun nixlike-run-nix-shell (v trace raw expr)
  (interactive)
  (let ((command (nixlike-eval-command expr v trace raw)))
    (nixlike-log v trace raw expr
                 "Running shell command: %s" command)
    (shell-command command))
  (let ((this-buffer (current-buffer)))
    (pop-to-buffer (shell-command-buffer) nil t)
    (goto-char (point-max))
    (unless (search-backward "       error:" nil t)
      (progn
        (search-backward "__replPrint" nil t)
        (pop-to-buffer this-buffer nil t)))))

(defun nixlike-run-nix-repl (v trace raw expr)
  (interactive)
  (nixlike-repl-load v trace raw nil)
  (nixlike-repl-eval expr v trace raw nil nil nil))

(defun run-nix (&optional v trace raw)
  (interactive)
  (unless v (setq v 0))
  (let ((expr (current-nix-expr)))
    (cond ((eq nixlike-mode 'shell) (nixlike-run-nix-shell v trace raw expr))
          ((eq nixlike-mode 'repl) (nixlike-run-nix-repl v trace raw expr))
          (t (error "Unknown mode: %s" nixlike-mode)))))

(defun nixlike-repl-name (v trace raw)
  (letrec ((vstr (verbosity-string v trace raw))
           (suffix (if (string= vstr "") "" (concat "-" vstr)))
           (name (cond ((eq nixlike-nix-variant 'nix) "Nix-REPL")
                       ((eq nixlike-nix-variant 'tvix) "Tvix-REPL")
                       (t (error "Unknown nixlike-nix-variant: %s" nixlike-nix-variant)))))
    (concat name suffix)))

(defun nixlike-repl-buffer-name (v trace raw)
  (interactive)
  (format "*%s*" (nixlike-repl-name v trace raw)))

(defun nixlike-repl-buffer (v trace raw)
  (interactive)
  (get-buffer-create (nixlike-repl-buffer-name v trace raw)))

(defun nixlike-repl-process (v trace raw)
  (interactive)
  (get-buffer-process (nixlike-repl-buffer v trace raw)))

(defun nixlike-repl-wait-for-output (v trace raw &optional timeout)
  (unless timeout (setq timeout 10.0))
  (accept-process-output (nixlike-repl-process v trace raw) timeout))

(defun nixlike-repl-print-fn (v trace raw)
  (cond (raw "x: {string = lib.trace x x;}.${lib.typeOf x} or (lib.traceSeqN 10 x x)")
        ((= v 0) "x: lib.trace (log.print x) x")
        (t (format "x: lib.trace (log.vprintD %d x) x" v))))

(defun nixlike-common-args (v trace raw)
  (let ((show-trace-arg
         (cond ((and (eq nixlike-nix-variant 'nix) trace) '("--show-trace"))
               (t '())))
        (extra-nix-path-arg
         (cond ((eq nixlike-nix-variant 'tvix)
                `("--extra-nix-path"
                  ,(format
                    "nixpkgs=%s"
                    (replace-regexp-in-string
                     "\n" ""
                     (shell-command-to-string "nix eval --impure --expr '<nixpkgs>'")))))
               (t '())))
        (display-ast-arg
         (cond ((and (eq nixlike-nix-variant 'tvix) (>= v 3))
                '("--display-ast"))
               (t '())))
        (dump-bytecode-arg
         (cond ((and (eq nixlike-nix-variant 'tvix) (>= v 3))
                '("--dump-bytecode"))
               (t '())))
        (trace-runtime-arg
         (cond ((and (eq nixlike-nix-variant 'tvix) (>= v 1) trace)
                '("--trace-runtime"))
               (t '())))
        (trace-runtime-timing-arg
         (cond ((and (eq nixlike-nix-variant 'tvix) (>= v 3) trace)
                '("--trace-runtime-timing"))
               (t '()))))
    (append
     show-trace-arg
     extra-nix-path-arg
     display-ast-arg
     dump-bytecode-arg
     trace-runtime-arg
     trace-runtime-timing-arg)))

(defun nixlike-eval-command-argv (expr-raw v trace raw)
  (letrec ((expr-with-ctx (concat "with __mkCtx {}; " expr-raw))
           (expr-with-ctx-printed (nixlike-replprint-expr expr-with-ctx v trace raw))
           (expr (format
                  "'%s'"
                  (replace-regexp-in-string
                   "'" "\\'"
                   (nixlike-expr-with-preamble expr-with-ctx-printed v trace raw)))))
    (append
     (cond ((eq nixlike-nix-variant 'nix)
            (cond ((eq nixlike-nix-eval-strategy 'eval)
                   `(,(nixlike-executable) "eval" "--impure" "--expr" ,expr))
                  ((eq nixlike-nix-eval-strategy 'instantiate)
                   `(,(concat (nixlike-executable) "-instantiate")
                     ,(if raw "--strict" "")
                     "--eval"
                     "-E" ,expr))
                  (t (error "Unknown nix-eval-strategy: %s" nixlike-nix-eval-strategy))))
           ((eq nixlike-nix-variant 'tvix) (append
                                            `(,(nixlike-executable))
                                            (if raw '("--raw") '())
                                            `("-E" ,expr)))
           (t (error "Unknown nixlike-nix-variant: %s" nixlike-nix-variant)))
     (nixlike-common-args v trace raw))))

(defun nixlike-eval-command (expr v trace raw)
  (string-join (nixlike-eval-command-argv expr v trace raw) " "))

(defun nixlike-enable-partial-trace (v) (if (>= v 1) "true" "false"))

(defun nixlike-enable-verbose-trace (v) (if (>= v 2) "true" "false"))

(defun nixlike-enable-short-trace (v) (if (>= v 2) "true" "false"))

(defun nixlike-repl-run-preamble (v trace raw)
  "Run the Nixlike REPL preamble. Only needed in Tvix; Nix has 'nix repl --expr' to install this."
  (interactive)
  (let ((trace-level (format "%d" v)))
    (progn
      (nixlike-repl-eval "\
                    pkgs = import <nixpkgs> {}" v trace raw t t nil)
      (nixlike-repl-eval "\
                    lib = pkgs.lib" v trace raw t t nil)
      (nixlike-repl-reload-preamble v trace raw))))

(defun nixlike-preamble-expr (v trace raw)
  "Build the strict Nixlike preamble expression, returning the full context."
  (nixlike-expr-with-preamble "__mkCtx {}" v trace raw))

(defun nixlike-repl-reload-preamble (v trace raw)
  "Reload the REPL preamble. Will lose any cached state but pick up new changes."
  (cond ((eq nixlike-nix-variant 'nix)
         (nixlike-repl-eval
          (format ":a %s" (nixlike-preamble-expr v trace raw))
          v trace raw t t nil))
        ((eq nixlike-nix-variant 'tvix)
         (nixlike-repl-eval
          (format "__ctx = %s" (nixlike-preamble-expr v trace raw))
          v trace raw t t nil))
        (t (error "Unknown nixlike-nix-variant: %s" nixlike-nix-variant))))

(defun nixlike-mkCtx-expr (v trace raw)
  "Built the Nixlike expr for mkCtx"
  (format "\
   _: \
   let \
   pkgs = import <nixpkgs> {}; \
   lib = pkgs.lib; \
   collective-lib = import %s/pkgs/collective-lib \
   { \
   inherit pkgs lib; \
   traceOpts = \
   { \
     traceLevel = %d; \
     enablePartialTrace = %s; \
     enableVerboseTrace = %s; \
     enableShortTrace = %s; \
   }; \
   }; \
   in \
   collective-lib.typed \
   // { \
   inherit pkgs; \
   inherit lib; \
   inherit collective-lib; \
   }"
          collective-dir
          v
          (nixlike-enable-partial-trace v)
          (nixlike-enable-verbose-trace v)
          (nixlike-enable-short-trace v)))

(defun nixlike-expr-with-preamble (expr v trace raw)
  "Built the Nixlike Shell preamble for a raw expr. Allows 'with __ctx {}; <expr>'."
  (format "let __mkCtx = %s; in %s"
          (nixlike-mkCtx-expr v trace raw)
          expr))

(defun nixlike-repl-load (v trace raw no-init)
  (let ((repl-buffer (nixlike-repl-buffer v trace raw)))
    (when nixlike-kill-repl-before-eval
      (with-current-buffer repl-buffer
        (display-buffer (current-buffer))
        (when (comint-check-proc (current-buffer))
          (comint-kill-subjob)
          (sleep-for 0.1))))
    (unless (comint-check-proc repl-buffer)
      ;;(with-current-buffer (nixlike-repl-buffer v trace raw)
      ;;(pop-to-buffer (nixlike-repl-buffer v trace raw) nil t)
      ;;(nixlike--make-repl-in-buffer v trace raw (current-buffer))
      (nixlike--make-repl-in-buffer v trace raw repl-buffer)
      (unless no-init (nixlike-repl-run-preamble v trace raw)))))

(defun nixlike-repl (&optional v trace raw no-init)
  "Load the Nixlike-REPL."
  (interactive)
  (unless v (setq v 0))
  (nixlike-repl-load v trace raw no-init))

(defun nixlike-replprint-expr (expr v trace raw)
  "Format the given expr for the Nixlike REPL."
  (cond ((and (eq nixlike-mode 'repl) (eq nixlike-nix-variant 'nix))
         (format "(%s) (%s)" (nixlike-repl-print-fn v trace raw) expr))
        (t (format "with __mkCtx {}; (%s) (%s)" (nixlike-repl-print-fn v trace raw) expr))))

(defun nixlike-repl-eval (expr v trace raw &optional no-init no-wrap no-wait)
  "Run the given expr (or current nix block) in the Nixlike REPL."
  (interactive)
  (with-current-buffer (nixlike-repl-buffer v trace raw)
    (display-buffer (current-buffer))
    (comint-kill-input)
    (insert (if no-wrap expr (nixlike-replprint-expr expr v trace raw)))
    (comint-send-input)
    (unless no-wait (nixlike-repl-wait-for-output v trace raw))
    (comint-kill-input)
    (goto-char (point-max))
    ))

(defun nixlike-repl-args (v trace raw)
  (append
   (cond ((eq nixlike-nix-variant 'nix)
          `("repl"
            "--eval-cache"
            "--expr" ,(nixlike-preamble-expr v trace raw)))
         ((eq nixlike-nix-variant 'tvix) '())
         (t (error "Unknown nixlike-nix-variant: %s" nixlike-nix-variant)))
   (nixlike-common-args v trace raw)))

(defun nixlike-executable ()
  (cond ((eq nixlike-nix-variant 'nix) nixlike-nix-executable)
        ((eq nixlike-nix-variant 'tvix) nixlike-tvix-executable)
        (t (error "Unknown nixlike-nix-variant: %s" nixlike-nix-variant))))

(defun nixlike-make-comint-in-buffer-args (v trace raw buffer)
  (append
   `(
     ,(nixlike-repl-name v trace raw)
     ,buffer
     ,(nixlike-executable)
     nil)
   (nixlike-repl-args v trace raw)))

(defun nixlike--make-repl-in-buffer (v trace raw buffer)
  "Make Nixlike REPL in BUFFER (either nix-repl or tvix-repl determined by nixlike-nix-variant."
  (apply
   'make-comint-in-buffer
   (nixlike-make-comint-in-buffer-args v trace raw buffer))
  (with-current-buffer (nixlike-repl-buffer v trace raw)
    (nix-repl-mode)))

(defun run-nix-t () (interactive) (run-nix 0 t nil))
(defun run-nix-vt () (interactive) (run-nix 1 t nil))
(defun run-nix-vvt () (interactive) (run-nix 2 t nil))
(defun run-nix-vvvt () (interactive) (run-nix 3 t nil))

(defun run-nix-xt () (interactive) (run-nix 0 t t))
(defun run-nix-vxt () (interactive) (run-nix 1 t t))
(defun run-nix-vvxt () (interactive) (run-nix 2 t t))
(defun run-nix-vvvxt () (interactive) (run-nix 3 t t))

(defun run-nix-t-reload () (interactive) (nixlike-repl-reload-preamble 0 t nil))
(defun run-nix-xt-reload () (interactive) (nixlike-repl-reload-preamble 0 t t))

(map!
 :map nix-mode-map
 :leader
 :prefix ("c n" . "Collective Nix")
 :n "n" #'run-nix-t
 :n "N" #'run-nix-xt
 :n "r" #'run-nix-t-reload
 :n "R" #'run-nix-xt-reload
 (:prefix ("v" . "Verbose")
          (:prefix "1"
           :n "n" #'run-nix-vt
           :n "N" #'run-nix-vxt)
          (:prefix "2"
           :n "n" #'run-nix-vvt
           :n "N" #'run-nix-vvxt)
          (:prefix "3"
           :n "n" #'run-nix-vvvt
           :n "N" #'run-nix-vvvxt))
 )
