;;; -*- lexical-binding: t; -*-

;;; (load-file "nixlike.el")

(defcustom
  collective-public-dir
  "~/collective/collective-public"
  "The absolute path of the directory of the collective-public repository.")

(defcustom
  nixlike-nix-executable
  "nix"
  "The nix executable, either absolute or as discoverable in the PATH.")

(defcustom
  nixlike-tvix-executable
  "~/code/tvix/target/debug/tvix"
  "The tvix executable, either absolute or as discoverable in the PATH.")

(defcustom
  nixlike-default-nix-variant
  'nix
  "The default Nix variant to use in run-nix when none is specified.")

(defcustom
  nixlike-default-mode
  'shell
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

(defun verbosity-string (nix-variant v trace raw)
  (cond ((eq nix-variant 'nix) "")  ; Nix does not accept a "--raw" REPL flag; only affects print-fn
        ((eq nix-variant 'tvix) (cond (raw "x") (trace "t") (t "n")))
        (t (error "Unknown nix-variant: %s" nix-variant))))

(defun shell-command-buffer ()
  (interactive)
  (get-buffer shell-command-buffer-name))

(defun nixlike-run-nix-shell (nix-variant v trace raw expr)
  (interactive)
  (shell-command (nixlike-eval-command nix-variant expr v trace raw))
  (let ((this-buffer (current-buffer)))
    (pop-to-buffer (shell-command-buffer) nil t)
    (goto-char (point-max))
    (unless (search-backward "       error:" nil t)
      (progn
        (search-backward "__replPrint" nil t)
        (pop-to-buffer this-buffer nil t)))))

(defun nixlike-run-nix-repl (nix-variant v trace raw expr)
  (interactive)
  (nixlike-repl-eval nix-variant expr v trace raw nil nil nil))

(defun run-nix (&optional nix-variant mode v trace raw)
  (interactive)
  (unless nix-variant (setq nix-variant nixlike-default-nix-variant))
  (unless mode (setq mode nixlike-default-mode))
  (unless v (setq v 0))
  (let ((expr (current-nix-expr)))
    (cond ((eq mode 'shell) (nixlike-run-nix-shell nix-variant v trace raw expr))
          ((eq mode 'repl) (nixlike-run-nix-repl nix-variant v trace raw expr))
          (t (error "Unknown mode: %s" mode)))))

(defun nixlike-repl-name (nix-variant v trace raw)
  (letrec ((vstr (verbosity-string nix-variant v trace raw))
           (suffix (if (string= vstr "") "" (concat "-" vstr)))
           (name (cond ((eq nix-variant 'nix) "Nix-REPL")
                       ((eq nix-variant 'tvix) "Tvix-REPL")
                       (t (error "Unknown nix-variant: %s" nix-variant)))))
    (concat name suffix)))

(defun nixlike-repl-buffer-name (nix-variant v trace raw)
  (interactive)
  (format "*%s*" (nixlike-repl-name nix-variant v trace raw)))

(defun nixlike-repl-buffer (nix-variant v trace raw)
  (interactive)
  (get-buffer-create (nixlike-repl-buffer-name nix-variant v trace raw)))

(defun nixlike-repl-process (nix-variant v trace raw)
  (interactive)
  (get-buffer-process (nixlike-repl-buffer nix-variant v trace raw)))

(defun nixlike-repl-wait-for-output (nix-variant v trace raw &optional timeout)
  (unless timeout (setq timeout 10.0))
  (accept-process-output (nixlike-repl-process nix-variant v trace raw) timeout))

(defun nixlike-repl-print-fn (v trace raw)
  (cond (raw "x: {string = lib.trace x x;}.${typeOf x} or (lib.traceSeqN 10 x x)")
        ((= v 0) "x: lib.trace (log.print x) x")
        (t (format "x: lib.trace (log.vprintD %d x) x" v))))

(defun nixlike-common-args (nix-variant v trace raw)
  (let ((show-trace-arg
         (cond ((and (eq nix-variant 'nix) trace) '("--show-trace"))
               (t '())))
        (extra-nix-path-arg
         (cond ((eq nix-variant 'tvix)
                `("--extra-nix-path"
                  ,(format
                    "nixpkgs=%s"
                    (replace-regexp-in-string
                     "\n" ""
                     (shell-command-to-string "nix eval --impure --expr '<nixpkgs>'")))))
               (t '())))
        (display-ast-arg
         (cond ((and (eq nix-variant 'tvix) (>= v 3))
                '("--display-ast"))
               (t '())))
        (dump-bytecode-arg
         (cond ((and (eq nix-variant 'tvix) (>= v 3))
                '("--dump-bytecode"))
               (t '())))
        (trace-runtime-arg
         (cond ((and (eq nix-variant 'tvix) (>= v 1) trace)
                '("--trace-runtime"))
               (t '())))
        (trace-runtime-timing-arg
         (cond ((and (eq nix-variant 'tvix) (>= v 3) trace)
                '("--trace-runtime-timing"))
               (t '()))))
    (append
     show-trace-arg
     extra-nix-path-arg
     display-ast-arg
     dump-bytecode-arg
     trace-runtime-arg
     trace-runtime-timing-arg)))

(defun nixlike-eval-command-args (nix-variant expr v trace raw)
  (let ((expr-with-ctx (concat "with __mkCtx {}; " expr))
        (expr-str (nixlike-expr-with-preamble nix-variant expr-with-ctx v trace raw)))
    (append
     (if raw '("--raw") '())
     (cond ((eq nix-variant 'nix) `("eval" "--impure" "--expr" ,expr-str))
           ((eq nix-variant 'tvix) `("-E" ,expr-str))
           (t (error "Unknown nix-variant: %s" nix-variant)))
     (nixlike-common-args nix-variant v trace raw))))

(defun nixlike-eval-command-argv (nix-variant expr v trace raw)
  (letrec ((args (nixlike-eval-command-args nix-variant expr v trace raw))
           (exe (nixlike-executable nix-variant))
           (argv (push exe args)))
    argv))

(defun nixlike-eval-command (nix-variant expr v trace raw)
  (string-join (nixlike-eval-command-argv nix-variant expr v trace raw) " "))

(defun nixlike-enable-partial-trace (v) (if (>= v 1) "true" "false"))

(defun nixlike-enable-verbose-trace (v) (if (>= v 2) "true" "false"))

(defun nixlike-repl-run-preamble (nix-variant v trace raw)
  "Run the Nixlike REPL preamble. Only needed in Tvix; Nix has 'nix repl --expr' to install this."
  (interactive)
  (let ((trace-level (format "%d" v)))
    (progn
      (nixlike-repl-eval nix-variant "\
                    pkgs = import <nixpkgs> {}" v trace raw t t nil)
      (nixlike-repl-eval nix-variant "\
                    lib = pkgs.lib" v trace raw t t nil)
      (nixlike-repl-reload-preamble nix-variant v trace raw))))

(defun nixlike-preamble-expr (nix-variant v trace raw)
  "Build the strict Nixlike preamble expression, returning the full context."
  (nixlike-expr-with-preamble nix-variant "__mkCtx {}" v trace raw))

(defun nixlike-repl-reload-preamble (nix-variant v trace raw)
  "Reload the REPL preamble. Will lose any cached state but pick up new changes."
  (unless nix-variant (setq nix-variant nixlike-default-nix-variant))
  (cond ((eq nix-variant 'nix)
         (nixlike-repl-eval
          nix-variant
          (format ":a %s" (nixlike-preamble-expr nix-variant v trace raw))
          v trace raw t t nil))
        ((eq nix-variant 'tvix)
         (nixlike-repl-eval
          nix-variant
          (format "__ctx = %s" (nixlike-preamble-expr nix-variant v trace raw))
          v trace raw t t nil))
        (t (error "Unknown nix-variant: %s" nix-variant))))

(defun nixlike-mkCtx-expr (nix-variant v trace raw)
  "Built the Nixlike expr for mkCtx"
  (format "\
   _: \
   let \
   pkgs = import <nixpkgs> {}; \
   lib = pkgs.lib; \
   collective-lib = import %s/pkgs/collective-lib { \
   inherit pkgs lib; \
   traceLevel = %d; \
   enablePartialTrace = %s; \
   enableVerboseTrace = %s; \
   }; \
   in \
   collective-lib.typed \
   // { \
   inherit pkgs; \
   inherit lib; \
   inherit collective-lib; \
   }"
          collective-public-dir
          v
          (nixlike-enable-partial-trace v)
          (nixlike-enable-verbose-trace v)))

(defun nixlike-expr-with-preamble (nix-variant expr v trace raw)
  "Built the Nixlike Shell preamble for a raw expr. Allows 'with __ctx {}; <expr>'."
  (format "let __mkCtx = %s; in %s"
          (nixlike-mkCtx-expr nix-variant v trace raw)
          expr))

(defun nixlike-repl-load (nix-variant v trace raw no-init)
  (unless (comint-check-proc (nixlike-repl-buffer nix-variant v trace raw))
    (pop-to-buffer (nixlike-repl-buffer nix-variant v trace raw) nil t)
    (nixlike--make-repl-in-buffer nix-variant v trace raw (current-buffer))
    (nix-repl-mode)
    (unless no-init (nixlike-repl-run-preamble nix-variant v trace raw))))

(defun with-nixlike-repl-buffer (nix-variant v trace raw no-init body)
  (nixlike-repl-load nix-variant v trace raw no-init)
  (with-current-buffer (nixlike-repl-buffer nix-variant v trace raw) body))

(defun nixlike-repl (nix-variant &optional v trace raw no-init)
  "Load the Nixlike-REPL."
  (interactive)
  (unless v (setq v 0))
  (nixlike-repl-load nix-variant v trace raw no-init))

(defun nixlike-replprint-expr (nix-variant mode expr v trace raw)
  "Format the given expr for the Nixlike REPL."
  (cond ((and (eq mode 'repl) (eq nix-variant 'nix))
         (format "(%s) (%s)" (nixlike-repl-print-fn v trace raw) expr))
        (t (format "with __ctx; (%s) (%s)" (nixlike-repl-print-fn v trace raw) expr))))

(defun nixlike-repl-eval (nix-variant expr v trace raw &optional no-init no-wrap no-wait)
  "Run the given expr (or current nix block) in the Nixlike REPL."
  (interactive)
  (nixlike-repl-load nix-variant v trace raw no-init)
  (with-current-buffer (nixlike-repl-buffer nix-variant v trace raw)
    (comint-kill-input)
    (insert (if no-wrap expr (nixlike-replprint-expr nix-variant 'repl expr v trace raw)))
    (comint-send-input)
    (unless no-wait (nixlike-repl-wait-for-output nix-variant v trace raw))
    (comint-kill-input)
    ))

(defun nixlike-repl-args (nix-variant v trace raw)
  (append
   (cond ((eq nix-variant 'nix)
          `("repl"
            "--eval-cache"
            "--expr" ,(nixlike-preamble-expr nix-variant v trace raw)))
         ((eq nix-variant 'tvix) '())
         (t (error "Unknown nix-variant: %s" nix-variant)))
   (nixlike-common-args nix-variant v trace raw)))

(defun nixlike-executable (nix-variant)
  (cond ((eq nix-variant 'nix) nixlike-nix-executable)
        ((eq nix-variant 'tvix) nixlike-tvix-executable)
        (t (error "Unknown nix-variant: %s" nix-variant))))

(defun nixlike-make-comint-in-buffer-args (nix-variant v trace raw buffer)
  (append
   `(
     ,(nixlike-repl-name nix-variant v trace raw)
     ,buffer
     ,(nixlike-executable nix-variant)
     nil)
   (nixlike-repl-args nix-variant v trace raw)))

(defun nixlike--make-repl-in-buffer (nix-variant v trace raw buffer)
  "Make Nixlike REPL in BUFFER (either nix-repl or tvix-repl determined by nix-variant."
  (apply
   'make-comint-in-buffer
   (nixlike-make-comint-in-buffer-args nix-variant v trace raw buffer)))

(defun run-nix-t () (interactive) (run-nix nil nil 0 t nil))
(defun run-nix-vt () (interactive) (run-nix nil nil 1 t nil))
(defun run-nix-vvt () (interactive) (run-nix nil nil 2 t nil))
(defun run-nix-vvvt () (interactive) (run-nix nil nil 3 t nil))

(defun run-nix-xt () (interactive) (run-nix nil nil 0 t t))
(defun run-nix-vxt () (interactive) (run-nix nil nil 1 t t))
(defun run-nix-vvxt () (interactive) (run-nix nil nil 2 t t))
(defun run-nix-vvvxt () (interactive) (run-nix nil nil 3 t t))

(defun run-nix-t-reload () (interactive) (nixlike-repl-reload-preamble nil 0 t nil))
(defun run-nix-xt-reload () (interactive) (nixlike-repl-reload-preamble nil 0 t t))

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
