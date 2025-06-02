function main() {
  FLAGS=
  PRINT="log.print"
  while [[ -n "$1" ]]; do
    case "$1" in
      v)
        PRINT="log.vprint"
        shift
        ;;
      vv)
        PRINT="log.vprint"
        FLAGS="--trace-verbose"
        shift
        ;;
      *)
        EXPR="$@"
        break
        ;;
    esac
  done

  FULL_EXPR="let pkgs = import <nixpkgs> {}; in with pkgs.lib; with (import ~/collective/collective-public/pkgs/collective-lib {}); ${PRINT} (${EXPR})"

  CMD="nix eval --extra-experimental-features pipe-operators --impure --expr '$FULL_EXPR' --raw --show-trace $FLAGS"

  echo "Running: ${CMD}" >&2
  bash -c "$CMD"
}

NOOP=$(cat << EOF
(progn
  (defun run-nix (&optional args)
    (interactive)
    (shell-command
      (concat
        "./test.sh '"
        args
        " "
        (buffer-substring (pos-bol) (pos-eol))
        "'")))

  (defun run-nix-v ()
    (interactive)
    (run-nix "v"))

  (defun run-nix-vv ()
    (interactive)
    (run-nix "vv"))
  
  (map!
   :map global-map
   :n "SPC c n" #'run-nix
   :n "SPC c v n" #'run-nix-v
   :n "SPC c v v n" #'run-nix-vv
  )
)
EOF
)

main $@

 
