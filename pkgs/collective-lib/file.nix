{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;

# Utilities for building home-manager file trees from mixed sources.
#
# The primary entry point is `mkSymlinkDir`, which produces an attrset
# suitable for merging into `home.file`.  It combines named links and
# glob-style entries whose sources may live either inside or outside the
# Nix store.
#
# Because Nix evaluation is sandboxed, there is an inherent asymmetry:
#
#   * Store paths   – can be introspected at eval time (builtins.readDir),
#                     so true glob expansion works.
#   * Non-store paths – cannot be read during eval.  We can only create a
#                       single directory symlink, or the caller must list
#                       children explicitly.
#
# mkOutOfStoreSymlink (from config.lib.file) is threaded through so that
# non-store targets are wired up correctly at activation time.
#
# ─── Usage ────────────────────────────────────────────────────────────
#
#   home.file = collective-lib.file.mkSymlinkDir {
#     prefix = ".claude/agents";
#     mkOutOfStoreSymlink = config.lib.file.mkOutOfStoreSymlink;  # optional; required only for non-store paths
#
#     # Named symlinks — key becomes the filename under prefix.
#     # Store vs non-store is auto-detected.
#     links = {
#       my-store-link = "${pkgs.somepkg}/share/agent.md";
#       my-local-link = "/home/user/agents/custom.md";
#     };
#
#     # Store paths — support directory expansion and glob filtering
#     # at eval time via builtins.readDir.
#     storeLinks = [
#       # Single file: symlinked under its basename
#       "${pkgs.io-mcp}/lib/agents/io-mcp.md"
#
#       # Directory: every file in it is symlinked individually
#       "${pkgs.bar}/share/agents"
#
#       # Glob: only matching files are symlinked
#       { path = "${pkgs.foo}/share/agents"; glob = "*.md"; }
#     ];
#
#     # Out-of-store paths — resolved at activation time.
#     outOfStoreLinks = [
#       # Single file: symlinked under its basename
#       { file = "/home/user/agents/extra.md"; }
#
#       # Directory with explicit children listing
#       { path = "/home/user/collective/agents";
#         children = [ "fiction.md" "coding.md" ]; }
#
#       # Whole directory symlink (ONE symlink to the dir itself).
#       # WARNING: this conflicts with any other entry that creates
#       # files inside the same prefix — use only when this is the
#       # sole source for the prefix.
#       { dir = "/home/user/collective/agents"; }
#     ];
#   };
#
# ─── Collision behaviour ──────────────────────────────────────────────
#
# If two entries resolve to the same home.file key, the last one wins
# (right-biased attrset merge: links < storeLinks < outOfStoreLinks).
# Home-manager itself will warn if the same key is set from multiple
# module sources.

rec {
  # ── Predicates ────────────────────────────────────────────────────

  # Whether a path string points into the Nix store.
  isStorePath = path:
    hasPrefix builtins.storeDir (toString path);

  # ── Store-path helpers ────────────────────────────────────────────

  # Read directory entries, returning only regular files and symlinks.
  # `glob` may be null (all files) or a simple suffix pattern like "*.md".
  readDirFiles = path: glob:
    let
      entries = builtins.readDir path;
      matchesGlob = name:
        if glob == null then true
        else if hasPrefix "*." glob
          then hasSuffix (removePrefix "*" glob) name
          else name == glob;
    in
      filterAttrs
        (name: type: (type == "regular" || type == "symlink") && matchesGlob name)
        entries;

  # Expand a store directory into per-file home.file entries.
  storeDirToHomeFiles = prefix: dirPath: glob:
    mapAttrs'
      (name: _: nameValuePair "${prefix}/${name}" { source = "${dirPath}/${name}"; })
      (readDirFiles dirPath glob);

  # Single store file → home.file entry keyed by its basename.
  storeFileToHomeFile = prefix: filePath:
    { "${prefix}/${baseNameOf (toString filePath)}" = { source = filePath; }; };

  # Dispatch on a storeLinks entry.
  processStoreLink = prefix: entry:
    if builtins.isAttrs entry then
      storeDirToHomeFiles prefix entry.path (entry.glob or null)
    else if builtins.pathExists entry && builtins.readFileType entry == "directory" then
      storeDirToHomeFiles prefix entry null
    else
      storeFileToHomeFile prefix entry;

  # ── Out-of-store helpers ──────────────────────────────────────────

  # Dispatch on an outOfStoreLinks entry.
  #
  # Accepted forms:
  #   { dir  = "/path"; }                         → single symlink to the directory
  #   { file = "/path/foo.md"; }                  → single symlink to one file
  #   { path = "/path"; children = [ ... ]; }     → per-child symlinks
  #   "/path"                                     → legacy shorthand, same as { dir = "/path"; }
  processOutOfStoreLink = prefix: mkOOS: entry:
    if builtins.isString entry then
      # Legacy string form: whole-directory symlink
      { "${prefix}" = { source = mkOOS entry; }; }

    else if entry ? dir then
      # Whole-directory symlink
      { "${prefix}" = { source = mkOOS entry.dir; }; }

    else if entry ? file then
      # Single file
      let name = baseNameOf entry.file;
      in { "${prefix}/${name}" = { source = mkOOS entry.file; }; }

    else if entry ? path && entry ? children then
      # Explicit per-child listing
      foldl'
        (acc: child: acc // {
          "${prefix}/${child}" = { source = mkOOS "${entry.path}/${child}"; };
        })
        {}
        entry.children

    else
      throw "file.processOutOfStoreLink: unrecognised entry shape: ${builtins.toJSON entry}";

  # ── Named links ──────────────────────────────────────────────────

  # Auto-detect store vs out-of-store for a named link.
  processLink = prefix: mkOOS: name: path:
    { "${prefix}/${name}" =
        { source = if isStorePath path then path else mkOOS path; };
    };

  # ── Main entry point ─────────────────────────────────────────────

  mkSymlinkDir = {
    prefix,
    mkOutOfStoreSymlink ? null,
    links ? {},
    storeLinks ? [],
    outOfStoreLinks ? [],
  }:
    let
      mkOOS =
        if mkOutOfStoreSymlink != null then mkOutOfStoreSymlink
        else throw "file.mkSymlinkDir: mkOutOfStoreSymlink is required when using non-store paths (links with non-store targets, or outOfStoreLinks)";

      # Only use mkOOS in paths that actually need it (lazy eval means
      # the throw above only fires if a non-store path is encountered).
      linkEntries = foldl' (acc: name:
        acc // processLink prefix mkOOS name links.${name}
      ) {} (attrNames links);

      storeLinkEntries = foldl' (acc: entry:
        acc // processStoreLink prefix entry
      ) {} storeLinks;

      outOfStoreLinkEntries = foldl' (acc: entry:
        acc // processOutOfStoreLink prefix mkOOS entry
      ) {} outOfStoreLinks;
    in
      linkEntries // storeLinkEntries // outOfStoreLinkEntries;
}
