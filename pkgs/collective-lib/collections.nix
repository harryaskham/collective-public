{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

# TODO: Move other polymorphic collection functions here e.g. size.

with lib;
with cutils.attrs;
with cutils.dispatch;
with cutils.errors;
with cutils.functions;
with cutils.lists;
with cutils.strings;
with cutils.tests;

let log = cutils.log;
in rec {

  # Prepend a collection to another collection.
  # The type of the second argument dictates the type of the result.
  # For 'prepend list list':
  #   the first list is prepended to the front of the second.
  #   does not check for solos, which are handled correctly by regular prepend.
  # For 'prepend set set':
  #   the sets are merged, preferring items in the second collection.
  # For 'prepend list set':
  #   list is treated as solos, merged with set, to return a set.
  #   fails if list is not a list of solos.
  # For 'prepend set list':
  #   set is converted to solos, prepended to list, to return a list of solos.
  #   fails if list is not a list of solos.
  prepend = dispatch {
    list =
      xs: dispatch {
        list = ys: xs ++ ys;
        set = ys: (mergeSolos xs) // ys;
      };
    set = xs: dispatch {
      list = ys: concatSolos (solos xs) (solos ys);
      set = ys: xs // ys;
    };
  };

  _tests = with cutils.tests; suite {
    dispatch = {

      prepend = {
        listToList = {
          simple = expect.eq (prepend [1 2] [3 4]) [1 2 3 4];
          emptyToList = expect.eq (prepend [] [3 4]) [3 4];
          listToEmpty = expect.eq (prepend [1 2] []) [1 2];
          emptyToEmpty = expect.eq (prepend [] []) [];
        };

        setToSet = {
          simple = expect.eq (prepend {a = 1; b = 2;} {c = 3; d = 4;}) {a = 1; b = 2; c = 3; d = 4;};
          overlapping = expect.eq (prepend {a = 1; b = 2;} {b = 3; c = 4;}) {a = 1; b = 3; c = 4;};
          emptyToSet = expect.eq (prepend {} {c = 3; d = 4;}) {c = 3; d = 4;};
          setToEmpty = expect.eq (prepend {a = 1; b = 2;} {}) {a = 1; b = 2;};
          emptyToEmpty = expect.eq (prepend {} {}) {};
        };

        listToSet = {
          nonSolos = expect.error (prepend [1 2] {c = 3; d = 4;});
          simple = expect.eq (prepend [ {a = 1;} {b = 2;} ] {b = 3; c = 4;}) {a = 1; b = 3; c = 4;};
          overlapping = expect.eq (prepend [ {a = 1;} {b = 2;} ] {b = 3; c = 4;}) {a = 1; b = 3; c = 4;};
          emptyToSet = expect.eq (prepend [] {c = 3; d = 4;}) {c = 3; d = 4;};
          listToEmpty = expect.eq (prepend [ {a = 1;} {b = 2;} ] {}) {a = 1; b = 2;};
          emptyToEmpty = expect.eq (prepend [] {}) {};
        };

        setToList = {
          nonSolos = expect.error (prepend {a = 1; b = 2;} [3 4]);
          simple = expect.eq (prepend {a = 1; b = 2;} [ {c = 3;} {d = 4;} ]) [ {a = 1;} {b = 2;} {c = 3;} {d = 4;} ];
          overlapping = expect.eq (prepend {a = 1; b = 2;} [ {b = 3;} {c = 4;} ]) [ {a = 1;} {b = 3;} {c = 4;} ];
          emptyToList = expect.eq (prepend {} [ {c = 3;} {d = 4;} ]) [ {c = 3;} {d = 4;} ];
          setToEmpty = expect.eq (prepend {a = 1; b = 2;} []) [ {a = 1;} {b = 2;} ];
          emptyToEmpty = expect.eq (prepend {} []) [];
        };
      };

    };
  };

}
