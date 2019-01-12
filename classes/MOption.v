(** Matches include/Option.hpp *)

Module Option.
  Class NativeOption A Option :=
  {
    some:   A -> Option;
    none:   Option;
  }.

  Instance nativeOption {T}: NativeOption T (option T) :=
  {
    some  := Some;
    none  := None;
  }.
End Option.

Require Extraction.
Extraction Language JSON.
Separate Extraction Option.nativeOption.
