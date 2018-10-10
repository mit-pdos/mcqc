(** Matches include/List.hpp *)

Require Coq.Lists.List.

Module List.
  Class NativeList A List :=
  {
    cons:   A -> List -> List;
    head:   List -> option A;
    tail:   List -> List;
    app:    List -> List -> List;
    empty:  List -> bool;
    length: List -> nat;
  }.

  Instance nativeList {T}: NativeList T (list T) :=
  {
    cons  := @Coq.Lists.List.cons T;
    head  := @Coq.Lists.List.head T;
    tail  := @Coq.Lists.List.tail T;
    app   := @Coq.Lists.List.app T;
    (** Not part of Coq.Lists.List but still useful *)
    empty := (fun l => match Coq.Lists.List.head l with
                      | None => true
                      | Some _ => false
                    end);
    length := @Coq.Lists.List.length T;
  }.
End List.

Require Extraction.
Extraction Language JSON.
Separate Extraction List.nativeList.
