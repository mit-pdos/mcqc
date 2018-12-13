(** Matches include/List.hpp *)


Module Array.
  Class NativeArray A List :=
  {
    get:    nat -> List -> A;
    put:    nat -> A -> List -> List;
    empty:  nat -> A -> List;
  }.


  Parameter Array: Type -> Type.

  Instance nativeArray {T}: NativeArray T (Array T). Admitted.                                   

End Array.

Require Extraction.
Extraction Language JSON.
Separate Extraction Array.nativeArray.

