Add Rec LoadPath "../../classes" as Mcqc.
Require Import Coq.Lists.List.
From Mcqc Require Import MIO.
Import IO.
Import ListNotations.
From Mcqc Require Import MShow.
Import Show.

Require Import Coq.Strings.String.
Local Open Scope string_scope.

Fixpoint series(n: nat) :=
  match n with
    | 0 => []
    | S m => n::series m
  end.

Require Import Coq.Init.Nat.

Require Import Coq.Sorting.Mergesort.
Import NatSort.
Definition  merge l1 l2 :=
  match l1, l2 with
  | [], _ => l2
  | _, [] => l1
  | a1::l1', a2::l2' =>
      if a1 <=? a2 then a1 :: merge l1' l2 else a2 :: merge l1 l2'
  end.

Fixpoint merge_list_to_stack stack l :=
  match stack with
  | [] => [Some l]
  | None :: stack' => Some l :: stack'
  | Some l' :: stack' => None :: merge_list_to_stack stack' (merge l' l)
  end.

Fixpoint merge_stack stack :=
  match stack with
  | [] => []
  | None :: stack' => merge_stack stack'
  | Some l :: stack' => merge l (merge_stack stack')
  end.

Fixpoint iter_merge stack l :=
  match l with
  | [] => merge_stack stack
  | a::l' => iter_merge (merge_list_to_stack stack [a]) l'
  end.

Definition sort := iter_merge [].

Definition main :=
  _ <- print "Sorted";
  let test := series 10 in
  print (show (sort test)).

Require Extraction.
Extraction Language Haskell.
Extract Inductive nat => "Prelude.Int" [ "0" "Prelude.succ" ]
  "(\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))".
Extract Inductive bool => "Prelude.Bool" [ "Prelude.True" "Prelude.False" ].
Separate Extraction sort series.

