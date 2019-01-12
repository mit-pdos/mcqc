
Require MNat.
Import MNat.Nat.

Require Import Coq.Strings.Ascii.
Require Import Coq.Strings.String.

Module Show.
  Class Show A :=
    {
      show: A -> string
    }.

  (** Private functions *)
  Module _Private.
    Definition natToDigit (n: nat) : ascii :=
      match n with
      | 0 => "0"
      | 1 => "1"
      | 2 => "2"
      | 3 => "3"
      | 4 => "4"
      | 5 => "5"
      | 6 => "6"
      | 7 => "7"
      | 8 => "8"
      | _ => "9"
      end.
    Fixpoint itoaT (time n : nat) (acc : string) : string :=
      let acc' := String (natToDigit (mod n 10)) acc in
      match time with
      | 0 => acc'
      | S time' =>
        match n / 10 with
        | 0 => acc'
        | n' => itoaT time' n' acc'
        end
      end.
  End _Private.
  (** End private functions *)

  Instance showAscii : Show ascii :=
    {
      show c:= String c EmptyString;
    }.
  Instance showString : Show string :=
    {
      show := id;
    }.
  Instance showNat : Show nat :=
    {
      show n := _Private.itoaT 1000 n "";
    }.
  Local Open Scope string_scope.
  Instance showBool: Show bool :=
    {
      show b := if b then "true" else "false";
    }.
  Instance showOption {T} {showT: Show T} : Show (option T) :=
    {
      show o := match o with
                  | Some v => "Some " ++ (show v)
                  | None => "None"
                end;
    }.
  Instance showProd {A B} {showA : Show A} {showB : Show B} : Show (A * B) :=
    {
      show t := "(" ++ (show (fst t)) ++ ", " ++ (show (snd t)) ++ ")"
    }.
End Show.

Require Extraction.
Extraction Language JSON.
Separate Extraction Show.showAscii Show.showString Show.showNat Show.showBool Show.showOption Show.showProd.
