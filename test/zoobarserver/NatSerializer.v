Require Import Ascii.
Require Import String.
Require Import Nat.

Local Open Scope string_scope.
Local Open Scope char_scope.
Definition digitToNat (c : ascii) : option nat :=
  match c with
    | "0" => Some 0
    | "1" => Some 1
    | "2" => Some 2
    | "3" => Some 3
    | "4" => Some 4
    | "5" => Some 5
    | "6" => Some 6
    | "7" => Some 7
    | "8" => Some 8
    | "9" => Some 9
    | _ => None
  end.

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
  let acc' := String (natToDigit (n mod 10)) acc in
  match time with
    | 0 => acc'
    | S time' =>
      match n / 10 with
        | 0 => acc'
        | n' => itoaT time' n' acc'
      end
  end.

Module NatSerializer.
  Fixpoint atoi (s : string) : option nat :=
    let len := length s -1 in
    match s with
    | EmptyString => None
    | String h "" => digitToNat h 
    | String h ts => 
      match digitToNat h with
      | None => None
      | Some n => 
        match atoi ts with
        | None => None
        | Some ns => Some (n* (pow 10 len) + ns)
        end  
      end
    end.

  Definition itoa (n: nat) : string :=
    itoaT 1000 n "".
End NatSerializer.
