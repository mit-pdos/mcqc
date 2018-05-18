Require Export Coq.Lists.List.
Require Eqdep.
Require Import RelationClasses.
Import ListNotations.

Set Implicit Arguments.

Ltac sigT_eq :=
  match goal with
  | [ H: existT ?P ?a _ = existT ?P ?a _ |- _ ] =>
    apply Eqdep.EqdepTheory.inj_pair2 in H; subst
  end.

Ltac inv H := inversion H; subst; clear H; repeat sigT_eq.


Inductive opT : Type -> Type :=
| print : nat -> opT unit
| getline : opT nat.

Inductive proc: Type -> Type :=
| ret: forall T, T -> proc T
| bind: forall T T', proc T -> (T -> proc T') -> proc T'
| runlist: forall T, list (opT T) -> proc unit
| prim: forall T, opT T -> proc T.
Definition event := sigT (fun T => opT T).

(*Definition event := {T & opT T}. *)
Definition ev T (op:opT T) : event :=
  existT _ _ op.

Inductive step : forall T, opT T -> T -> Prop :=
| step_print : forall n,
    step (print n) tt
| step_getline : forall n,
    step getline n
.

Definition trace := list event.

Inductive step_list : forall T, list (opT T) -> trace -> Prop :=
| step_list_nil : forall T, step_list (@nil (opT T)) nil
| step_list_cons : forall T (op:opT T) (l: list (opT T)) r tr,
    step op r ->
    step_list l tr ->
    step_list (op::l) (ev op::tr)
.

(* [exec p r tr] means that p runs and returns r, while printing tr *)
Inductive exec : forall T, proc T -> T -> trace -> Prop :=
| exec_prim : forall T (op:opT T) r,
    step op r ->
    exec (prim op) r [ev op]
| exec_ret : forall T (v:T),
    exec (ret v) v []
| exec_bind : forall T T' (p: proc T) (p': T -> proc T') r tr r' tr',
    exec p r tr ->
    exec (p' r) r' tr' ->
    exec (bind p p') r' (tr ++ tr')
| exec_runlist : forall T (ops: list (opT T)) tr,
    step_list ops tr ->
    exec (runlist ops) tt tr
.

Hint Constructors exec.

Definition equiv T (p p':proc T) : Prop :=
  forall r tr, exec p r tr <-> exec p' r tr.

Definition Print a := prim (print a).

Hint Constructors step_list.

Theorem ret_getlist : forall T,
    equiv (ret tt) (@runlist T []).
Proof.
  intros.
  unfold equiv; split; intros.
  destruct r.
  inv H; eauto.
  inv H.
  inv H3; eauto.
Qed.

Theorem bind_getlist : forall T (op:opT T) l,
    equiv (bind (prim op) (fun _ => runlist l))
          (runlist (op::l)).
Admitted.

Theorem bind_congruence : forall T (p p': proc T) T' (rx rx': T -> proc T'),
    equiv p p' ->
    (forall x, equiv (rx x) (rx' x)) ->
    equiv (bind p rx) (bind p' rx').
Admitted.

Instance equiv_Equivalence T : Equivalence (equiv (T:=T)).
constructor; hnf; intros.
firstorder.
firstorder.
unfold equiv, iff in *; intros.
specialize (H r tr).
specialize (H0 r tr).
intuition auto.
Defined.

(** Small example *)
Definition foo := bind (Print 2)
                       (fun _ => bind (Print 1)
                                   (fun _ => ret tt)).

Definition foo' := runlist [print 2; print 1].

Definition foo_opt : {foo_opt | equiv foo foo_opt}.
  unfold foo, Print.
  eexists.

  etransitivity.
  eapply bind_congruence; intros.
  reflexivity.

  etransitivity.
  eapply bind_congruence; intros.
  reflexivity.
  eapply ret_getlist.

  repeat match goal with
  | |- equiv (bind (prim ?op) ?rx) _ =>
    match eval simpl in (rx tt) with
    | runlist ?l => eapply (bind_getlist op l)
    end
  end.

  match goal with
  | |- equiv (bind (prim ?op) ?rx) _ =>
    match eval simpl in (rx tt) with
    | runlist ?l => eapply (bind_getlist op l)
    end
  end.
Defined.

Definition foo_p := ltac:(let x := eval simpl in (proj1_sig foo_opt) in
                              match x with
                              | runlist ?l => exact l
                              end).

Print foo_p.