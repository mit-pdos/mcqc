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

Require Extraction.
Extraction Language Haskell.
Extract Inductive nat => "Prelude.Integer" [ "0" "Prelude.succ" ].
Extract Inductive bool => "Prelude.Bool" [ "true" "false" ].
Extract Inductive unit => "()" [ "()" ].

Parameter filename : Type.
Parameter fd : Type.
Parameter data : Type.
Definition pathname := list filename.

Inductive opT : Type -> Type :=
| print : nat -> opT unit
| getline : opT nat
(** pseudomail Proc *)
| open : pathname -> opT fd
| write : fd -> data -> opT unit
| close : fd -> opT unit
| pidfn : opT filename
| random : opT  filename
| link : pathname -> pathname -> opT bool
| unlink : pathname -> opT unit
| whilefalse : opT bool -> opT unit.

Inductive proc: Type -> Type :=
| ret: forall T, T -> proc T
| bind: forall T T', proc T -> (T -> proc T') -> proc T'
| runlist: forall T, list (opT T) -> proc unit
| prim: forall T, opT T -> proc T.
Definition event := sigT (fun T => opT T).

Definition Print a := prim (print a).
Definition Getline := prim (getline).
Definition Random := prim (random).
Definition Close a := prim (close a).
Definition Pidfn := prim (pidfn).
Definition Link s d := prim (link s d).
Definition Unlink f := prim (unlink f).
Definition Whilefalse b := prim (whilefalse b).

Notation "x <- p1 ; p2" := (bind p1 (fun x => p2))
  (at level 60, right associativity).

Definition ev T (op:opT T) : event :=
  existT _ _ op.

Inductive step : forall T, opT T -> T -> Prop :=
| step_print : forall n,
    step (print n) tt
| step_getline : forall n,
    step getline n
| step_open: forall p f,
    step (open p) f
| step_write: forall f d,
    step (write f d) tt
| step_close: forall f,
    step (close f) tt
| step_pidfn: forall f,
    step pidfn f
| step_random: forall n,
    step random n
| step_link: forall s d,
    step (link s d) true
| step_unlink: forall p,
    step (unlink p) tt
| step_whilefalse: forall b,
    step (whilefalse b) tt
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
Proof.
  intros.
  induction l.
  constructor.
  destruct r.
  constructor.
  inv H.
  inv H6.
  inv H7.
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
Definition foo :=
  _ <- Print 1;
  _ <- Print 2;
  ret tt.

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

  match goal with
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
Separate Extraction foo_p.

(**
(** Another example *)
Definition bar :=
  _ <- Print 1;
  n <- Getline;
  _ <- Print n;
  ret tt.

(** equiv (runlist []) (?rx' x3) *)
Definition bar_opt : {bar_opt | equiv bar bar_opt}.
  unfold bar, Print.
  eexists.

  etransitivity.
  eapply bind_congruence; intros.
  reflexivity.

  etransitivity.
  eapply bind_congruence; intros.
  reflexivity.

  etransitivity.
  eapply bind_congruence; intros.
  reflexivity.

  eapply ret_getlist.

  etransitivity.
  eapply bind_congruence; intros.
  reflexivity.

  etransitivity.
  constructor.

  intro.
  apply H.
  intro.
  apply H.
  constructor.
  intro.
  apply H.
  intros.
  apply H.
  constructor.
  eapply bind_congruence; intros.
  reflexivity.
  constructor.
  intro.
  apply H.
  intro. apply H.
  intros.
  eapply bind_congruence; intros.
  reflexivity.
  constructor.
  intros. apply H0.
  intros. apply H0.
  eapply bind_congruence; intros.
  reflexivity.
  constructor.
  intros. apply H0.
  intros. apply H0.
  eapply bind_congruence; intros.
  reflexivity.
  constructor.
  intros. apply H0.
  intros. apply H0.
  eapply bind_congruence; intros.
  reflexivity.
  constructor.
  intros. apply H0.
  intros. apply H0.
  eapply bind_congruence; intros.
  reflexivity.
  constructor.
  intros. apply H0.
  intros. apply H0.
  eapply bind_congruence; intros.
  reflexivity.
  constructor.
  intros. apply H0.
  intros. apply H0.
  eapply bind_congruence; intros.
  reflexivity.
  constructor.
  intros. apply H0.
  intros. apply H0.
Admitted.

Definition bar_p := ltac:(let x := eval simpl in (proj1_sig bar_opt) in
                              match x with
                              | runlist ?l => exact l
                              end).

Print bar_p.


Definition mail_deliver (msg : data) (tmpdir : pathname) (mboxdir : pathname) :=
  tmpfn <- pidfn;
  fd <- open (tmpdir ++ [tmpfn]);
  _ <- write fd msg;
  _ <- close fd;
  whilefalse
    ( mailfn <- random;
      ok <- link (tmpdir ++ [tmpfn]) (mboxdir ++ [mailfn]);
      if ok then
        _ <- unlink (tmpdir ++ [tmpfn]);
        ret true
      else
        ret false ).

*)
