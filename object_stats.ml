[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let printf = Printf.printf

module String = struct
  include String
  module Set = Misc.StringSet
end

type stats = {
  mutable direct_calls : String.Set.t;
  mutable c_calls : String.Set.t;
  mutable indirect_calls : int;
  mutable allocation_points : int;
  mutable may_allocate : bool;
  mutable may_raise : bool;
  mutable bounded_stack_space : bool;
}


let analyse ~(fundecl : Linearize.fundecl) =
  let stats : stats = {
    direct_calls = String.Set.empty;
    c_calls = String.Set.empty;
    indirect_calls = 0;
    allocation_points = 0;
    may_allocate = false;
    may_raise = false;
    bounded_stack_space = true;
  }
  in
  let insn = ref fundecl.fun_body in
  while not ((!insn).next == !insn) do
    begin match (!insn).desc with
    | Lop op ->
      begin match op with
      | Icall_ind ->
        stats.indirect_calls <- stats.indirect_calls + 1;
        stats.may_allocate <- true;
        stats.may_raise <- true;
        stats.bounded_stack_space <- false
      | Icall_imm func ->
        stats.direct_calls <- String.Set.add func stats.direct_calls;
        stats.bounded_stack_space <- false;
        if not (func == fundecl.fun_name) then begin
          stats.may_allocate <- true;
          stats.may_raise <- true
        end
      | Itailcall_ind ->
        stats.indirect_calls <- stats.indirect_calls + 1;
        stats.may_allocate <- true;
        stats.may_raise <- true;
        stats.bounded_stack_space <- false
      | Itailcall_imm func ->
        stats.direct_calls <- String.Set.add func stats.direct_calls;
        if not (func == fundecl.fun_name) then begin
          stats.may_allocate <- true;
          stats.may_raise <- true;
          stats.bounded_stack_space <- false
        end
      | Iextcall (func, false) ->
        stats.c_calls <- String.Set.add func stats.c_calls;
        stats.bounded_stack_space <- false
      | Iextcall (func, true) ->
        stats.c_calls <- String.Set.add func stats.c_calls;
        stats.may_allocate <- true;
        stats.may_raise <- true;
        stats.bounded_stack_space <- false
      | Ialloc _ ->
        stats.allocation_points <- stats.allocation_points + 1;
        stats.may_allocate <- true;
        (* Allocation points may unfortunately raise or run finalizers. *)
        stats.may_raise <- true;
        stats.bounded_stack_space <- false
      | Imove
      | Ispill
      | Ireload
      | Iconst_int _
      | Iconst_float _
      | Iconst_symbol _
      | Iconst_blockheader _
      | Istackoffset _ | Iload _ | Istore _ -> ()
      | Iintop op | Iintop_imm (op, _) ->
        begin match op with
        | Iadd | Isub | Imul | Imulh | Idiv | Imod
        | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
        | Icomp _ -> ()
        | Icheckbound -> stats.may_raise <- true
        end
      | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
      | Ifloatofint | Iintoffloat -> ()
      | Ispecific _ ->
        (* For x86-64, these don't raise, allocate or call. *)
        ()
      end
    | Lend
    | Lreloadretaddr
    | Lreturn
    | Llabel _
    | Lbranch _
    | Lcondbranch _
    | Lcondbranch3 _
    | Lswitch _
    | Lsetuptrap _
    | Lpushtrap
    | Lpoptrap -> ()
    | Lraise _ -> stats.may_raise <- true
    end;
    insn := (!insn).next
  done;
  let print_string_set set =
    String.Set.iter (fun str -> printf "   %S\n" str) set
  in
  printf "((function %S)\n" fundecl.fun_name;
  printf " (location %S)\n" (Debuginfo.to_string fundecl.fun_dbg);
  printf " (direct_calls_to_ocaml_code (\n";
  print_string_set stats.direct_calls;
  printf " )\n (direct_calls_to_c_code (\n";
  print_string_set stats.c_calls;
  printf " )\n (num_indirect_calls_to_ocaml_code %d)\n" stats.indirect_calls;
  printf " (may_allocate %b)\n" stats.may_allocate;
  printf " (num_inline_allocation_points %d)\n" stats.allocation_points;
  printf " (may_raise_exception %b)\n" stats.may_raise;
  printf " (definitely_bounded_stack_space %b)\n)\n\n" stats.bounded_stack_space

let () =
  if Array.length Sys.argv <> 2 then begin
    failwith "Syntax: object_stats <LINEARIZE FILE>"
  end;
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let finished = ref false in
  while not !finished do
    match ((Marshal.from_channel chan) : Linearize.fundecl) with
    | fundecl -> analyse ~fundecl
    | exception _ -> finished := true
  done;
  close_in chan
