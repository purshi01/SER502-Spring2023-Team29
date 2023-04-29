% Runtime Semantics

% Update Environment
update_s(t_id(K_s), V, Type_s, Env_s, FinalEnv_s) :- update_s(K_s, V, Type_s, Env_s, FinalEnv_s).
update_s(K_s, V, Type_s, [], [(K_s, V, Type_s)]) :- K_s \= t_id(_).
update_s(K_s, V, Type_s, [(K_s, _, _)|T], [(K_s, V, Type_s)|T]) :- K_s \= t_id(_).
update_s(K_s, V, Type_s, [H|T], [H|R]) :- K_s \= t_id(_), H \= (K_s,_,_), update_s(K_s, V, Type_s, T, R).


% Lookup Value in Environment
lookup_s(t_id(K_s), Env_s, V, Type_s) :- lookup_s(K_s, Env_s, V, Type_s).
lookup_s(K_s, [(K_s,V,Type_s)|_], V, Type_s) :- K_s \= t_id(_).
lookup_s(K_s, [_|T], V, Type_s) :- K_s \= t_id(_), lookup_s(K_s, T, V, Type_s).


% Check if an identifier is present in env.
check_present_s(t_id(K_s),Env_s) :- check_present_s(K_s, Env_s).
check_present_s(K_s,[(K_s,_,_)|_]).
check_present_s(K_s,[(H,_,_)|T]) :- K_s \= H, check_present_s(K_s,T).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% List operations
addAtIndex_s(_ValToAdd_s, List, Index_s, _FinalList) :- length(List, Length), Index_s >= Length + 2, write("Length of the list is "), write(Length), write(". Please provide correct index."), nl, fail.
addAtIndex_s(_ValToAdd_s, _List, Index_s, _FinalList) :- Index_s =< 0, write("Please provide index greater than 0."), nl, fail.
addAtIndex_s(ValToAdd_s, List, Index_s, FinalList) :- length(List, Length), Index_s < Length + 2, Index_s > 0, addAtIndex_s(ValToAdd_s, List, 1, Index_s, FinalList).
addAtIndex_s(ValToAdd_s, List, Iterator_s, Index_s, [ValToAdd_s|List]) :- Iterator_s =:= Index_s.
addAtIndex_s(ValToAdd_s, [H|T], Iterator_s, Index_s, [H|FinalList]) :- Iterator_s < Index_s, NextIterator = Iterator_s + 1, addAtIndex_s(ValToAdd_s, T, NextIterator, Index_s, FinalList).

deleteAtIndex_s(Index_s, List, _FinalList) :- length(List, Length), Index_s > Length, write("Length of the list is "), write(Length), write(". Please provide correct index."), nl, fail.
deleteAtIndex_s(Index_s, _List, _FinalVal) :- Index_s =< 0, write("Please provide index greater than 0."), nl, fail.
deleteAtIndex_s(Index_s, List, FinalList) :- length(List, Length), Index_s =< Length, Index_s > 0, deleteAtIndex_s(Index_s, 1, List, FinalList).
deleteAtIndex_s(Index_s, Iterator_s, [_|T], T) :- Index_s =:= Iterator_s.
deleteAtIndex_s(Index_s, Iterator_s, [H|T], [H|FinalList]) :- Iterator_s < Index_s, NextIterator = Iterator_s + 1, deleteAtIndex_s(Index_s, NextIterator, T, FinalList).

getAtIndex_s(Index_s, List, _Val) :- length(List, Length), Index_s > Length, write("Length of the list is "), write(Length), write(". Please provide correct index."), nl, fail.
getAtIndex_s(Index_s, _List, _Val) :- Index_s =< 0, write("Please provide index greater than 0."), nl, fail.
getAtIndex_s(Index_s, List, Val_s) :- length(List, Length), Index_s =< Length, Index_s > 0, getAtIndex_s(Index_s, 1, List, Val_s).
getAtIndex_s(Index_s, Iterator_s, [Val_s|_], Val_s) :- Index_s =:= Iterator_s.
getAtIndex_s(Index_s, Iterator_s, [_|T], Val_s) :- Iterator_s < Index_s, NextIterator = Iterator_s + 1, getAtIndex_s(Index_s, NextIterator, T, Val_s).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Expression
eval_expr_s(t_assign(t_id(X_s), Y_s), Env_s, FinalEnv_s, Val_s):- check_present_s(X_s, Env_s),
    eval_expr_s(Y_s, Env_s, Env1_s, Val_s), update_s(X_s, Val_s, num, Env1_s, FinalEnv_s).

eval_expr_s(t_assign(t_id(X_s), _Y), Env_s, _FinalEnv, _Val):- \+check_present_s(X_s, Env_s),
    write("Variable not initialised. Please check."),nl, abort.

eval_expr_s(t_assign(t_id(X_s), _Y), Env_s, _FinalEnv, _Val):- lookup_s(X_s, Env_s, _, Type_s),
    Type_s \= num, write("This operation can only be perfomed on num type of variable. Please check."),nl, abort.

eval_expr_s(t_add(X_s, Y_s), Env_s, FinalEnv_s, Val_s):- eval_expr_s(X_s, Env_s, Env1_s, V1_s),
                                             eval_expr_s(Y_s, Env1_s, FinalEnv_s, V2_s),
                                             Val_s is V1_s + V2_s.

eval_expr_s(t_sub(X_s, Y_s), Env_s, FinalEnv_s, Val_s):- eval_expr_s(X_s, Env_s, Env1_s, V1_s),
                                             eval_expr_s(Y_s, Env1_s, FinalEnv_s, V2_s),
                                             Val_s is V1_s - V2_s.

eval_expr_s(t_div(X_s, Y_s), Env_s, FinalEnv_s, Val_s):- eval_expr_s(X_s, Env_s, Env1_s, V1_s),
                                             eval_expr_s(Y_s, Env1_s, FinalEnv_s, V2_s),
    							   			 Val_s is V1_s / V2_s.

eval_expr_s(t_mul(X_s, Y_s), Env_s, FinalEnv_s, Val_s):- eval_expr_s(X_s, Env_s, Env1_s, V1_s),
                                             eval_expr_s(Y_s, Env1_s, FinalEnv_s, V2_s),
    							   			 Val_s is V1_s * V2_s.

eval_expr_s(t_num(X_s), Env_s, Env_s, X_s).

eval_expr_s(t_id(X_s), Env_s, Env_s, Val_s):- check_present_s(X_s, Env_s), lookup_s(X_s, Env_s, Val_s, num).

eval_expr_s(t_id(X_s), Env_s, Env_s, _Val):- \+check_present_s(X_s, Env_s), write("Variable not initialised. Please check."),nl, abort.

eval_expr_s(t_id(X_s), Env_s, Env_s, Val_s):- lookup_s(X_s, Env_s, Val_s, Type_s), Type_s \= num,
    write("This operation can only be perfomed on num type of variable. Please check."),nl, abort.

eval_expr_s(t_stack(X_s), Env_s, FinalEnv_s, Val_s) :- eval_stack_pt_s(X_s, Val_s, Env_s, FinalEnv_s).

eval_expr_s(t_queue(X_s), Env_s, FinalEnv_s, Val_s) :- eval_queue_pt_s(X_s, Val_s, Env_s, FinalEnv_s).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Boolean Expression
not(true, false).
not(false, true).

eval_bool_s(false, Env_s, Env_s, false).
eval_bool_s(true, Env_s, Env_s, true).
eval_bool_s(t_id(X_s), Env_s, Env_s, Val_s) :- check_present_s(X_s, Env_s), lookup_s(X_s, Env_s, Val_s, bool).
eval_bool_s(t_id(X_s), Env_s, Env_s, Val_s):- lookup_s(X_s, Env_s, Val_s, Type_s), Type_s \= bool,
    write("This operation can only be perfomed on boolean type of variable. Please check."), nl, abort.

eval_bool_s(t_id(X_s), Env_s, Env_s, _Val):- \+check_present_s(X_s, Env_s), write("Variable not initialised. Please check."),
    nl, abort.

eval_bool_s(t_notbool(not, X_s), Env_s, FinalEnv_s, Val_s) :- eval_bool_s(X_s, Env_s, FinalEnv_s, V1_s), not(V1_s, Val_s).


eval_bool_s(t_bool_operation(X_s, Y_s, Z), Env_s, FinalEnv_s, Val_s) :- eval_bool_s(X_s, Env_s, Env1_s, V1_s),
    eval_bool_s(Z, Env1_s, FinalEnv_s, V2_s),
    eval_bool_operator_s(Y_s, V1_s, V2_s, Val_s).

eval_bool_s(t_bool(X_s, Y_s, Z), Env_s, FinalEnv_s, Val_s):- eval_expr_s(X_s, Env_s, Env1_s,V1_s),
    eval_expr_s(Z, Env1_s, FinalEnv_s,V2_s),
    eval_compare_s(Y_s, V1_s, V2_s, Val_s).

eval_compare_s(t_comp_op(>), V1_s, V2_s, true):- V1_s > V2_s.
eval_compare_s(t_comp_op(>), V1_s, V2_s, false):- V1_s =< V2_s.

eval_compare_s(t_comp_op(<), V1_s, V2_s, true):- V1_s < V2_s.
eval_compare_s(t_comp_op(<), V1_s, V2_s, false):- V1_s >= V2_s.

eval_compare_s(t_comp_op(==), V1_s, V2_s, true):- V1_s =:= V2_s.
eval_compare_s(t_comp_op(==), V1_s, V2_s, false):- V1_s =\= V2_s.

eval_compare_s(t_comp_op(<=), V1_s, V2_s, true):- V1_s =< V2_s.
eval_compare_s(t_comp_op(<=), V1_s, V2_s, false):- V1_s > V2_s.

eval_compare_s(t_comp_op(>=), V1_s, V2_s, true):- V1_s >= V2_s.
eval_compare_s(t_comp_op(>=), V1_s, V2_s, false):- V1_s < V2_s.

eval_compare_s(t_comp_op(=\=), V1_s, V2_s, false):- V1_s =:= V2_s.
eval_compare_s(t_comp_op(=\=), V1_s, V2_s, true):- V1_s =\= V2_s.

% And OR boolean operations
eval_bool_operator_s(t_bool_op_and(and),false,true,false).
eval_bool_operator_s(t_bool_op_and(and),false,false,false).
eval_bool_operator_s(t_bool_op_and(and),true,false,false).
eval_bool_operator_s(t_bool_op_and(and),true,true,true).

eval_bool_operator_s(t_bool_op_or(or),false,true,true).
eval_bool_operator_s(t_bool_op_or(or),false,false,false).
eval_bool_operator_s(t_bool_op_or(or),true,false,true).
eval_bool_operator_s(t_bool_op_or(or),true,true,true).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Ternary Statement
eval_ternary_s(t_ternary(X_s, Y_s, _), Env_s, FinalEnv_s, Val_s) :- eval_bool_s(X_s, Env_s, Env1_s, true),
    eval_expr_s(Y_s, Env1_s, FinalEnv_s, Val_s).

eval_ternary_s(t_ternary(X_s, _, Z), Env_s, FinalEnv_s, Val_s) :- eval_bool_s(X_s, Env_s, Env1_s, false),
    eval_expr_s(Z, Env1_s, FinalEnv_s, Val_s).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate String Manipulation
eval_string_type_s(t_string_concat_id(X_s), Env_s, V):- lookup_s(X_s, Env_s, V, str).
eval_string_type_s(t_string_concat_id(X_s), Env_s, _V):- \+check_present_s(X_s, Env_s),
    write("Variable not initialised. Please check."), nl, abort.
eval_string_type_s(t_string_concat_id(X_s), Env_s, _V):- lookup_s(X_s, Env_s, _Val, Type_s),
    Type_s \= str,
    write("This operation can only be perfomed on string type of variable. Please check."),
    nl, abort.
eval_string_type_s(t_string_concat_str(X_s), _Env_s, X_s).
eval_string_concat(t_string_concat(X_s, Y_s), Env_s, Env_s, Val_s) :- eval_string_type_s(X_s, Env_s, V1_s),
    eval_string_type_s(Y_s, Env_s, V2_s), string_concat(V1_s, V2_s, Val_s).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Declaration Statements
eval_declaration_s(t_declaration_bool_assign(t_id(X_s),Y_s), Env_s, FinalEnv_s) :-
    eval_bool_s(Y_s, Env_s, Env1_s, Val_s), update_s(X_s, Val_s, bool, Env1_s , FinalEnv_s).

eval_declaration_s(t_declaration_bool_assign(t_id(X_s)), Env_s, FinalEnv_s) :-
    update_s(X_s, false, bool, Env_s , FinalEnv_s).

eval_declaration_s(t_declaration_str_assign(t_id(X_s),Y_s), Env_s, FinalEnv_s) :-
    update_s(X_s, Y_s, str, Env_s , FinalEnv_s).

eval_declaration_s(t_declaration_str_assign(t_id(X_s)), Env_s, FinalEnv_s) :-
    update_s(X_s, "", str, Env_s , FinalEnv_s).

eval_declaration_s(t_declaration_str_assign_concat(X_s, Y_s), Env_s, FinalEnv_s) :-
    eval_string_concat(Y_s, Env_s, Env1_s, Val_s),
    update_s(X_s, Val_s, str, Env1_s, FinalEnv_s).

eval_declaration_s(t_declaration_num_assign(t_id(X_s),Y_s), Env_s, FinalEnv_s) :-
    eval_expr_s(Y_s, Env_s, Env1_s, Val_s), update_s(X_s, Val_s, num, Env1_s , FinalEnv_s).

eval_declaration_s(t_declaration_num_assign_ternary(t_id(X_s), Y_s), Env_s, FinalEnv_s) :-
    eval_ternary_s(Y_s, Env_s, Env1_s, Val_s), update_s(X_s, Val_s, num, Env1_s , FinalEnv_s).

eval_declaration_s(t_declaration_num_assign(t_id(X_s)), Env_s, FinalEnv_s) :-
    update_s(X_s, 0, num, Env_s, FinalEnv_s).

eval_declaration_s(t_declaration_stack_assign(t_id(X_s)), Env_s, FinalEnv_s) :-
    update_s(X_s, [], stack, Env_s, FinalEnv_s).

eval_declaration_s(t_declaration_stack_assign(t_id(X_s), Y_s), Env_s, FinalEnv_s) :-
    update_s(X_s, Y_s, stack, Env_s, FinalEnv_s).

eval_declaration_s(t_declaration_queue_assign(t_id(X_s)), Env_s, FinalEnv_s) :-
    update_s(X_s, [], queue, Env_s, FinalEnv_s).

eval_declaration_s(t_declaration_queue_assign(t_id(X_s), Y_s), Env_s, FinalEnv_s) :-
    update_s(X_s, Y_s, queue, Env_s, FinalEnv_s).

eval_declaration_s(t_declaration_list_assign(t_id(X_s), Y_s), Env_s, FinalEnv_s) :-
    update_s(X_s, Y_s, list, Env_s, FinalEnv_s).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate assign statements
eval_assignment_s(t_assignment_bool(t_id(X_s), Y_s), Env_s, FinalEnv_s) :-
    eval_bool_s(Y_s, Env_s, Env1_s, Val_s),
    update_s(X_s, Val_s, bool, Env1_s, FinalEnv_s).

eval_assignment_s(t_assignment_str(t_id(X_s), Y_s), Env_s, FinalEnv_s) :-
    update_s(X_s, Y_s, str, Env_s, FinalEnv_s).

eval_assignment_s(t_assignment_str_concat(X_s, Y_s), Env_s, FinalEnv_s) :-
    eval_string_concat(Y_s, Env_s, Env1_s, Val_s),
    update_s(X_s, Val_s, str, Env1_s , FinalEnv_s).

eval_assignment_s(t_assignment_num_assign(t_id(X_s), Y_s), Env_s, FinalEnv_s) :-
    eval_expr_s(Y_s, Env_s, Env1_s, Val_s),
	update_s(X_s, Val_s, num, Env1_s, FinalEnv_s).

eval_assignment_s(t_assignment_num_assign_ternary(t_id(X_s), Y_s), Env_s, FinalEnv_s) :-
    eval_ternary_s(Y_s, Env_s, Env1_s, Val_s),
	update_s(X_s, Val_s, num, Env1_s, FinalEnv_s).

eval_assignment_s(t_assignment_stack(t_id(X_s), Y_s), Env_s, FinalEnv_s) :-
	update_s(X_s, Y_s, stack, Env_s, FinalEnv_s).

eval_assignment_s(t_assignment_queue(t_id(X_s), Y_s), Env_s, FinalEnv_s) :-
	update_s(X_s, Y_s, queue, Env_s, FinalEnv_s).

eval_assignment_s(t_assignment_list(t_id(X_s), Y_s), Env_s, FinalEnv_s) :-
	update_s(X_s, Y_s, list, Env_s, FinalEnv_s).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Print Statements
eval_print_s(t_print(), Env_s, Env_s).
eval_print_s(t_print(X_s, Y_s), Env_s, FinalEnv_s) :- write(X_s), eval_print_s(Y_s, Env_s, FinalEnv_s).
eval_print_s(t_print_id(X_s, Y_s), Env_s, FinalEnv_s) :- lookup_s(X_s,Env_s,Val_s,_), write(Val_s), eval_print_s(Y_s, Env_s, FinalEnv_s).
eval_print_s(t_print_id(X_s, _), Env_s, Env_s) :- \+check_present_s(X_s, Env_s), write("Variable not initialised. Please check.").
eval_print_s(t_print_expr(X_s, Y_s), Env_s, FinalEnv_s) :- eval_expr_s(X_s, Env_s, Env1_s, Val_s), write(Val_s), eval_print_s(Y_s, Env1_s, FinalEnv_s).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% If else statement
eval_ifelse_stmt_s(t_ifstmt(X_s, Y_s, _), Env_s, FinalEnv_s) :- eval_bool_s(X_s, Env_s, Env1_s, true), eval_command_s(Y_s, Env1_s, FinalEnv_s).
eval_ifelse_stmt_s(t_ifstmt(X_s, _, Z), Env_s, FinalEnv_s) :- eval_bool_s(X_s, Env_s, Env1_s, false), eval_ifelse_stmt_s(Z, Env1_s, FinalEnv_s).
eval_ifelse_stmt_s(t_elifstmt(X_s, Y_s, _), Env_s, FinalEnv_s) :- eval_bool_s(X_s, Env_s, Env1_s, true), eval_command_s(Y_s, Env1_s, FinalEnv_s).
eval_ifelse_stmt_s(t_elifstmt(X_s, _, Z), Env_s, FinalEnv_s) :- eval_bool_s(X_s, Env_s, Env1_s, false), eval_ifelse_stmt_s(Z, Env1_s, FinalEnv_s).
eval_ifelse_stmt_s(t_goto_else_stmt(X_s), Env_s, FinalEnv_s) :- eval_ifelse_stmt_s(X_s, Env_s, FinalEnv_s).
eval_ifelse_stmt_s(t_elsestmt(X_s), Env_s, FinalEnv_s) :- eval_command_s(X_s, Env_s, FinalEnv_s).
eval_ifelse_stmt_s(t_elsestmt(), Env_s, Env_s) :- true.
%----------------------------------------------------------------------------------------------------------------------------------------------------------

% While statement
eval_while_s(t_statement_while(X_s,Y_s), Env_s, FinalEnv_s):- eval_bool_s(X_s, Env_s, Env1_s, true),
    eval_command_s(Y_s, Env1_s, Env2_s),
    eval_statement(t_statement_while(X_s,Y_s), Env2_s, FinalEnv_s).

eval_while_s(t_statement_while(X_s,_), Env_s, FinalEnv_s):- eval_bool_s(X_s, Env_s, FinalEnv_s, false).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% For loops
eval_for_loop_s(t_new_for(A,B,C,D), Env_s, FinalEnv_s) :-
    eval_for_loop_s(t_conventional_for(A,B,t_comp_op(<),C, t_assign(A, t_add(A, t_num(1))),D), Env_s, FinalEnv_s).

eval_for_loop_s(t_conventional_for(A,B,C,D,E,F), Env_s, FinalEnv_s) :- eval_expr_s(B, Env_s, Env1_s, Val_s),
	update_s(A, Val_s, num, Env1_s, Env2_s),
    eval_for_statement_s(t_conventional_for(A,B,C,D,E,F), Env2_s, FinalEnv_s).

eval_for_statement_s(t_conventional_for(A,B,C,D,E,F), Env_s, FinalEnv_s) :- eval_bool_s(t_bool(A, C, D), Env_s, Env1_s, true),
    eval_command_s(F, Env1_s, Env2_s),
	eval_expr_s(E, Env2_s, Env3, Val_s),
    update_s(A, Val_s, num, Env3, Env4),
	eval_for_statement_s(t_conventional_for(A,B,C,D,E,F), Env4, FinalEnv_s).

eval_for_statement_s(t_conventional_for(A,_,C,D,_,_), Env_s, Env_s) :- eval_bool_s(t_bool(A, C, D), Env_s, _, false).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate stack commands
eval_stack_s(t_stack_push(X_s, Y_s), Env_s, FinalEnv_s) :- eval_expr_s(Y_s, Env_s, Env1_s, V1_s), lookup_s(X_s, Env1_s, V2_s, stack), update_s(X_s, [V1_s|V2_s], stack, Env1_s, FinalEnv_s).

eval_stack_s(t_stack_pt(X_s), Env_s, FinalEnv_s) :- eval_stack_pt_s(X_s, _Val, Env_s, FinalEnv_s).
eval_stack_pt_s(t_stack_pop(X_s), Val_s, Env_s, FinalEnv_s) :- lookup_s(X_s, Env_s, [Val_s|Rest], stack), update_s(X_s, Rest, stack, Env_s, FinalEnv_s).
eval_stack_pt_s(t_stack_pop(X_s), _Val, Env_s, Env_s) :- lookup_s(X_s, Env_s, [], stack), write("Stack "), write(X_s), write(" is empty."), abort.
eval_stack_pt_s(t_stack_top(X_s), Val_s, Env_s, Env_s) :- lookup_s(X_s, Env_s, [Val_s|_], stack).
eval_stack_pt_s(t_stack_top(X_s), _Val, Env_s, Env_s) :- lookup_s(X_s, Env_s, [], stack), write("Stack "), write(X_s), write(" is empty."), nl.

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate queue commands
eval_queue_s(t_queue_push(X_s, Y_s), Env_s, FinalEnv_s) :- eval_expr_s(Y_s, Env_s, Env1_s, V1_s), lookup_s(X_s, Env1_s, V2_s, queue), append(V2_s, [V1_s], FinalVal), update_s(X_s, FinalVal, queue, Env1_s, FinalEnv_s).

eval_queue_s(t_queue_pt(X_s), Env_s, FinalEnv_s) :- eval_queue_pt_s(X_s, _Val, Env_s, FinalEnv_s).
eval_queue_pt_s(t_queue_poll(X_s), Val_s, Env_s, FinalEnv_s) :- lookup_s(X_s, Env_s, [Val_s|Rest], queue), update_s(X_s, Rest, queue, Env_s, FinalEnv_s).
eval_queue_pt_s(t_queue_poll(X_s), _Val, Env_s, Env_s) :- lookup_s(X_s, Env_s, [], queue), write("Queue "), write(X_s), write(" is empty."), abort.
eval_queue_pt_s(t_queue_head(X_s), Val_s, Env_s, Env_s) :- lookup_s(X_s, Env_s, [Val_s|_], queue).
eval_queue_pt_s(t_queue_head(X_s), _Val, Env_s, Env_s) :- lookup_s(X_s, Env_s, [], queue), write("Queue "), write(X_s), write(" is empty."), nl.

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate list commands
eval_list_s(t_add(X_s, t_num(Y_s)), Env_s, FinalEnv_s) :- lookup_s(X_s, Env_s, Val_s, list), append(Val_s, [Y_s], FinalVal), update_s(X_s, FinalVal, list, Env_s, FinalEnv_s).
eval_list_s(t_add(X_s, t_num(ValToAdd_s), t_num(Index_s)), Env_s, FinalEnv_s) :- lookup_s(X_s, Env_s, Val_s, list), addAtIndex_s(ValToAdd_s, Val_s, Index_s, FinalVal), update_s(X_s, FinalVal, list, Env_s, FinalEnv_s).
eval_list_s(t_remove(X_s, t_num(Index_s)), Env_s, FinalEnv_s) :- lookup_s(X_s, Env_s, Val_s, list), deleteAtIndex_s(Index_s, Val_s, FinalVal), update_s(X_s, FinalVal, list, Env_s, FinalEnv_s).
eval_list_s(t_get(X_s, t_num(Index_s)), Env_s, Env_s) :- lookup_s(X_s, Env_s, List, list), getAtIndex_s(Index_s, List, Val_s), write(Val_s), nl.

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Method
find_val_s(A, Val_s, Type_s, Env_s):- lookup_s(A, Env_s, Val_s, Type_s).
find_val_s(t_str(A), A, _Type, _Env_s).
find_val_s(t_num(A), A, _Type, _Env_s).

% Create a local environment for method_s
form_method_env_s(t_formal_parameter(), t_actual_parameter(), _Env_s, NewEnv_s, NewEnv_s).
form_method_env_s(t_formal_parameter(X_s, Y_s), t_actual_parameter(A, B), Env_s, NewEnv_s, NewFinalEnv) :-
    find_val_s(A, Val_s, Type_s, Env_s),
    update_s(X_s, Val_s, Type_s, NewEnv_s, NewEnv1_s),
    form_method_env_s(Y_s, B, Env_s, NewEnv1_s, NewFinalEnv).

% Evaluate body of method_s
eval_body_s(t_body(X_s), Env_s) :- eval_command_s(X_s, Env_s, _FinalEnv).

% Method Declaration evaluation
eval_method_s(t_method_declaration(FuncName, Parameters, Body), Env_s, FinalEnv_s) :-
    update_s(FuncName, (Parameters, Body), method_s, Env_s, FinalEnv_s).

% Method Call evaluation
eval_method_s(t_method_call(MethodName_s, ActualParameters), Env_s, Env_s) :-
    lookup_s(MethodName_s, Env_s, (FormalParameters, Body), method_s),
    form_method_env_s(FormalParameters, ActualParameters, Env_s, [], FinalMethodEnv),
    eval_body_s(Body, FinalMethodEnv).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Statements
eval_statement(t_statement_declaration(X_s), Env_s, FinalEnv_s) :- eval_declaration_s(X_s, Env_s, FinalEnv_s).
eval_statement(t_statement_assign(X_s), Env_s, FinalEnv_s) :- eval_assignment_s(X_s, Env_s, FinalEnv_s).
eval_statement(t_statement_print(X_s), Env_s, FinalEnv_s) :- eval_print_s(X_s, Env_s, FinalEnv_s).
eval_statement(t_statement_ifelse(X_s), Env_s, FinalEnv_s) :- eval_ifelse_stmt_s(X_s, Env_s, FinalEnv_s).
eval_statement(t_statement_while(X_s, Y_s), Env_s, FinalEnv_s) :- eval_while_s(t_statement_while(X_s, Y_s), Env_s, FinalEnv_s).
eval_statement(t_statement_for(X_s), Env_s, FinalEnv_s) :- eval_for_loop_s(X_s, Env_s, FinalEnv_s).
eval_statement(t_statement_stack(X_s), Env_s, FinalEnv_s) :- eval_stack_s(X_s, Env_s, FinalEnv_s).
eval_statement(t_statement_queue(X_s), Env_s, FinalEnv_s) :- eval_queue_s(X_s, Env_s, FinalEnv_s).
eval_statement(t_statement_list(X_s), Env_s, FinalEnv_s) :- eval_list_s(X_s, Env_s, FinalEnv_s).
eval_statement(t_statement_method(X_s), Env_s, FinalEnv_s) :- eval_method_s(X_s, Env_s, FinalEnv_s).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Command
eval_command_s(t_command(), Env_s, Env_s).
eval_command_s(t_command(X_s, Y_s), Env_s, FinalEnv_s) :- eval_statement(X_s, Env_s, Env1_s), eval_command_s(Y_s, Env1_s, FinalEnv_s).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Block
eval_block(t_block(X_s), Env_s, FinalEnv_s):- eval_command_s(X_s, Env_s, FinalEnv_s).

program_eval(t_program(X_s), Env_s):- eval_block(X_s, [], Env_s).