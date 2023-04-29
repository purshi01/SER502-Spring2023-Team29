
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