% Parse Tree Generator
%:- use_rendering(svgtree).

:- use_module(library(tabling)).
:- table expr_op_s/3, term_s/3, bool_s/3.


% Reserved Keywords in language
reserved_keywords_s([def, in, not,num, true, false, print,+,-,*, bool_s,elif, stack, queue, '(',')', str, while, for, and, in, if, else,'{','}',/,=]).
check_reserved_keywords_s(X):- reserved_keywords_s(L), \+ member(X, L).

% Update Environment
update_s(t_id(K_s), Type_s, Env_s, FinalEnv_s) :- updte_s(K_s, Type_s, Env_s, FinalEnv_s).
updte_s(K_s, Type_s, [], [(K_s,Type_s)]).
updte_s(K_s, Type_s, [(K_s, _)|T_s], [(K_s, Type_s)|T_s]).
updte_s(K_s, Type_s, [H|T_s], [H|R]) :- H \= (K_s,_), updte_s(K_s, Type_s, T_s, R).


% Lookup Value in Environment
lookup_s(t_id(K_s), Type_s, Env_s) :- look_up_s(K_s, Type_s, Env_s).
look_up_s(K_s, _Type, []) :- write(Variable "), write(K_s), write(" not defined properly \n"), abort.
look_up_s(K_s, Type_s, [(K_s,Type_s)|_T]).
look_up_s(K1, Type_s, [(K2,_T2)|T_s]) :- K1 \= K2, look_up_s(K1, Type_s, T_s).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Expressions
expr_s(t_assign(X, Y)) --> identifier_s(X), [=], expr_op_s(Y).
expr_s(X) --> expr_op_s(X).

expr_op_s(t_add(X, Y))-->expr_op_s(X), [+], term_s(Y).
expr_op_s(t_sub(X, Y))-->expr_op_s(X), [-], term_s(Y).
expr_op_s(X) --> term_s(X).

term_s(t_div(X, Y))-->term_s(X), [/], brackets_s(Y).
term_s(t_mul(X, Y)) --> term_s(X), [*], brackets_s(Y).
term_s(X) --> brackets_s(X).

brackets_s(X) --> [('], expr_s(X), [')'].
brackets_s(X) --> num(X).
brackets_s(X) --> identifier_s(X).
brackets_s(t_stack(X)) --> stack_pt_s(X).
brackets_s(t_queue(X)) --> queue_pt_s(X).

identifier_s(t_id(X)) -->[X], {check_reserved_keywords_s(X)}, {atom(X)}.
num(t_num(X)) --> [X], {number(X)}.

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Boolean Operators
boolean_operator_s(t_bool_op_and(and))  --> [and].
boolean_operator_s(t_bool_op_or(or))  --> [or].

% Boolean Operations
bool_s(t_bool_operation(X,Y,Z)) --> bool_s(X), boolean_operator_s(Y), boolean_s(Z).
bool_s(X) --> boolean_s(X).
boolean_s(t_bool(X,Y,Z)) --> expr_s(X), comparison_operator_s(Y), expr_s(Z).
boolean_s(t_notbool(not, X)) --> [not], boolean_s(X).
boolean_s(X) --> identifier_s(X).
boolean_s(true) --> [true].
boolean_s(false) --> [false].
boolean_s(X) --> brkt_bool(X).
brkt_bool(X)-->['('], bool_s(X), [')'].

% Comparison Operators
comparison_operator_s(t_comp_op(>)) --> [>].
comparison_operator_s(t_comp_op(<)) --> [<].
comparison_operator_s(t_comp_op(==)) --> [==].
comparison_operator_s(t_comp_op(<=)) --> [<=].
comparison_operator_s(t_comp_op(>=)) --> [>=].
comparison_operator_s(t_comp_op(=\=)) --> ["!="].

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Ternary Operation
ternary_op_s(t_ternary(X, Y, Z)) --> bool_s(X), [?], expr_s(Y), [:], expr_s(Z).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% String Manipulation
string_type_s(t_string_concat_id(X)) --> identifier_s(X).
string_type_s(t_string_concat_str(X)) --> [X], {string(X)}.
string_add(t_string_concat(X, Y)) --> string_type_s(X), [+], string_type_s(Y).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Declaration statements
declaration_s(Env_s, FinalEnv_s, t_declaration_bool_assign(X, Y)) --> [boolean_s], identifier_s(X), [=], bool_s(Y), {update_s(X, bool_s, Env_s, FinalEnv_s)}.
declaration_s(Env_s, FinalEnv_s, t_declaration_bool_assign(X)) --> [boolean_s], identifier_s(X), {update_s(X, bool_s, Env_s, FinalEnv_s)}.
declaration_s(Env_s, FinalEnv_s, t_declaration_str_assign(X, Y)) --> [string], identifier_s(X), [=], [Y], {string(Y)}, {update_s(X, str, Env_s, FinalEnv_s)}.
declaration_s(Env_s, FinalEnv_s, t_declaration_str_assign(X)) --> [string], identifier_s(X),{update_s(X, str, Env_s, FinalEnv_s)}.
declaration_s(Env_s, FinalEnv_s, t_declaration_str_assign_concat(X, Y)) --> [string], identifier_s(X), [=], string_add(Y), {update_s(X, str, Env_s, FinalEnv_s)}.
declaration_s(Env_s, FinalEnv_s, t_declaration_num_assign(X, Y)) --> [num], identifier_s(X), [=], expr_s(Y), {update_s(X, num, Env_s, FinalEnv_s)}.
declaration_s(Env_s, FinalEnv_s, t_declaration_num_assign(X)) --> [num], identifier_s(X), {update_s(X, num, Env_s, FinalEnv_s)}.
declaration_s(Env_s, FinalEnv_s, t_declaration_num_assign_ternary(X, Y)) --> [num], identifier_s(X), [=], ternary_op_s(Y), {update_s(X, num, Env_s, FinalEnv_s)}.
declaration_s(Env_s, FinalEnv_s, t_declaration_stack_assign(X, Y)) --> [stack], identifier_s(X), [=], [Y], {is_list(Y)}, {update_s(X, stack, Env_s, FinalEnv_s)}.
declaration_s(Env_s, FinalEnv_s, t_declaration_stack_assign(X)) --> [stack], identifier_s(X), {update_s(X, stack, Env_s, FinalEnv_s)}.
declaration_s(Env_s, FinalEnv_s, t_declaration_queue_assign(X, Y)) --> [queue], identifier_s(X), [=], [Y], {is_list(Y)}, {update_s(X, queue, Env_s, FinalEnv_s)}.
declaration_s(Env_s, FinalEnv_s, t_declaration_queue_assign(X)) --> [queue], identifier_s(X), {update_s(X, queue, Env_s, FinalEnv_s)}.
declaration_s(Env_s, FinalEnv_s, t_declaration_list_assign(X, Y)) --> [list], identifier_s(X), [=], [Y], {is_list(Y)}, {update_s(X, list, Env_s, FinalEnv_s)}.

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Assignment statements
assignment_s(Env_s, Env_s, t_assignment_num_assign(X, Y)) --> identifier_s(X), [=], expr_s(Y), {lookup_s(X, num, Env_s)}.
assignment_s(Env_s, Env_s, t_assignment_num_assign_ternary(X, Y)) --> identifier_s(X), [=], ternary_op_s(Y) , {lookup_s(X, num, Env_s)}.
assignment_s(Env_s, Env_s, t_assignment_bool(X, Y)) --> identifier_s(X), [=], bool_s(Y), {lookup_s(X, bool_s, Env_s)}.
assignment_s(Env_s, Env_s, t_assignment_str(X, Y)) --> identifier_s(X), [=], [Y], {string(Y)}, {lookup_s(X, str, Env_s)}.
assignment_s(Env_s, Env_s, t_assignment_str_concat(X, Y)) --> identifier_s(X), [=], string_add(Y), {lookup_s(X, str, Env_s)}.
assignment_s(Env_s, Env_s, t_assignment_stack(X, Y)) --> identifier_s(X), [=], [Y], {is_list(Y)}, {lookup_s(X, stack, Env_s)}.
assignment_s(Env_s, Env_s, t_assignment_queue(X, Y)) --> identifier_s(X), [=], [Y], {is_list(Y)}, {lookup_s(X, queue, Env_s)}.
assignment_s(Env_s, Env_s, t_assignment_list(X, Y)) --> identifier_s(X), [=], [Y], {is_list(Y)}, {lookup_s(X, list, Env_s)}.

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Print statements
print_lookup(X, Env_s, true):- lookup_s(X, str, Env_s); lookup_s(X, bool_s, Env_s); lookup_s(X, unknown, Env_s); lookup_s(X, stack, Env_s) ;lookup_s(X, queue, Env_s).
print_statement_list(_Env, t_print()) --> [].
print_statement_list(Env_s, X) --> [,], print_statement_s(Env_s, X).
print_statement_s(Env_s, t_print(X, Y)) --> [X], {string(X)}, print_statement_list(Env_s, Y).
print_statement_s(Env_s, t_print_id(X, Y)) --> identifier_s(X), {print_lookup(X, Env_s, true)}, print_statement_list(Env_s, Y).
print_statement_s(Env_s, t_print_expr(X, Y)) --> expr_s(X), {\+print_lookup(X, Env_s, true)}, print_statement_list(Env_s, Y).
print_statement_s(_Env, t_print_stack_element(X)) --> stack_pt_s(X).
print_statement_s(_Env, t_print_queue_element(X)) --> queue_pt_s(X).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% if else statements
if_stmt(Env_s, t_ifstmt(X, Y, Z)) --> [if], [('], bool_s(X), [')'], ['{'], command_s(Env_s, _, Y), ['}'], elif_stmt_s(Env_s, Z).

elif_stmt_s(Env_s, t_elifstmt(X, Y, Z)) --> [elif], ['('], bool_s(X), [')'], ['{'], command_s(Env_s, _, Y), ['}'], elif_stmt_s(Env_s, Z).
elif_stmt_s(Env_s, t_goto_else_stmt(X)) --> else_stmt_s(Env_s, X).

else_stmt_s(Env_s, t_elsestmt(X)) --> [else], ['{'], command_s(Env_s, _, X), ['}'].
else_stmt_s(_, t_elsestmt()) --> [].

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% for loops
conventional_for_s(Env_s, t_conventional_for(A,B,C,D,E,F)) --> [for], ['('], identifier_s(A), [=], expr_s(B), [;],
    identifier_s(A), comparison_operator_s(C), expr_s(D), [;],
    identifier_s(A), [=], expr_s(E), [')'], {update_s(A, num, Env_s, FinalEnv_s)}, ['{'], command_s(FinalEnv_s, _, F), ['}'].

new_for_s(Env_s, t_new_for(A,B,C,D)) --> [for], identifier_s(A), [in],
    [range], ['('], expr_s(B), [,], expr_s(C), [')'], {update_s(A, num, Env_s, FinalEnv_s)}, ['{'], command_s(FinalEnv_s, _, D), ['}'].

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% stack operations
stack_op_s(_Env, t_stack_pt(X)) --> stack_pt_s(X).
stack_op_s(Env_s, t_stack_push(X, Y)) --> identifier_s(X), [.] , [push], ['('], expr_s(Y) , [')'], {lookup_s(X, stack, Env_s)}.
stack_pt_s(t_stack_pop(X)) --> identifier_s(X), [.], [pop], ['('], [')'].
stack_pt_s(t_stack_top(X)) --> identifier_s(X), [.], [top], ['('],[')'].

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% queue operations
queue_op_s(_Env, t_queue_pt(X)) --> queue_pt_s(X).
queue_op_s(Env_s, t_queue_push(X, Y)) --> identifier_s(X), [.] , [push], [('], expr_s(Y) , [')'], {lookup_s(X, queue, Env_s)}.
queue_pt_s(t_queue_poll(X)) --> identifier_s(X), [.], [poll], ['('], [')'].
queue_pt_s(t_queue_head(X)) --> identifier_s(X), [.], [head], ['('],[')'].

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% list operations
list_op_s(Env_s, t_add(X, Y)) --> identifier_s(X), [.] , [add], ['('], expr_s(Y) , [')'], {lookup_s(X, list, Env_s)}.
list_op_s(Env_s, t_add(X, Y, Z)) --> identifier_s(X), [.] , [add], ['('], expr_s(Y), [','], expr_s(Z), [')'], {lookup_s(X, list, Env_s)}.
list_op_s(Env_s, t_remove(X, Y)) --> identifier_s(X), [.], [remove], ['('], expr_s(Y), [')'], {lookup_s(X, list, Env_s)}.
list_op_s(Env_s, t_get(X, Y)) --> identifier_s(X), [.], [get], ['('], expr_s(Y) , [')'], {lookup_s(X, list, Env_s)}.

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Method Declaration
formal_parameter_list_s(Env_s, FinalEnv_s, X) --> [,], get_formal_parameters_s(Env_s, FinalEnv_s, X).
formal_parameter_list_s(Env_s, Env_s, t_formal_parameter()) --> [].
get_formal_parameters_s(Env_s, FinalEnv_s, t_formal_parameter(X, Y)) --> identifier_s(X), {update_s(X, unknown, Env_s, Env1)}, formal_parameter_list_s(Env1, FinalEnv_s, Y).
get_formal_parameters_s(Env_s, Env_s, t_formal_parameter()) --> [].

get_body_s(Env_s, t_body(X)) --> command_s(Env_s, _FinalEnv_s, X).

method_dec_s(Env_s, FinalEnv_s, t_method_declaration(X, Y, Z)) --> [def], identifier_s(X),
    ['('],get_formal_parameters_s([], Env1, Y),[')'],
    ['{'],get_body_s(Env1, Z),['}'],
    {update_s(X, method_s, Env_s, FinalEnv_s)}.


% Method Call
actual_parameter_list_s(Env_s, X) --> [,], get_actual_parameters_s(Env_s, X).
actual_parameter_list_s(_Env, t_actual_parameter()) --> [].

get_actual_parameters_s(Env_s, t_actual_parameter(X, Y)) --> identifier_s(X), {lookup_s(X, _, Env_s)}, actual_parameter_list_s(Env_s, Y).
get_actual_parameters_s(Env_s, t_actual_parameter(t_str(X), Y)) --> [X], {string(X)}, actual_parameter_list_s(Env_s, Y).
get_actual_parameters_s(Env_s, t_actual_parameter(t_num(X), Y)) --> [X], {number(X)}, actual_parameter_list_s(Env_s, Y).
get_actual_parameters_s(_Env, t_actual_parameter()) --> [].

method_call(Env_s, t_method_call(X, Y)) --> identifier_s(X), [('], get_actual_parameters_s(Env_s, Y), [')'], {lookup_s(X, method_s, Env_s)}.

% Methods
method_s(Env_s, FinalEnv_s, X) --> method_dec_s(Env_s, FinalEnv_s, X).
method_s(Env_s, Env_s, X) --> method_call(Env_s, X).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Statements
statement_s(Env_s, FinalEnv_s, t_statement_method(X)) --> method_s(Env_s, FinalEnv_s, X).
statement_s(Env_s, FinalEnv_s, t_statement_declaration(X)) --> declaration_s(Env_s, FinalEnv_s, X).
statement_s(Env_s, FinalEnv_s, t_statement_assign(X)) --> assignment_s(Env_s, FinalEnv_s, X).
statement_s(Env_s, Env_s, t_statement_print(X)) --> [print], ['('] , print_statement_s(Env_s, X), [')'].
statement_s(Env_s, Env_s, t_statement_ifelse(X)) --> if_stmt(Env_s, X).
statement_s(Env_s, Env_s, t_statement_while(X, Y)) --> [while], ['('], bool_s(X), [')'], ['{'], command_s(Env_s, _, Y), ['}'].
statement_s(Env_s, Env_s, t_statement_for(X)) --> conventional_for_s(Env_s, X).
statement_s(Env_s, Env_s, t_statement_for(X)) --> new_for_s(Env_s, X).
statement_s(Env_s, Env_s, t_statement_stack(X)) --> stack_op_s(Env_s, X).
statement_s(Env_s, Env_s, t_statement_queue(X)) --> queue_op_s(Env_s, X).
statement_s(Env_s, Env_s, t_statement_list(X)) --> list_op_s(Env_s, X).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Command List and single command_s is called statement_s.
command_s(Env_s, FinalEnv_s, t_command(X, Y)) --> statement_s(Env_s, Env1, X), command_s(Env1, FinalEnv_s, Y).
command_s(Env_s, Env_s, t_command()) --> [].

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Block.
block(t_block(X))-->command_s([], _, X).

% Entry point for the program. will create a parse tree from a supplied token list.
program(t_program(X))-->block(X).