%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Command
eval_command_s(t_command(), Env_s, Env_s).
eval_command_s(t_command(X_s, Y_s), Env_s, FinalEnv_s) :- eval_statement(X_s, Env_s, Env1_s), eval_command_s(Y_s, Env1_s, FinalEnv_s).

%----------------------------------------------------------------------------------------------------------------------------------------------------------

% Evaluate Block
eval_block(t_block(X_s), Env_s, FinalEnv_s):- eval_command_s(X_s, Env_s, FinalEnv_s).

program_eval(t_program(X_s), Env_s):- eval_block(X_s, [], Env_s).