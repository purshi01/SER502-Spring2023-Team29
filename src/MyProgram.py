#!/usr/bin/env python3
from pyswip import Prolog
from Compiler.lexer import lexer
import sys
import os


def generate_parse_tree(lex):
    '''
    This method will produce a parse tree using the lexer's tokens.
    Specify the following arguments: lex: type: str desc: list of tokens
    Return: type: str desc: Parse tree created after token parsing
    '''
    os.chdir("..")
    os.chdir("src")
    prolog.consult("Compiler/parse_tree.pl")
    prolog_queryy = "program(K,{},[])."
    parse__tree = prolog.query(prolog_queryy.format(lex))
    x = next(parse__tree)["K"]
    y = x.replace("b'", "'")
    return y


def semanticsForParseTree(parse_tree):
    '''
   This method will run code after receiving a parsed tree as input.

    Arguments:
        parse_tree: type: str desc: the output of parsing the code, or the input parse tree.
    No Return Type

    '''

    prolog.consult("Runtime/runtime.pl")
    prolog_queryy = "program_eval({},Z)"
    Env = prolog.query(prolog_queryy.format(parse_tree))
    environment = next(Env)["Z"]
    # print(environment)

if __name__ == "__main__":

    # If you want to run mass testing, comment out these lines.
    prolog = Prolog()
    os.chdir("..")
    os.chdir("data")
    codeFile_input_file = input('Please enter file name from data dictionary .ipt:- ')
    file = os.path.join(os.getcwd(),codeFile_input_file).replace('\\', '/')
    lex = lexer(file)
    file_remove = file + "_iptk"
    try:
        parse_tree = generate_parse_tree(lex)
    except Exception:
        print("$yntax Error")
        os.remove(file_remove) if os.path.exists(file_remove) else None
        sys.exit(0)
    try:
        semanticsForParseTree(parse_tree)
    except Exception:
        print("$emantics Error")
        os.remove(file_remove) if os.path.exists(file_remove) else None
        sys.exit(0)
    os.remove(file_remove) if os.path.exists(file_remove) else None
