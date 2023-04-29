#!/usr/bin/env python3
from functools import reduce
from tokenize import tokenize, untokenize, NUMBER, STRING, NAME, OP
from io import BytesIO


def lexer(given_file):
    '''
    This proce_ss will produce tokens and

    Arguments:
        file: type: str desc: Enter the name of the file
    The following will be returned: type: str desc: token-generated list format
    '''

    input_file = open(given_file, 'r')
    input_comment_removed = open(given_file+"_iptk", 'w')
    result = []
    tokens = "["
    for line in input_file:
        if not line.startswith('#'):
            input_comment_removed.write(line)
    input_comment_removed.close()
    proce_ss = open(given_file+"_iptk", 'r').read()
    tokenize__str = tokenize(BytesIO(proce_ss.encode('utf-8')).readline)
    identify__list = []
    for tok_num, tok_val, , _, _ in tokenize_str:
        if(len(tok_val) != 0):
            ascii = reduce(lambda x, y: str(x)+str(y), map(ord, tok_val))
            if ascii != 10 and tok_val != "utf-8" and ascii != "32323232" and ascii != 9:
                if tok_val == '[':
                    identify__list.append(tok_val)
                    continue
                elif tok_val == ']':
                    identify__list.append(tok_val)
                    tokens += ("".join(identify__list))
                    identify__list = []
                else:
                    if tok_val == '!=':
                        tokens += '"'+tok_val+'"'
                    elif tok_val == ')' or tok_val == '(' or tok_val == '{' or tok_val == '}':
                        tokens += "'"+tok_val+"'"
                    else:
                        tokens += tok_val
                tokens += ","
    tokens += ']'
    # print(tokens)
    return tokens
