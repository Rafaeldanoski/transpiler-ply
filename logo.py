################################# Imports ###############################################
import ply.yacc as yacc
import ply.lex as lex
from collections.abc import Iterable
import warnings
warnings.filterwarnings("ignore")

program = []
################################ Reserved keywords ######################################
reserved = {
    'true': 'TRUE',
    'false': 'FALSE',
    'write': 'WRITE',
    'to': 'TO',
    'end': 'END',
    'fo': 'FORWARD',
    'forward': 'FORWARD',
    'bk': 'BACKWARD',
    'back': 'BACKWARD',
    'rt': 'RIGHT',
    'right': 'RIGHT',
    'lt': 'LEFT',
    'left': 'LEFT',
    'pu': 'PENUP',
    'penup': 'PENUP',
    'pd': 'PENDOWN',
    'pendown': 'PENDOWN',
    'heading': 'HEADING',
    'setxy': 'SETXY',
    'home': 'HOME',
    'wipeclean': 'WIPECLEAN',
    'wc': 'WIPECLEAN',
    'cs': 'RESET',
    'clearscreen': 'RESET',
    'random': 'RANDOM',
    'xcor': 'XCOR',
    'ycor': 'YCOR',
    'typein': 'TYPEIN',
    'sqrt': 'SQRT',
    'and': 'AND',
    'or': 'OR',
    'not': 'NOT'
}

############################################ Tokens #########################################
tokens = [
    'ID', 'FLOAT', 'INT', 'STRING',
    'LPAR', 'RPAR', 'LK', 'RK',
    'COLON', 'COMMA',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER', 'ASSIGN', 'EQ'
] + list(set(reserved.values()))


######################################### Tokens Chars #####################################

t_LPAR = r'\('
t_LK = r'{'
t_RK = r'}'
t_RPAR = r'\)'
t_COLON = r':'
t_COMMA = r','
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_POWER = r'\^'
t_EQ = r'=='
t_ASSIGN = r'='


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    return t


def t_STRING(t):
    r'\".*\"'
    t.type = reserved.get(t.value, 'STRING')
    return t


def t_FLOAT(t):
    r'\d*\.\d+'
    t.value = float(t.value)
    return t


def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t


t_ignore = " \t"


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print(f"Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


###################################### Precedence ############################################

precedence = (
    ('right', 'RANDOM'),
    ('left', 'PLUS', 'MINUS'),
    ('right', 'SQRT'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),
    ('left', 'POWER'),
)




def p_program(p):
    '''
    program : statement statement_list
    '''
    statement = p[1]
    statements = p[2]
    if statements is None:
        statements = [statement]
    else:
        statements.insert(0, statement)
    p[0] = statements
    vm_program = []
    for statement_body in statements:
        vm_program.extend(statement_body)
    global program
    program = list(flatten(vm_program))


def flatten(lis):
    for item in lis:
        if isinstance(item, Iterable) and not isinstance(item, str):
            for x in flatten(item):
                yield x
        else:
            yield item


def p_statement_list(p):
    '''
    statement_list : statement statement_list
                   | empty
    '''
    if len(p) == 2:
        p[0] = None
        return
    statement = p[1]
    statements = p[2]
    if statements is None:
        statements = [statement]
    else:
        statements.insert(0, statement)
    p[0] = statements


def p_statement(p):
    '''
    statement : turtle_instruction
              | variable_declaration
              | procedure_definition
              | procedure_call
              | write_statement
    '''
    p[0] = p[1]


def p_turtle_instruction(p):
    '''
    turtle_instruction : SETXY LK expression COMMA expression RK
                       | FORWARD expression
                       | BACKWARD expression
                       | RIGHT expression
                       | LEFT expression
                       | HEADING expression
                       | PENUP
                       | PENDOWN
                       | HOME
                       | WIPECLEAN
                       | RESET
                       | XCOR
                       | YCOR
                       | TYPEIN expression
    '''
    function = reserved[p[1]]
    if len(p) == 7:
        p[0] = [p[3], p[5], 'CALL ' + function]
    elif len(p) == 3:
        p[0] = [p[2], 'CALL ' + function]
    else:
        p[0] = ['CALL ' + function]
    program.extend(p[0])


def p_variable_declaration(p):
    '''
    variable_declaration : ID ASSIGN expression
    '''
    p[0] = [p[3], 'STORE ' + p[1]]
    program.extend(p[0])


def p_procedure_definition(p):
    '''
    procedure_definition : TO ID parameter_list statement_list END
    '''
    symbol_table = {}
    args = [a for a in p[3] if a is not None]
    p[0] = ['DEF ' + p[2], args, p[4]]
    symbol_table[p[2]] = {
        'args': [a for a in args if a is not None],
        'body': p[4]
    }
    program.extend(p[0])


def p_parameter_list(p):
    '''
    parameter_list : parameter_list COMMA parameter
                   | parameter
    '''
    if len(p) == 4:
        p[0] = p[1] + p[3]
    else:
        p[0] = p[1]


def p_parameter(p):
    '''
    parameter : name
              | empty
    '''
    p[0] = [p[1]]


def p_procedure_call(p):
    '''
    procedure_call : ID expression_list
    '''
    args = [a for a in p[2] if a is not None]
    p[0] = ['CALL ' + p[1], args]
    program.extend(p[0])


def p_expression_list(p):
    '''
    expression_list : expression_list COMMA expression
                    | expression
                    | empty
    '''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]


def p_write_statement(p):
    '''
    write_statement : WRITE word
                    | WRITE expression
                    | WRITE expression word
                    | WRITE word expression
    '''
    if len(p) == 3:
        p[0] = [p[2], 'CALL ' + reserved[p[1]]]
    else:
        p[0] = [p[2], p[3], 'CALL ' + reserved[p[1]]]
    program.extend(p[0])


def p_expression_int_float(p):
    '''
    expression : INT
               | FLOAT
    '''
    p[0] = [f'PUSH {p[1]}']
    program.extend(p[0])


def p_expression_binary(p):
    '''
    expression : expression TIMES expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
               | expression POWER expression
    '''
    operators = {
        '+': 'ADD',
        '-': 'SUB',
        '*': 'MUL',
        '/': 'DIV',
        '^': 'POW'
    }

    p[0] = [p[1], p[3], operators[p[2]]]
    program.extend(p[0])




def p_expression_uminus(p):
    '''
    expression : MINUS expression %prec UMINUS
    '''
    p[0] = [p[2], 'UMINUS']
    program.extend(p[0])


def p_expression_group(p):
    '''
    expression : LPAR expression RPAR
    '''
    p[0] = [p[2]]
    program.extend(p[0])



def p_expression_name(p):
    '''
    expression : name
    '''
    p[0] = p[1]


def p_word(p):
    '''
    word : STRING
    '''
    p[0] = 'PUSH ' + p[1]


def p_name(p):
    '''
    name : COLON ID
    '''
    p[0] = ['LOAD ' + p[2]]
    program.extend(p[0])


def p_empty(p):
    '''
    empty :
    '''
    p[0] = None


def p_error(p):
    if p:
        print("Syntax error at token", p.type)
        parser.errok()
    else:
        print("Syntax error at EOF")





lexer = lex.lex()
parser = yacc.yacc()


def main():
    try:
        logo_program = '''
        a = 8 
        b = 5 ^ (2 + 1)
        c = 4 * (:a + :b) 
        write :c
        '''
    except EOFError:
        print('EOF Error')

    ######################## Parser ###########################################
    p = parser.parse(logo_program)

    ######################## Result ############################################
    if p is None:
        pass
    else:
        for line in program:
            print(line)


if __name__ == '__main__':
    main()
