#Lucas Ebrenz Fernandes Pereira  turma das 9h(Herman)  2011488
import ply.lex as lex
import ply.yacc as yacc

# Definindo os tokens reservados
reservados = {
    'INICIO': 'INICIO',
    'MONITOR': 'MONITOR',
    'EXECUTE': 'EXECUTE',
    'TERMINO': 'TERMINO',
    'ENQUANTO': 'ENQUANTO',
    'FACA': 'FACA',
    'FIM': 'FIM',
    'IF': 'IF',
    'THEN': 'THEN',
    'ELSE': 'ELSE',
    'ZERO': 'ZERO',
    'EVAL': 'EVAL'
}

tokens = [
    'ID', 'NUMERO',
    'MAIS', 'MENOS', 'MULTIPLICACAO', 'DIVISAO',
    'IGUAL', 'VIRGULA', 'LPAREN', 'RPAREN'
] + list(reservados.values())

# Definindo as expressões regulares para tokens simples
t_MAIS = r'\+'
t_MENOS = r'-'
t_MULTIPLICACAO = r'\*'
t_DIVISAO = r'/'
t_IGUAL = r'='
t_VIRGULA = r','
t_LPAREN = r'\('
t_RPAREN = r'\)'

def t_NUMERO(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reservados.get(t.value, 'ID')  # Verifica se é uma palavra reservada
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    
t_ignore = ' \t\n' # ignora espaços e tabs

def t_error(t): # nos dizer qual caractere ilegal e se tem erro
    print("Caracter ilegal: ", t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

# Variáveis monitoradas
monitoradas = []

# Definindo as regras de produção
def p_program(p):
    'program : INICIO varlist MONITOR varlist EXECUTE cmds TERMINO'
    global monitoradas
    monitoradas = p[4].split(', ')
    p[0] = f"#include <stdio.h>\nint main(){{\nint{p[2]}; /*MONITOR: {p[4]}*/ \n {p[6]} \nreturn 0;\n}}"

def p_varlist(p):
    '''
    varlist : ID
            | ID VIRGULA varlist
    '''
    if len(p) == 2:
        p[0] = f" {p[1]}"
    else:
        p[0] = f" {p[1]}, {p[3]}"
        
        
def p_cmds(p):
    '''
    cmds : cmd cmds
         | cmd
    '''
    if len(p) == 3:
        p[0] = f"{p[1]} {p[2]}"
    else:
        p[0] = f"{p[1]}"

def p_cmd(p):
    '''
    cmd : ENQUANTO ID FACA cmds FIM
        | exp
        | IF ID THEN cmd
        | IF ID THEN cmd ELSE cmd
        | EVAL LPAREN ID VIRGULA ID VIRGULA cmds RPAREN
        | ZERO LPAREN ID RPAREN
    '''
    if p[1] == 'ENQUANTO': # ENQUANTO
        p[0] = f"\n    while({p[2]}){{ {p[4]} }}" #ENQUANTO P[2]!=0 , cmds
    elif p[1] == 'ZERO': #ZERO
        p[0] = f"\n{p[3]} = 0; {print_monitorada(p[3])}" #P[1] ZERO, P[3] o que será zerado
    elif len(p) == 2: # se o tamanho é 2, isto é, p[0] e p[1] é só um "cmd"
        p[0] = f"{p[1]}"
    elif p[1] == 'EVAL':# eval é como se fosse um for: 
        p[0] = f"\nfor(int {p[3]}, {p[5]}; {p[3]}!={p[5]}; {p[3]}--){{  {p[7]} \n}}"
    elif p[1] == 'IF':
        if len(p) == 5:  # IF without ELSE
            p[0] = f"\n  if({p[2]}){{ {p[4]}; \n}}"
        else:  # IF with ELSE
            p[0] = f"\n  if({p[2]}){{ {p[4]}; \n  }} else {{ {p[6]}; \n}}"
    else:
        p[0] = f"{p[1]} = {p[5]}  {p[7]} {p[9]}"
        
def p_exp(p):
    '''
    exp : ID IGUAL ID MAIS ID
        | ID IGUAL ID MENOS ID
        | ID IGUAL ID MULTIPLICACAO ID
        | ID IGUAL ID DIVISAO ID
        | ID IGUAL ID
        | ID IGUAL NUMERO MAIS ID
        | ID IGUAL NUMERO MENOS ID
        | ID IGUAL NUMERO MULTIPLICACAO ID
        | ID IGUAL NUMERO DIVISAO ID
        | ID IGUAL ID MAIS NUMERO
        | ID IGUAL ID MENOS NUMERO
        | ID IGUAL ID MULTIPLICACAO NUMERO
        | ID IGUAL ID DIVISAO NUMERO
        | ID IGUAL NUMERO MAIS NUMERO
        | ID IGUAL NUMERO MENOS NUMERO
        | ID IGUAL NUMERO MULTIPLICACAO NUMERO
        | ID IGUAL NUMERO DIVISAO NUMERO
        | ID IGUAL NUMERO
    '''
    if len(p) == 4:
        p[0] = f"{p[1]} {p[2]} {p[3]}; {print_monitorada(p[1])}"
    else:
        p[0] = f"\n{p[1]} {p[2]} {p[3]} {p[4]} {p[5]}; {print_monitorada(p[1])}"

def p_error(p): 
    print("Erro de sintaxe: ", p)

def print_monitorada(var):
    if var in monitoradas:
        return f'printf("%d\\n", {var}); '
    return ''

parser = yacc.yacc()

# Exemplo de análise sintática
#result = parser.parse("INICIO a, b, c, d MONITOR x EXECUTE a = b + c b = b + c TERMINO")
#result = parser.parse("INICIO X, Y MONITOR Z EXECUTE Y=2 X=5 Z = Y ENQUANTO X FACA Z = Z + 1 FIM TERMINO")
#result = parser.parse("INICIO X, Y, B MONITOR Z EXECUTE X = 5 Y = 2 B = 0 EVAL(X, Y, IF B THEN Z = Z + 2 ELSE Z = Z + 1) TERMINO")
#result = parser.parse("INICIO a, b, c, d MONITOR x EXECUTE a = b + c b = b + c ZERO(K) x = 3 TERMINO")
result = parser.parse("INICIO X, Y, B MONITOR Z EXECUTE X = 5 Y = 2 B = 0 EVAL(X, Y, Z = Z + 2 Z = Z + 1) TERMINO")
print(result)
