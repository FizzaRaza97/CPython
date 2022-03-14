import ply.lex as lex

tokens = (
        'ANDAND',       # &&
        'COMMA',        # ,
        'DIVIDE',       # /
        'ELSE',         # else
        'EQUAL',        # =
        'EQUALEQUAL',   # ==
        'NOTEQUAL',     #!=        
        'GE',           # >=
        'GT',           # >
        'IF',           # if
        'LBRACE',       # {
        'LE',           # <=
        'LPAREN',       # (
        'LT',           # <
        'MINUS',        # -
        'NOT',          # !
        'OROR',         # ||
        'PLUS',         # +
        'MOD',          # %
        'FOR',          # for
        'PLUSPLUS',     # ++
        'MINUSMINUS',   # --
        'LSQUARE',      # [
        'RSQUARE',      # ]
        'MAIN',         # main
        'CLASS',
        'PUBLIC',
        'PRIVATE',
        'RBRACE',       # }
        'RETURN',       # return
        'RPAREN',       # )
        'SEMICOLON',    # ;
        'COLON',
        'STRING',       #### Not used in this problem.
        'TIMES',        # *
        'NUMBERINT',
        'NUMBERDOUBLE',
        'CHARELEMENT',
        'STRINGELEMENT',
        'BOOLELEMENT',
        'IDENTIFIER',
        'INT',          # int
        'CHAR',         # char
        'DOUBLE',       # double
        'BOOL',         # bool
        'COUT',
        'ENDL',
        'DOT'
)


states = ( ( 'eolcomment', 'exclusive'),  ('delimitedcomment', 'exclusive'), )

def t_eolcomment(t):
    r'//'
    t.lexer.begin('javascripteolcomment')

def t_eolcomment_end(t):
    r'\n'
    t.lexer.begin('INITIAL')


def t_delimitedcomment(t):
  r'/\*'
  t.lexer.begin('javascriptdelimitedcomment')

def t_delimitedcomment_end(t):
  r'\*/'
  t.lexer.begin('INITIAL')

t_delimitedcomment_ignore = r'.'
t_eolcomment_ignore  = r'.'

def t_eolcomment_error(t):
  t.lexer.skip(1)

def t_delimitedcomment_error(t):
  t.lexer.skip(1)

t_ANDAND =  r'&&'
t_COMMA =  r','
t_DIVIDE =  r'/'
t_EQUALEQUAL = r'=='
t_NOTEQUAL = r'!='
t_MOD = r'\%'
t_PLUSPLUS = r'\+\+'
t_MINUSMINUS = r'\-\-'
t_LSQUARE = r'\['
t_RSQUARE = r'\]'
t_EQUAL = r'='
t_GE =  r'>='
t_GT =   r'>'
t_LBRACE = r'\{'
t_LE =  r'<='
t_LPAREN =   r'\('
t_LT = r'<'


t_NOT = r'!'
t_OROR = r'\|\|'
t_PLUS = r'\+'
t_RBRACE =  r'\}'
t_RPAREN =  r'\)'
t_SEMICOLON =   r';'
t_COLON = r':'
t_TIMES = r'\*'
t_DOT = r'\.'

def t_MINUS(token):
    r'-'
    return token

def t_MAIN(token):
    r'main'
    return token

def t_IF(token):
    r'if'
    return token

def t_ELSE(token):
    r'else'
    return token

def t_FOR(token):
    r'for'
    return token

def t_BOOLELEMENT(token):
    r'true | false | False | True'
    return token

def t_INT(token):
    r'int'
    return token

def t_DOUBLE(token):
    r'double'
    return token

def t_CHAR(token):
    r'char'
    return token

def t_BOOL(token):
    r'bool'
    return token

def t_STRING(token):
    r'string'
    return token

def t_RETURN(token):
    r'return'
    return token

def t_COUT(token):
    r'cout'
    return token

def t_ENDL(token):
    r'endl'
    return token

def t_CLASS(token):
    r'class'
    return token

def t_PUBLIC(token):
    r'public'
    return token

def t_PRIVATE(token):
    r'private'
    return token

def t_STRINGELEMENT(token):
    r'\"[^"]*\"'
    token.value = token.value[1:-1]
    return token

def t_CHARELEMENT(token):
    r'\'[^\']\''
    token.value = token.value[1]
    return token

def t_IDENTIFIER(token):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    return token


def t_NUMBERDOUBLE(token):
    r'-?[0-9]*(?:\.[0-9]+)'
    token.value = float(token.value)
    return token

def t_NUMBERINT(token):
    r'-?[0-9]+'
    token.value = int(token.value)
    return token





# def t_NUMBERINT(token):
#     r'[0-9]+'
#     token.value = int(token.value)
#     return token

t_ignore = ' \t\v\r' # whitespace

def t_newline(t):
        r'\n'
        t.lexer.lineno += 1

def t_error(t):
        print ("Lexer: Illegal character " + t.value[0])
        t.lexer.skip(1)

# #testing

# jinput = """ 
# class shape {
#     public:
#     int a;
#     int b;
#     private:
#     int c;
# };
# x.c;
# x>c
# double -0.32
# """
# lexer = lex.lex()
# lexer.input(jinput)
# while True:
#         tok = lexer.token()
#         if not tok: break
#         print tok
