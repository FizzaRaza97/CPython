import ply.yacc as yacc
import ply.lex as lex
import parser 
start = 'lang' # the start symbol in our grammar
precedence = (
        ('left', 'EQUALEQUAL'),
        ('nonassoc', 'LT', 'LE', 'GT', 'GE'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIVIDE'),
        ('right', 'PLUSPLUS', 'MINUSMINUS'),
        ('right', 'NOT'),
)

tokens = (
        'ANDAND',       # &&
        'COMMA',        # ,
        'DIVIDE',       # /
        'ELSE',         # else
        'EQUAL',        # =
        'EQUALEQUAL',   # ==
        'NOTEQUAL',     #!=        
        'FALSE',        # false
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
        'COLON',
        'RBRACE',       # }
        'RETURN',       # return
        'RPAREN',       # )
        'SEMICOLON',    # ;
        'STRING',       #### Not used in this problem.
        'TIMES',        # *
        'TRUE',         # true
        'NUMBERINT',
        'NUMBERDOUBLE',
        'BOOLELEMENT',
        'CHARELEMENT',
        'STRINGELEMENT',
        'IDENTIFIER',
        'INT',          # int
        'CHAR',         # char
        'DOUBLE',       # double
        'BOOL',         # bool
        'COUT',
        'ENDL',
        'DOT'
)

#lang -> element lang | e
def p_lang(p): 
    'lang : element lang'
    p[0] = [p[1]] + p[2]

def p_lang_empty(p):
    'lang : '
    p[0] = [ ]

#element -> INT MAIN () compoundstmt | type IDENTIFIER (optargs) compoundstmt | CLASS IDENTIFIER LBRACE attributes RBRACE SEMICOLON | e
def p_element_main(p):
    'element : INT MAIN LPAREN RPAREN compoundstmt'
    p[0] = ('maincall', p[5])

def p_element_function(p):
    'element : type IDENTIFIER LPAREN optargstype RPAREN compoundstmt'
    p[0] = ('function', p[1], p[2],p[4],p[6])

def p_element_class(p):
    'element : CLASS IDENTIFIER LBRACE attributes RBRACE SEMICOLON'
    p[0] = ('classdefinition',p[2],p[4])

def p_classkeywordspublic(p):
    'attributes : PUBLIC COLON attributes'
    p[0] = p[3]

def p_classkeywordsprivate(p):
    'attributes : PRIVATE COLON attributes'
    p[0] = p[3]

def p_classattributes(p):
    'attributes : type IDENTIFIER SEMICOLON attributes'
    p[0] = [('classattribute',p[1],p[2])] + p[4]

def p_classfunction(p):
    'attributes : type IDENTIFIER LPAREN optargstype RPAREN compoundstmt attributes'
    p[0] = [('classfunction', p[1], p[2],p[4],p[6])] + p[7]

def p_classattributesempty(p):
    'attributes : '
    p[0] = []

# def p_element_functioncalltype(p):
#     'functioncalltype : type IDENTIFIER LPAREN optargstype RPAREN'
#     p[0] = ('function-declaration', p[1],p[2],p[4])

def p_optargstype(p):
    'optargstype : argstype'
    p[0] = p[1]
def p_optargstypeempty(p):
    'optargstype : '
    p[0] = []

def p_argstype(p):
    'argstype : type IDENTIFIER COMMA argstype'
    p[0] = [(p[1],p[2])] + p[4]

def p_argstype_last(p):
    'argstype : type IDENTIFIER'
    p[0] = [(p[1],p[2])]

def p_element_functioncall(p):
    'functioncall : IDENTIFIER LPAREN optargs RPAREN'
    p[0] = ('functioncall',p[1],p[3])

def p_element_empty(p):
    'element : '
    p[0] = []

#optargs -> args | e
def p_optargs(p):
    'optargs : args'
    p[0] = p[1]

def p_optargs_empty(p):
    'optargs : '
    p[0] = []

#args -> IDENTIFIER COMMA args | IDENTIFIER
def p_args(p):
    'args : exp COMMA args'
    p[0] = [ p[1] ] + p[3]

def p_args_last(p):
    'args : exp'
    p[0] = [ p[1] ]

#compoundstmt -> {stmts}
def p_compoundstmt(p):
    'compoundstmt : LBRACE statements RBRACE'
    p[0] = p[2]

#stmts -> stmt ; stmts | e
def p_statements(p):
    'statements : stmt statements'
    p[0] = [ p[1] ] + p[2]

def p_statements_empty(p):
    'statements : '
    p[0] = []

#type -> INT | CHAR | BOOL | DOUBLE | STRING
def p_type_int(p):
    'type : INT'
    p[0] = ('type',p[1])

def p_type_char(p):
    'type : CHAR'
    p[0] = ('type',p[1])

def p_type_bool(p):
    'type : BOOL'
    p[0] = ('type',p[1])

def p_type_double(p):
    'type : DOUBLE'
    p[0] = ('type',p[1])

def p_type_string(p):
    'type : STRING'
    p[0] = ('type',p[1])

#stmt -> SimpleVarDec | ArrayDeclaration | SimpleVarAssign | ArrayIndexAssignment 
#   | ForStmt | IfStmt | IfElseStmt | IfElseIfStmt | ReturnStmt | Exp | PrintStmt | ClassObjectDec
# | ObjectAttributeAssign



def p_stmt_ifthen(p):
    'stmt : IF LPAREN exp RPAREN compoundstmt'
    p[0] = ('if-then', p[3], p[5])

def p_stmt_if_then_else(p):
    'stmt : IF LPAREN exp RPAREN compoundstmt ELSE compoundstmt'
    p[0] = ('if-then-else', p[3], p[5], p[7])

def p_stmt_if_else_if(p):
    'stmt : IF LPAREN exp RPAREN compoundstmt elseifstmt'
    p[0] = ('if-else-if',p[3],p[5],p[6])

def p_else_if_stmt(p):
    'elseifstmt : ELSE IF LPAREN exp RPAREN compoundstmt elseifstmt'
    p[0] = ('else-if',p[4],p[6],p[7])

def p_else_if_else_stmt(p):
    'elseifstmt : ELSE compoundstmt'
    p[0] = ('else', p[2])

def p_else_if_empty_stmt(p):
    'elseifstmt : '
    p[0] = []

def p_stmt_exp(p):
    'stmt : exp SEMICOLON'
    p[0] = p[1]

def p_stmt_simplevar_dec(p):
    'stmt : simplevardec SEMICOLON'
    p[0] = p[1]

def p_stmt_multiplevariables(p):
    'stmt : multiplevars SEMICOLON'
    p[0] = p[1]

def p_stmt_array_dec(p):
    'stmt : arraydeclaration SEMICOLON'
    p[0] = p[1]

def p_stmt_array_init(p):
    'stmt : arrayinitialisation SEMICOLON'
    p[0] = p[1]

def p_stmt_array_init_length(p):
    'stmt : arrayinitialisationwithlength SEMICOLON'
    p[0] = p[1]

def p_multiplevars(p):
    'multiplevars : type IDENTIFIER anothervar'
    p[0] = ('MultipleVariables',p[1],[p[2]]+p[3])

def p_anothervar(p):
    'anothervar : COMMA IDENTIFIER'
    p[0] = [p[2]]
def p_anothervar_last(p):
    'anothervar : IDENTIFIER'
    p[0] = [p[1]]

def p_array_init_length(p):
    'arrayinitialisationwithlength : type IDENTIFIER LSQUARE NUMBERINT RSQUARE EQUAL LBRACE values RBRACE'
    p[0] = ('ArrayInitialisationWithLength',p[1],p[2],p[4],p[8])

def p_array_init(p):
    'arrayinitialisation : type IDENTIFIER LSQUARE RSQUARE EQUAL LBRACE values RBRACE'
    p[0] = ('ArrayInitialisation',p[1],p[2],p[7])

def p_values(p):
    'values : constant COMMA values'
    p[0] = [p[1]] + p[3]

def  p_values_last(p):
    'values : constant'
    p[0] = [p[1]]

#SimpleVarAssign -> IDENTIFIER EQUAL exp
def p_simplevarassign(p):
    'stmt : IDENTIFIER EQUAL exp SEMICOLON'
    p[0] = ('SimpleVarAssign',p[1],p[3])

#ArrayIndexing -> IDENTIFIER LSQUARE NUMBERINT RSQUARE EQUAL exp
def p_arrayindex(p):
    'stmt : IDENTIFIER LSQUARE exp RSQUARE EQUAL exp SEMICOLON'
    p[0] = ('ArrayIndexAssignment',p[1], p[3], p[6])

def p_forstmt(p):
    'stmt : FOR LPAREN expF SEMICOLON exp SEMICOLON exp RPAREN compoundstmt'
    p[0] = ('For Loop',p[3],p[5],p[7], p[9])

def p_stmtreturn(p):
    'stmt : RETURN exp SEMICOLON'
    p[0] = ('Return',p[2])

def p_printnl(p):
    'stmt : COUT LT LT IDENTIFIER LT LT ENDL SEMICOLON'
    p[0] = ('printn',('variable',p[4]))

def p_printnlconstant(p):
    'stmt : COUT LT LT constant LT LT ENDL SEMICOLON'
    p[0] = ('printn',p[4])

def p_printnlarray(p):
    'stmt : COUT LT LT IDENTIFIER LSQUARE exp RSQUARE LT LT ENDL SEMICOLON'
    p[0] = ('printn',('ArrayElement',p[4],p[6]))


def p_print(p):
    'stmt : COUT LT LT IDENTIFIER SEMICOLON'
    p[0] = ('print',('variable',p[4]))

def p_printconstant(p):
    'stmt : COUT LT LT constant SEMICOLON'
    p[0] = ('print',p[4])

def p_printarray(p):
    'stmt : COUT LT LT IDENTIFIER LSQUARE exp RSQUARE SEMICOLON'
    p[0] = ('print',('ArrayElement',p[4],p[6]))

def p_stmt_objectcreate(p):
    'stmt : createobject SEMICOLON'
    p[0] = p[1]

def p_attributeassign(p):
    'stmt : IDENTIFIER DOT IDENTIFIER EQUAL exp SEMICOLON'
    p[0] = ("ObjectAttributeAssign",p[1],p[3],p[5])

def p_objectcreate(p):
    'createobject : IDENTIFIER IDENTIFIER'
    p[0] = ('classobject',p[1],p[2])


#expF -> exp | e
def p_for_exp(p):
    'expF : type IDENTIFIER EQUAL exp'
    p[0] = ('initialize-simple-variable', p[1], p[2], p[4])

def p_for_exp_i(p):
    'expF : IDENTIFIER EQUAL exp'
    p[0] = ('SimpleVarAssign',p[1],p[3])



def p_for_empty(p):
    'expF : exp'
    p[0] = p[1]


#SimpleVariableDeclaration -> DeclareOnly | DeclareandAssign
def p_simplevardec_deconly(p):
    'simplevardec : type IDENTIFIER'
    p[0] = ('declare-simple-variable',p[1],p[2])

def p_simplevardec_initialize(p):
    'simplevardec : type IDENTIFIER EQUAL exp morevars'
    p[0] = ('initialize-simple-variable', p[1], p[2], p[4])

# def p_morevars(p):
#     'morevars : COMMA simplevardec'
#     p[0] = p[2]

def p_morevars_empty(p):
    'morevars : '
    p[0] = []

#ArrayDeclaration -> type IDENTIFIER LSQUARE NUMBERINT RSQUARE 
def p_array_declaration(p):
    'arraydeclaration : type IDENTIFIER LSQUARE NUMBERINT RSQUARE'
    p[0] = ('arraydeclaration',p[1],p[2],p[4])




#Constant -> NUMBERINT | NUMBERDOUBLE | CHARELEMENT | STRINGELEMENT
def p_constant_int(p):
    'constant : NUMBERINT'
    p[0] = ('int',p[1])

def p_constant_double(p):
    'constant : STRINGELEMENT'
    p[0] = ('string',p[1])

def p_constant_char(p):
    'constant : NUMBERDOUBLE'
    p[0] = ('double',p[1])

def p_constant_string(p):
    'constant : CHARELEMENT'
    p[0] = ('char',p[1])

def p_constant_bool(p):
    'constant : BOOLELEMENT'
    p[0] = ('bool',p[1])

#Exp -> Constant | ArrayElement | FunctionCall | (Exp) | Exp Boperator Exp 
#   | Uoperator Exp | Exp Uoperator | IDENTIFIER |objectAttribute | objectFunctionCall
def p_exp_constant(p):
    'exp : constant'
    p[0] = p[1]

def p_exp_identifier(p):
    'exp : IDENTIFIER'
    p[0] = ('variable',p[1])

def p_exp_arrayelement(p):
    'exp : IDENTIFIER LSQUARE exp RSQUARE'
    p[0] = ('ArrayElement',p[1],p[3])

def p_exp_bopr(p):
    'exp : exp bop exp'
    p[0] = ('BinaryOperation',p[1],p[2],p[3])


def p_exp_functioncall(p):
    'exp : functioncall'
    p[0] = p[1]

def p_exp_brackex(p):
    'exp : LPAREN exp RPAREN'
    p[0] = ('Paranthesis',p[2])

def p_exp_objatt(p):
    'exp : IDENTIFIER DOT IDENTIFIER'
    p[0] = ("ObjectAttribute",p[1],p[3])

def p_exp_objfunc(p):
    'exp : IDENTIFIER DOT IDENTIFIER LPAREN optargs RPAREN'
    p[0] = ("objectFunctionCall",p[1],p[3],p[5])

#Uopr ->
def p_uoprpp(p):
    'uop : PLUSPLUS'
    p[0] = ('plusplus',p[1])

def p_uoprmm(p):
    'uop : MINUSMINUS'
    p[0] = ('minusminus',p[1])


def p_exp_uoprl(p):
    'exp : uop exp'
    p[0] = ('UnaryOperationL',p[1],p[2])

def p_exp_uoprr(p):
    'exp : exp uop'
    p[0] = ('UnaryOperationR',p[1],p[2])

def p_exp_negativenum(p):
    'exp : MINUS exp'
    p[0] = ('negative',p[2])


# def p_exp_empty(p):
#     'exp : '
#     p[0] = []


#Bop -> 
def p_bopplus(p):
    'bop : PLUS'
    p[0] = ('plus',p[1])

def p_bopminus(p): #NEEDS FIXING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    'bop : MINUS'
    p[0] = ('minus',p[1])

def p_bopmult(p):
    'bop : TIMES'
    p[0] = ('mult',p[1])

def p_bopdiv(p):
    'bop : DIVIDE'
    p[0] = ('divide',p[1])

def p_bopmod(p):
    'bop : MOD'
    p[0] = ('mod',p[1])

def p_bopgt(p):
    'bop : GT'
    p[0] = ('gt',p[1])

def p_bopge(p):
    'bop : GE'
    p[0] = ('ge',p[1])

def p_boplt(p):
    'bop : LT'
    p[0] = ('lt',p[1])

def p_bople(p):
    'bop : LE'
    p[0] = ('le',p[1])

def p_bopne(p):
    'bop : NOTEQUAL'
    p[0] = ('ne',p[1])

def p_bopee(p):
    'bop : EQUALEQUAL'
    p[0] = ('eqeq',p[1])



def p_error(p):
    if not p:
        print ("End of input")
    else:
        print ("Syntax error on line " + str(p.lineno) + " on token " + str(p.value))



jslexer = lex.lex(module=parser)
jsparser = yacc.yacc()

input_string = """

class Shape {
    public:
    int left;
    int right;
    int sum() {
        return a + b;
    }
};

int main()
{
    x.left = -7;
    int a = x.left;
    
}
"""

jslexer.input(input_string)
parse_tree = jsparser.parse(input_string,lexer=jslexer)

