from enum import Enum
from typing import NamedTuple

#====ALL TOKENS====
class TokenType(Enum):
    #Data Types Tokens
    BOOL      = 'janee'           # bool
    INT       = 'getal'           # int
    FLOAT     = 'zweef'           # float
    # STRING    = 'woord'           # string

    #Constants Tokens
    TRUE    = 'waar'    # true
    FALSE   = 'onwaar'  # false

    #Arithmetic Operator Tokens
    ADDIDION        = 'plus'            # +
    SUBTRACTION     = 'min'             # -
    MULTIPLICATION  = 'keer'            # *
    DIVISION        = 'delendoor'       # /

    #Assignment Operator Tokens
    IS  = 'is'  # =

    #Comparison Operator Tokens
    EQUALTO                 = 'gelijkaan'           # ==
    NOTEQUAL                = 'nietgelijkaan'       # !=
    GREATERTHAN             = 'groterdan'           # >
    GREATERTHANOREQUALTO    = 'groterofgelijkaan'   # >=
    LESSTHAN                = 'kleinerdan'          # <
    LESSTHANOREQUALTO       = 'kleinerofgelijkaan'  # <=

    #Logical Operator Tokens
    LOGICALNOT      = 'niet'    # !
    LOGICALAND      = 'en'      # &&
    LOGICALOR       = 'of'      # ||

    #Special Chars Tokens
    LEFTROUNDBRACKET    = '('   # (
    RIGHTROUNDBRACKET   = ')'   # )
    LEFTSQUAREBRACKET   = '['   # [
    RIGHTSQUAREBRACKET  = ']'   # ]
    LEFTANGLEBRACKET    = '<'   # <
    RIGHTANGLEBRACKET   = '>'   # >
    LEFTCURLYBRACKET    = '{'   # {
    RIGHTCURLYBRACKET   = '}'   # }
    COMMA               = ','   # ,
    SEMICOLON           = ';'   # ;

    #Other Tokens
    IF          = 'als'         # if
    ELSEIF      = 'andersals'   # else if
    ELSE        = 'anders'      # else
    WHILE       = 'zolang'     # while
    PRINT       = 'print'       # cout <<
    RETURN      = 'geefterug'   # return
    VARIABLE    = 'VARIABLE'    # name of variable
    DIGIT       = 'DIGIT'       # any digit
    EOF         = 'EVB'         # EOF

class TokenData(NamedTuple):
    line: int
    char: int

class Token(NamedTuple):
    type: TokenType
    value: any
    token_data: TokenData

