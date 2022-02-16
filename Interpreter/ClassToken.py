from dataclasses import dataclass
from enum import Enum

#====ALL TOKENS====
class TokenType(Enum):
    #Data Types Tokens
    BOOL      = 'janee'           # bool
    INT       = 'getal'           # int
    FLOAT     = 'zweef'           # float
    STRING    = 'woord'           # string

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
    WHILE       = 'terwijl'     # while
    PRINT       = 'print'       # cout <<
    RETURN      = 'geefterug'   # return
    VARIABLE    = any           # name of variable
    DIGIT       = any           # any digit
    EOF         = 'EVB'         # EOF

@dataclass
class TokenData:
    line: int
    char: int

class Token:
    def __init__(self, type: TokenType, value: str, token_data: TokenData) -> None:
        self.type = type
        self.value = value
        self.token_data = token_data

    def __str__(self) -> str:
        return 'Token({type}, {value}, {token_data})'.format(
            type=self.type,
            value=repr(self.value),
            token_data=repr(self.token_data)
        )
    
    def __repr__(self) -> str:
        return self.__str__()

