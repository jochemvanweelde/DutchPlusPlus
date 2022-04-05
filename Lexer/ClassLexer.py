import copy
from pprint import pprint
from typing import List
from Lexer.ClassToken import Token, TokenData, TokenType
import operator


class Lexer:
    '''Lexer class that translates a string into a list with tokens'''
    
    def get_next_digit(self, data: str) -> str:
        '''returns the next integer or float from text'''
        if data != '' and (data[0].isdigit() or data[0] == '.'):
            return data[0] + self.get_next_digit(data[1:])
        return ''

    def get_next_word(self, data: str) -> str:
        '''returns the next word'''
        if data != '' and data[0].isalpha():
            return data[0] + self.get_next_word(data[1:])
        return ''

    def get_token_list(self, data: str, token_data: TokenData = TokenData(1,1)) -> List[Token]:
        '''get the token list'''
        result_token: Token = None
        result_string: str = None
        if data == "":
            result_token = Token(TokenType.EOF, result_string, token_data)
            return [result_token]

        # Check if first char in data is a digit and get the full digit string
        # then check if the digit string is an integer or float and fill result_token with the corresponding TokenType
        elif data[0].isdigit():
            result_string = self.get_next_digit(data)
            if result_string.isdigit():
                result_token = Token(TokenType.DIGIT, int(result_string), token_data)
            elif result_string.count('.') == 1:
                result_token = Token(TokenType.DIGIT, float(result_string), token_data)
            else:
                raise Exception(f"Illegal digit string '{result_string}'")

        # Check if first char in data is a letter and get the full word string
        # then check if the word string is a keyword and fill result_token with the corresponding TokenType
        elif data[0].isalpha():
            result_string = self.get_next_word(data)
            match result_string:
                case TokenType.BOOL.value:
                    result_token = Token(TokenType.BOOL, bool, token_data)
                case TokenType.INT.value:
                    result_token = Token(TokenType.INT, int, token_data)
                case TokenType.FLOAT.value:
                    result_token = Token(TokenType.FLOAT, float, token_data)
                case TokenType.STRING.value:
                    result_token = Token(TokenType.STRING, str, token_data)
                case TokenType.TRUE.value:
                    result_token = Token(TokenType.TRUE, True, token_data)
                case TokenType.FALSE.value:
                    result_token = Token(TokenType.FALSE, False, token_data)
                case TokenType.ADDIDION.value:
                    result_token = Token(TokenType.ADDIDION, operator.add, token_data)
                case TokenType.SUBTRACTION.value:
                    result_token = Token(TokenType.SUBTRACTION, operator.sub, token_data)
                case TokenType.MULTIPLICATION.value:
                    result_token = Token(TokenType.MULTIPLICATION, operator.mul, token_data)
                case TokenType.DIVISION.value:
                    result_token = Token(TokenType.DIVISION, operator.truediv, token_data)
                case TokenType.IS.value:
                    result_token = Token(TokenType.IS, None, token_data)
                case TokenType.EQUALTO.value:
                    result_token = Token(TokenType.EQUALTO, operator.eq, token_data)
                case TokenType.NOTEQUAL.value:
                    result_token = Token(TokenType.NOTEQUAL, operator.ne, token_data)
                case TokenType.GREATERTHAN.value:
                    result_token = Token(TokenType.GREATERTHAN, operator.gt, token_data)
                case TokenType.GREATERTHANOREQUALTO.value:
                    result_token = Token(TokenType.GREATERTHANOREQUALTO, operator.ge, token_data)
                case TokenType.LESSTHAN.value:
                    result_token = Token(TokenType.LESSTHAN, operator.lt, token_data)
                case TokenType.LESSTHANOREQUALTO.value:
                    result_token = Token(TokenType.LESSTHANOREQUALTO, operator.le, token_data)
                case TokenType.LOGICALNOT.value:
                    result_token = Token(TokenType.LOGICALNOT, operator.not_, token_data)
                case TokenType.LOGICALAND.value:
                    result_token = Token(TokenType.LOGICALAND, operator.and_, token_data)
                case TokenType.LOGICALOR.value:
                    result_token = Token(TokenType.LOGICALOR, operator.or_, token_data)
                case TokenType.IF.value:
                    result_token = Token(TokenType.IF, result_string, token_data)
                case TokenType.ELSEIF.value:
                    result_token = Token(TokenType.ELSEIF, result_string, token_data)
                case TokenType.ELSE.value:
                    result_token = Token(TokenType.ELSE, result_string, token_data)
                case TokenType.WHILE.value:
                    result_token = Token(TokenType.WHILE, result_string, token_data)
                case TokenType.PRINT.value:
                    result_token = Token(TokenType.PRINT, result_string, token_data)
                case TokenType.RETURN.value:
                    result_token = Token(TokenType.RETURN, result_string, token_data)
                case _:    
                    result_token = Token(TokenType.VARIABLE, result_string, token_data)

        # Check if first char in data is a space or a newline character
        # then change the TokenData accordingly and call this function recursively with the next char
        elif data[0].isspace():
            if data[0] == '\n':
                token_data.line += 1
                token_data.char = 1
            token_data.char += 1
            return self.get_token_list(data[1:], token_data)
        
        # Check if first char in data is a special char and fill result_token with the corresponding TokenType
        else:
            result_string = data[0]
            match data[0]:
                case TokenType.LEFTROUNDBRACKET.value:
                    result_token = Token(TokenType.LEFTROUNDBRACKET, result_string, token_data)
                case TokenType.RIGHTROUNDBRACKET.value:
                    result_token = Token(TokenType.RIGHTROUNDBRACKET, result_string, token_data)
                case TokenType.LEFTSQUAREBRACKET.value:
                    result_token = Token(TokenType.LEFTSQUAREBRACKET, result_string, token_data)
                case TokenType.RIGHTSQUAREBRACKET.value:
                    result_token = Token(TokenType.RIGHTSQUAREBRACKET, result_string, token_data)
                case TokenType.LEFTANGLEBRACKET.value:
                    result_token = Token(TokenType.LEFTANGLEBRACKET, result_string, token_data)
                case TokenType.RIGHTANGLEBRACKET.value:
                    result_token = Token(TokenType.RIGHTANGLEBRACKET, result_string, token_data)
                case TokenType.LEFTCURLYBRACKET.value:
                    result_token = Token(TokenType.LEFTCURLYBRACKET, result_string, token_data)
                case TokenType.RIGHTCURLYBRACKET.value:
                    result_token = Token(TokenType.RIGHTCURLYBRACKET, result_string, token_data)
                case TokenType.COMMA.value:
                    result_token = Token(TokenType.COMMA, result_string, token_data)
                case TokenType.SEMICOLON.value:
                    result_token = Token(TokenType.SEMICOLON, result_string, token_data)
        
        # Check if result_string or result_token is None, then give an error message
        if result_string is None or result_token is None:
            raise Exception('Error: Invalid token')

        # First change token_data based on the result_string and then call this function recursively with the next char
        length_result_string = len(result_string)
        new_token_data: TokenData = copy.copy(token_data)
        new_token_data.char += length_result_string
        return [result_token] + self.get_token_list(data[length_result_string:], new_token_data)

if __name__ == '__main__':
    with open('dutchPlusPlusLoopig.txt', 'r') as file:
        data = file.read()
    
    lexer = Lexer()
    token_list = lexer.get_token_list(data)

    pprint(token_list)

        
