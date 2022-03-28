from typing import List
from Interpreter.Lexer.ClassToken import Token, TokenData, TokenType
from copy import copy
import operator

class Lexer:
    def __init__(self, text: str) -> None:
        self.text = text
        self.pos = 0
        self.token_data = TokenData(1,1)
        self.current_token = None


    def error(self) -> Exception:
        raise Exception("Error!")

    def add_pos(self, adder: int):
        self.pos += adder
        self.token_data.char += adder
    
    def add_new_line(self):
        self.add_pos(1)
        self.token_data.line += 1
        self.token_data.char = 1

    def is_eof(self, text: str, pos: int) -> bool:
        return pos > len(text) - 1

    def get_next_token(self) -> Token:
        if self.pos > len(self.text) - 1:
            return Token(TokenType.EOF, None, self.token_data)
        
        current_char = self.text[self.pos]
        current_token_data = copy(self.token_data)

        if current_char.isdigit():
            digit = current_char + self.get_next_digit(self.text, self.pos+1)
            self.add_pos(len(digit))
            if digit.count('.') == 0:
                return Token(TokenType.DIGIT, int(digit), current_token_data)
            if digit.count('.') == 1:
                return Token(TokenType.DIGIT, float(digit), current_token_data)
            raise Exception("Wrong number format: " + digit)

        elif current_char.isalpha():
            word = current_char + self.get_next_word(self.text, self.pos+1)
            self.add_pos(len(word))

            match word:
                case TokenType.BOOL.value:
                    return Token(TokenType.BOOL, bool, current_token_data)
                case TokenType.INT.value:
                    return Token(TokenType.INT, int, current_token_data)
                case TokenType.FLOAT.value:
                    return Token(TokenType.FLOAT, float, current_token_data)
                case TokenType.STRING.value:
                    return Token(TokenType.STRING, str, current_token_data)
                case TokenType.TRUE.value:
                    return Token(TokenType.TRUE, True, current_token_data)
                case TokenType.FALSE.value:
                    return Token(TokenType.FALSE, False, current_token_data)
                case TokenType.ADDIDION.value:
                    return Token(TokenType.ADDIDION, operator.add, current_token_data)
                case TokenType.SUBTRACTION.value:
                    return Token(TokenType.SUBTRACTION, operator.sub, current_token_data)
                case TokenType.MULTIPLICATION.value:
                    return Token(TokenType.MULTIPLICATION, operator.mul, current_token_data)
                case TokenType.DIVISION.value:
                    return Token(TokenType.DIVISION, operator.truediv, current_token_data)
                case TokenType.IS.value:
                    return Token(TokenType.IS, None, current_token_data)
                case TokenType.EQUALTO.value:
                    return Token(TokenType.EQUALTO, operator.eq, current_token_data)
                case TokenType.NOTEQUAL.value:
                    return Token(TokenType.NOTEQUAL, operator.ne, current_token_data)
                case TokenType.GREATERTHAN.value:
                    return Token(TokenType.GREATERTHAN, operator.gt, current_token_data)
                case TokenType.GREATERTHANOREQUALTO.value:
                    return Token(TokenType.GREATERTHANOREQUALTO, operator.ge, current_token_data)
                case TokenType.LESSTHAN.value:
                    return Token(TokenType.LESSTHAN, operator.lt, current_token_data)
                case TokenType.LESSTHANOREQUALTO.value:
                    return Token(TokenType.LESSTHANOREQUALTO, operator.le, current_token_data)
                case TokenType.LOGICALNOT.value:
                    return Token(TokenType.LOGICALNOT, operator.not_, current_token_data)
                case TokenType.LOGICALAND.value:
                    return Token(TokenType.LOGICALAND, operator.and_, current_token_data)
                case TokenType.LOGICALOR.value:
                    return Token(TokenType.LOGICALOR, operator.or_, current_token_data)
                case TokenType.IF.value:
                    return Token(TokenType.IF, None, current_token_data)
                case TokenType.ELSEIF.value:
                    return Token(TokenType.ELSEIF, None, current_token_data)
                case TokenType.ELSE.value:
                    return Token(TokenType.ELSE, None, current_token_data)
                case TokenType.WHILE.value:
                    return Token(TokenType.WHILE, None, current_token_data)
                case TokenType.PRINT.value:
                    return Token(TokenType.PRINT, None, current_token_data)
                case TokenType.RETURN.value:
                    return Token(TokenType.RETURN, None, current_token_data)
            return Token(TokenType.VARIABLE, word, current_token_data)

        elif current_char.isspace():
            if current_char == '\n':
                self.add_new_line()
            else:
                self.add_pos(1)
            return self.get_next_token()
        

        match current_char:
            case TokenType.LEFTROUNDBRACKET.value:
                self.add_pos(1)
                return Token(TokenType.LEFTROUNDBRACKET, None, current_token_data)
            case TokenType.RIGHTROUNDBRACKET.value:
                self.add_pos(1)
                return Token(TokenType.RIGHTROUNDBRACKET, None, current_token_data)
            case TokenType.LEFTSQUAREBRACKET.value:
                self.add_pos(1)
                return Token(TokenType.LEFTSQUAREBRACKET, None, current_token_data)
            case TokenType.RIGHTSQUAREBRACKET.value:
                self.add_pos(1)
                return Token(TokenType.RIGHTSQUAREBRACKET, None, current_token_data)
            case TokenType.LEFTANGLEBRACKET.value:
                self.add_pos(1)
                return Token(TokenType.LEFTANGLEBRACKET, None, current_token_data)
            case TokenType.RIGHTANGLEBRACKET.value:
                self.add_pos(1)
                return Token(TokenType.RIGHTANGLEBRACKET, None, current_token_data)
            case TokenType.LEFTCURLYBRACKET.value:
                self.add_pos(1)
                return Token(TokenType.LEFTCURLYBRACKET, None, current_token_data)
            case TokenType.RIGHTCURLYBRACKET.value:
                self.add_pos(1)
                return Token(TokenType.RIGHTCURLYBRACKET, None, current_token_data)
            case TokenType.COMMA.value:
                self.add_pos(1)
                return Token(TokenType.COMMA, None, current_token_data)
            case TokenType.SEMICOLON.value:
                self.add_pos(1)
                return Token(TokenType.SEMICOLON, None, current_token_data)

        raise Exception(f"Character '{current_char}' not recognised: " + current_token_data.__str__())

    def get_next_digit(self, text: str, pos: int) -> str:
        # add chars if next char is a digit or a dot (floats)
        if not self.is_eof(text, pos) and (text[pos].isdigit() or text[pos] == '.'):
            return text[pos] + self.get_next_digit(text, pos+1)
        return ''

    def get_next_word(self, text: str, pos: int) -> str:
        if not self.is_eof(text, pos) and text[pos].isalpha():
            return text[pos] + self.get_next_word(text, pos+1)
        return ''

def get_token_list(data: str, debug : bool = False) -> List[Token]:
    # with open('dutchPlusPlus.txt', 'r') as file:
    #     data = file.read()

    a = Lexer(data)
    current_tokens : List[TokenType] = []
    current_tokens.append(a.get_next_token())

    while not current_tokens[-1].__str__().__contains__('EOF'):
        current_tokens.append(a.get_next_token())

    if debug:
        for token in current_tokens:
            print(token)
    return current_tokens
    
if __name__ == "__main__":
    get_token_list(True)
