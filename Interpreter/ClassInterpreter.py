from ClassToken import *

class Interpreter:
    def __init__(self, text: str) -> None:
        self.text = text
        self.pos = 0
        self.current_token = None

    def error(self) -> Exception:
        raise Exception("Error!")

    def is_eof(self, text, pos) -> bool:
        return pos > len(text) - 1

    def get_next_token(self) -> Token:
        if self.pos > len(self.text) - 1:
            return Token(EOF, None)

        current_char = self.text[self.pos]

        if current_char.isdigit():
            digit = current_char + self.get_next_digit(self.text, self.pos+1)
            self.pos += len(digit)
            return Token(GETAL, int(digit))

        elif current_char.isalpha():
            word = current_char + self.get_next_word(self.text, self.pos+1)
            self.pos += len(word)
            return Token(WOORD, word)

        elif current_char.isspace() or current_char == '\n':
            self.pos += 1
            return self.get_next_token()

        elif current_char == '+':
            self.pos += 1
            return Token(PLUS, '+')

        elif current_char == '-':
            self.pos += 1
            return Token(MIN, '-')
        
        elif current_char == '*':
            self.pos += 1
            return Token(KEER, '*')

        elif current_char == '/':
            self.pos += 1
            return Token(DEEL, '/')

        else:
            raise Exception("Character not recognised: " + current_char)

    def get_next_digit(self, text: str, pos: int) -> str:
        if not self.is_eof(text, pos) and text[pos].isdigit():
            return text[pos] + self.get_next_digit(text, pos+1)
        return ''

    def get_next_word(self, text: str, pos: int) -> str:
        if not self.is_eof(text, pos) and text[pos].isalpha():
            return text[pos] + self.get_next_word(text, pos+1)
        return ''

if __name__ == "__main__":
    a = Interpreter("10 + 3")
    print(a.get_next_token())
    print(a.get_next_token())
    print(a.get_next_token())
    print(a.get_next_token())
    # print(a.get_next_token())
    # print(a.get_next_token())
    
