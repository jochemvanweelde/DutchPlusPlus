from lib2to3.pgen2.token import tok_name
import operator
import unittest

from Lexer.ClassLexer import Lexer
from Lexer.ClassToken import TokenData, TokenType

class TestLexer(unittest.TestCase):
    def test_lexer_next_digit_int(self):
        string = "10 + 20"
        integer = Lexer().get_next_digit(string)
        self.assertEqual(integer, "10", "The integer should be 10") 
        self.assertEqual(type(integer), str, "The type of the integer should be a string")
        self.assertEqual(int(integer), 10, "The integer string should be castable to an integer")
        self.assertEqual(string, "10 + 20", "The string should not change")

    def test_lexer_next_digit_float(self):
        string = "10.5 + 20"
        float_var = Lexer().get_next_digit(string)
        self.assertEqual(float_var, "10.5", "The float should be 10.5") 
        self.assertEqual(type(float_var), str, "The type of the float should be a string")
        self.assertEqual(float(float_var), 10.5, "The float string should be castable to a float")
        self.assertEqual(string, "10.5 + 20", "The string should not change")
        
    def test_lexer_next_word(self):
        string = "Hello World"
        next_word = Lexer().get_next_word(string)
        self.assertEqual(next_word, "Hello", "The next word should be Hello")
        self.assertEqual(type(next_word), str, "The type of the next word should be a string")
        self.assertEqual(string, "Hello World", "The string should not change")
        
    def test_lexer_get_token_list(self):
        # This test does not test Token types only if the function returns a list of tokens in a specific order.
        string = "getal a is 10;"
        result = Lexer().get_token_list(string)
        self.assertEqual(result[0].type, TokenType.INT, "The first token should be an INT Tokentype")
        self.assertEqual(result[1].type, TokenType.VARIABLE, "The first token should be an VARIABLE Tokentype")
        self.assertEqual(result[2].type, TokenType.IS, "The first token should be an IS Tokentype")
        self.assertEqual(result[3].type, TokenType.DIGIT, "The first token should be an DIGIT Tokentype")
        self.assertEqual(result[4].type, TokenType.SEMICOLON, "The first token should be an SEMICOLON Tokentype")
        self.assertEqual(result[5].type, TokenType.EOF, "The first token should be an EOF Tokentype")
        
    def test_lexer_get_token_list_invalid_token(self):
        string = "getal a = 10;"
        with self.assertRaises(Exception):
            Lexer().get_token_list(string)
            
            
class TestToken(unittest.TestCase):
    def test_token_bool(self):
        token_string = "janee"
        data_str, token_data, token = Lexer().get_token(token_string)
        self.assertEqual(token.type, TokenType.BOOL, "The token should be a BOOL Tokentype")
        self.assertEqual(token.value, bool, "The token value should be a bool")
        self.assertEqual(data_str, "", "The data string should be empty")
        self.assertEqual(token_data, TokenData(1,6), "janee is 5 characters long so the token data should be 1,6")
        
    def test_token_int(self): 
        token_string = "getal"
        data_str, token_data, token = Lexer().get_token(token_string)
        self.assertEqual(token.type, TokenType.INT, "The token should be a INT Tokentype")
        self.assertEqual(token.value, int, "The token value should be an int")
        self.assertEqual(data_str, "", "The data string should be empty")
        self.assertEqual(token_data, TokenData(1,6), "getal is 5 characters long so the token data should be 1,6")
        
    def test_token_int(self): 
        token_string = "zweef"
        data_str, token_data, token = Lexer().get_token(token_string)
        self.assertEqual(token.type, TokenType.FLOAT, "The token should be a FLOAT Tokentype")
        self.assertEqual(token.value, float, "The token value should be a float")
        self.assertEqual(data_str, "", "The data string should be empty")
        self.assertEqual(token_data, TokenData(1,6), "getal is 5 characters long so the token data should be 1,6")
    
    def test_token_true(self): 
        token_string = "waar"
        data_str, token_data, token = Lexer().get_token(token_string)
        self.assertEqual(token.type, TokenType.TRUE, "The token should be a TRUE Tokentype")
        self.assertEqual(token.value, True, "The token value should be True")
        self.assertEqual(data_str, "", "The data string should be empty")
        self.assertEqual(token_data, TokenData(1,5), "getal is 4 characters long so the token data should be 1,5")
    
    def test_token_int(self): 
        token_string = "onwaar"
        data_str, token_data, token = Lexer().get_token(token_string)
        self.assertEqual(token.type, TokenType.FALSE, "The token should be a FALSE Tokentype")
        self.assertEqual(token.value, False, "The token value should be False")
        self.assertEqual(data_str, "", "The data string should be empty")
        self.assertEqual(token_data, TokenData(1,7), "getal is 6 characters long so the token data should be 1,7")
    
    def test_token_addition(self): 
        token_string = "plus"
        data_str, token_data, token = Lexer().get_token(token_string)
        self.assertEqual(token.type, TokenType.ADDIDION, "The token should be a ADDITION Tokentype")
        self.assertEqual(token.value, operator.add, "The token value should be the add operator")
        self.assertEqual(data_str, "", "The data string should be empty")
        self.assertEqual(token_data, TokenData(1,5), "getal is 4 characters long so the token data should be 1,5")
    
    def test_token_subtraction(self): 
       token_string = "min"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.SUBTRACTION, "The token should be a SUBTRACTION Tokentype")
       self.assertEqual(token.value, operator.sub, "The token value should be the sub operator")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,4), "getal is 3 characters long so the token data should be 1,4")
    
    def test_token_multiplication(self): 
       token_string = "keer"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.MULTIPLICATION, "The token should be a MULTIPLICATION Tokentype")
       self.assertEqual(token.value, operator.mul, "The token value should be the mul operator")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,5), "getal is 4 characters long so the token data should be 1,5")
    
    def test_token_division(self): 
       token_string = "delendoor"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.DIVISION, "The token should be a DIVISION Tokentype")
       self.assertEqual(token.value, operator.truediv, "The token value should be a truediv operator")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,10), "getal is 9 characters long so the token data should be 1,10")
    
    def test_token_assignment(self): 
       token_string = "is"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.IS, "The token should be a IS Tokentype")
       self.assertEqual(token.value, None, "The token value should be None")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,3), "getal is 2 characters long so the token data should be 1,3")
    
    def test_token_equalto(self): 
       token_string = "gelijkaan"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.EQUALTO, "The token should be a EQUALTO Tokentype")
       self.assertEqual(token.value, operator.eq, "The token value should be the eq operator")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,10), "getal is 9 characters long so the token data should be 1,10")
    
    def test_token_not_equal(self): 
       token_string = "nietgelijkaan"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.NOTEQUAL, "The token should be a NOTEQUAL Tokentype")
       self.assertEqual(token.value, operator.ne, "The token value should be the ne operator")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,14), "getal is 13 characters long so the token data should be 1,14")
    
    def test_token_greater_than(self): 
       token_string = "groterdan"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.GREATERTHAN, "The token should be a GREATERTHAN Tokentype")
       self.assertEqual(token.value, operator.gt, "The token value should be the gt operator")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,10), "getal is 9 characters long so the token data should be 1,10")
    
    def test_token_greater_than_or_equal_to(self): 
       token_string = "groterofgelijkaan"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.GREATERTHANOREQUALTO, "The token should be a GREATERTHANOREQUALTO Tokentype")
       self.assertEqual(token.value, operator.ge, "The token value should be the ge operator")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,18), "getal is 17 characters long so the token data should be 1,18")
    
    def test_token_less_than(self): 
       token_string = "kleinerdan"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.LESSTHAN, "The token should be a LESSTHAN Tokentype")
       self.assertEqual(token.value, operator.lt, "The token value should be the lt operator")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,11), "getal is 10 characters long so the token data should be 1,11")
    
    def test_token_less_than_or_equal_to(self): 
       token_string = "kleinerofgelijkaan"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.LESSTHANOREQUALTO, "The token should be a LESSTHANOREQUALTO Tokentype")
       self.assertEqual(token.value, operator.le, "The token value should be the le operator")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,19), "getal is 18 characters long so the token data should be 1,19")
    
    def test_token_logical_not(self): 
       token_string = "niet"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.LOGICALNOT, "The token should be a LOGICALNOT Tokentype")
       self.assertEqual(token.value, operator.not_, "The token value should be the not_ operator")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,5), "getal is 4 characters long so the token data should be 1,5")
    
    def test_token_logical_and(self): 
       token_string = "en"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.LOGICALAND, "The token should be a LOGICALAND Tokentype")
       self.assertEqual(token.value, operator.and_, "The token value should be the and_ operator")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,3), "getal is 2 characters long so the token data should be 1,3")
    
    def test_token_logical_or(self): 
       token_string = "of"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.LOGICALOR, "The token should be a LOGICALOR Tokentype")
       self.assertEqual(token.value, operator.or_, "The token value should be the or_ operator")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,3), "getal is 2 characters long so the token data should be 1,3")
    
    def test_token_left_round_bracket(self): 
       token_string = "("
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.LEFTROUNDBRACKET, "The token should be a LEFTROUNDBRACKET Tokentype")
       self.assertEqual(token.value, '(', "The token value should be a (")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,2), "getal is 1 characters long so the token data should be 1,2")
    
    def test_token_right_round_bracket(self): 
       token_string = ")"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.RIGHTROUNDBRACKET, "The token should be a RIGHTROUNDBRACKET Tokentype")
       self.assertEqual(token.value, ')', "The token value should be a )")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,2), "getal is 1 characters long so the token data should be 1,2")
    
    def test_token_left_square_bracket(self): 
       token_string = "["
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.LEFTSQUAREBRACKET, "The token should be a LEFTSQUAREBRACKET Tokentype")
       self.assertEqual(token.value, '[', "The token value should be a [")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,2), "getal is 1 characters long so the token data should be 1,2")
    
    def test_token_right_square_bracket(self): 
       token_string = "]"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.RIGHTSQUAREBRACKET, "The token should be a RIGHTSQUAREBRACKET Tokentype")
       self.assertEqual(token.value, ']', "The token value should be a ]")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,2), "getal is 1 characters long so the token data should be 1,2")
    
    def test_token_left_angle_bracket(self): 
       token_string = "<"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.LEFTANGLEBRACKET, "The token should be a LEFTANGLEBRACKET Tokentype")
       self.assertEqual(token.value, '<', "The token value should be a <")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,2), "getal is 1 characters long so the token data should be 1,2")
    
    def test_token_right_angle_bracket(self): 
       token_string = ">"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.RIGHTANGLEBRACKET, "The token should be a RIGHTANGLEBRACKET Tokentype")
       self.assertEqual(token.value, '>', "The token value should be a >")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,2), "getal is 1 characters long so the token data should be 1,2")
    
    def test_token_left_curly_bracket(self): 
       token_string = "{"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.LEFTCURLYBRACKET, "The token should be a LEFTCURLYBRACKET Tokentype")
       self.assertEqual(token.value, '{', "The token value should be a {")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,2), "getal is 1 characters long so the token data should be 1,2")
    
    def test_token_right_curly_bracket(self): 
       token_string = "}"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.RIGHTCURLYBRACKET, "The token should be a RIGHTCURLYBRACKET Tokentype")
       self.assertEqual(token.value, '}', "The token value should be a }")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,2), "getal is 1 characters long so the token data should be 1,2")
    
    def test_token_comma(self): 
       token_string = ","
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.COMMA, "The token should be a COMMA Tokentype")
       self.assertEqual(token.value, ',', "The token value should be a ,")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,2), "getal is 1 characters long so the token data should be 1,2")
    
    def test_token_semicolon(self): 
       token_string = ";"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.SEMICOLON, "The token should be a SEMICOLON Tokentype")
       self.assertEqual(token.value, ';', "The token value should be a ;")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,2), "getal is 1 characters long so the token data should be 1,2")
    
    def test_token_if(self): 
       token_string = "als"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.IF, "The token should be a IF Tokentype")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,4), "getal is 3 characters long so the token data should be 1,4")
    
    def test_token_else_if(self): 
       token_string = "andersals"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.ELSEIF, "The token should be a ELSEIF Tokentype")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,10), "getal is 9 characters long so the token data should be 1,10")
    
    def test_token_else(self): 
       token_string = "anders"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.ELSE, "The token should be a ELSE Tokentype")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,7), "getal is 6 characters long so the token data should be 1,7")
    
    def test_token_while(self): 
       token_string = "zolang"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.WHILE, "The token should be a WHILE Tokentype")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,7), "getal is 6 characters long so the token data should be 1,7")
    
    def test_token_print(self): 
       token_string = "print"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.PRINT, "The token should be a PRINT Tokentype")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,6), "getal is 5 characters long so the token data should be 1,6")
    
    def test_token_variable(self): 
       token_string = "anyVariable"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.VARIABLE, "The token should be a VARIABLE Tokentype")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,len(token_string) + 1), "getal is len(token_string) characters long so the token data should be 1,len(token_string) + 1")
    
    def test_token_digit(self): 
       token_string = "234562"
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.DIGIT, "The token should be a DIGIT Tokentype")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,len(token_string)+1), "getal is len(token_string) characters long so the token data should be 1,len(token_string)+1")
    
    def test_token_eof(self): 
       token_string = ""
       data_str, token_data, token = Lexer().get_token(token_string)
       self.assertEqual(token.type, TokenType.EOF, "The token should be a EOF Tokentype")
       self.assertEqual(data_str, "", "The data string should be empty")
       self.assertEqual(token_data, TokenData(1,1), "getal is 0 characters long so the token data should be 1,1")
    
    