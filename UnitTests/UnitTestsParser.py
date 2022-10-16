from multiprocessing.sharedctypes import Value
import operator
import unittest
from Lexer.ClassLexer import Lexer

from Lexer.ClassToken import Token, TokenType
from Parser.ClassNodes import *
from Parser.ClassParser import Parser

class TestParser(unittest.TestCase):
    def test_parser_get_parameters(self):
        list_of_tokens = [
            Token(type=TokenType.BOOL, value=bool, token_data=None),
            Token(type=TokenType.VARIABLE, value='a', token_data=None),
            Token(type=TokenType.COMMA, value=',', token_data=None),
            Token(type=TokenType.INT, value=int, token_data=None),
            Token(type=TokenType.VARIABLE, value='b', token_data=None),
            Token(type=TokenType.COMMA, value=',', token_data=None),
            Token(type=TokenType.FLOAT, value=float, token_data=None),
            Token(type=TokenType.VARIABLE, value='c', token_data=None),
            Token(type=TokenType.RIGHTROUNDBRACKET, value=')', token_data=None),
        ]
        parameter_list, list_of_tokens = Parser().get_parameters(list_of_tokens)
        self.assertEqual(parameter_list[0].name, 'a')
        self.assertEqual(parameter_list[0].return_type, bool)
        self.assertEqual(parameter_list[1].name, 'b')
        self.assertEqual(parameter_list[1].return_type, int)
        self.assertEqual(parameter_list[2].name, 'c')
        self.assertEqual(parameter_list[2].return_type, float)
        self.assertEqual(len(parameter_list), 3, 'The length of the parameter list should be 3')
        self.assertEqual(len(list_of_tokens), 1, "The last token is the right round bracket")
        
    def test_parser_get_parameters_with_no_parameters(self):
        list_of_tokens = [
            Token(type=TokenType.RIGHTROUNDBRACKET, value=')', token_data=None),
        ]
        parameter_list, list_of_tokens = Parser().get_parameters(list_of_tokens)
        self.assertEqual(len(parameter_list), 0, 'The length of the parameter list should be 0')
        self.assertEqual(len(list_of_tokens), 1, "The last token is the right round bracket")
        
    def test_parser_get_call_parameters(self):
        list_of_tokens = [
            Token(type=TokenType.DIGIT, value=15, token_data=None),
            Token(type=TokenType.COMMA, value=',', token_data=None),
            Token(type=TokenType.VARIABLE, value='a', token_data=None),
            Token(type=TokenType.RIGHTROUNDBRACKET, value=')', token_data=None)
        ]
        parameter_list, list_of_tokens = Parser().get_call_parameters(list_of_tokens)
        self.assertEqual(parameter_list[0].value, 15) # The first parameter is a ValueNode
        self.assertEqual(type(parameter_list[0]), ValueNode)
        self.assertEqual(parameter_list[1].name, 'a') # The second parameter is a VariableCallNode
        self.assertEqual(type(parameter_list[1]), VariableCallNode)
        
class TestNodes(unittest.TestCase):
    def test_node_value_node(self):
        string = "15"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), ValueNode)
        self.assertEqual(node.value, 15)
        
    def test_node_variable_declaration_node(self):
        string = "getal a;"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), VariableDeclarationNode)
        self.assertEqual(node.return_type, int)
        self.assertEqual(node.name, 'a')
        
    def test_node_variable_definition_node(self):
        string = "getal a is 15;"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), VariableDefinitionNode)
        self.assertEqual(node.return_type, int)
        self.assertEqual(node.name, 'a')
        self.assertEqual(type(node.value), ValueNode)
        self.assertEqual(node.value.value, 15)
        
    def test_node_variable_assignment_node(self):
        string = "a is 15;"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), VariableAssignmentNode)
        self.assertEqual(node.name, 'a')
        self.assertEqual(type(node.value), ValueNode)
        self.assertEqual(node.value.value, 15)
        
    def test_node_variable_call_node(self):
        string = "a"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), VariableCallNode)
        self.assertEqual(node.name, 'a')
        
    def test_node_function_declaration_node(self):
        string = "getal a(janee b, zweef c);"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), FunctionDeclarationNode)
        self.assertEqual(node.name, 'a')
        self.assertEqual(node.return_type, int)
        self.assertEqual(len(node.parameters), 2)
        self.assertEqual(type(node.parameters[0]), VariableDeclarationNode)
        self.assertEqual(node.parameters[0].name, 'b')
        self.assertEqual(node.parameters[0].return_type, bool)
        self.assertEqual(type(node.parameters[1]), VariableDeclarationNode)
        self.assertEqual(node.parameters[1].name, 'c')
        self.assertEqual(node.parameters[1].return_type, float)
    
    def test_node_function_definition_node(self):
        string = "getal a(janee b, zweef c){getal a is 5;}"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), FunctionDefinitionNode)
        self.assertEqual(node.name, 'a')
        self.assertEqual(node.return_type, int)
        self.assertEqual(len(node.parameters), 2)
        self.assertEqual(type(node.parameters[0]), VariableDeclarationNode)
        self.assertEqual(node.parameters[0].name, 'b')
        self.assertEqual(node.parameters[0].return_type, bool)
        self.assertEqual(type(node.parameters[1]), VariableDeclarationNode)
        self.assertEqual(node.parameters[1].name, 'c')
        self.assertEqual(node.parameters[1].return_type, float)
        self.assertEqual(type(node.body[0]), VariableDefinitionNode)
        
    def test_node_function_call_node(self):
        string = "a(b, 15);"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), FunctionCallNode)
        self.assertEqual(node.name, 'a')
        self.assertEqual(len(node.parameters), 2)
        self.assertEqual(type(node.parameters[0]), VariableCallNode)
        self.assertEqual(node.parameters[0].name, 'b')
        self.assertEqual(type(node.parameters[1]), ValueNode)
        
    def test_node_expression_node(self):
        string = "a plus 15;"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), ExpressionNode)
        self.assertEqual(type(node.lhs), VariableCallNode)
        self.assertEqual(node.lhs.name, 'a')
        self.assertEqual(node.expression_type, operator.add)
        self.assertEqual(type(node.rhs), ValueNode)
        self.assertEqual(node.rhs.value, 15)
        
    def test_node_conditional_node(self):
        string = "als (a gelijkaan 15){a is 5;};"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), ConditionalNode)
        self.assertEqual(type(node.expression), ExpressionNode)
        self.assertEqual(type(node.body[0]), VariableAssignmentNode)
        
    def test_node_return_node(self):
        string = "geefterug 15;"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), ReturnNode)
        self.assertEqual(type(node.value), ValueNode)
        
    def test_node_while_node(self):
        string = "zolang (a gelijkaan 15){a is 5;};"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), WhileNode)
        self.assertEqual(type(node.expression), ExpressionNode)
        self.assertEqual(type(node.body[0]), VariableAssignmentNode)
        
    def test_node_print_node(self):
        string = "print(a);"
        tokens = Lexer().get_token_list(string)
        node, tokens = Parser().get_next_node(tokens)
        self.assertEqual(type(node), PrintNode)
        self.assertEqual(type(node.value), VariableCallNode)