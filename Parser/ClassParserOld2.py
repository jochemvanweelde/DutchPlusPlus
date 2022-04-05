

from pprint import pprint
import re
from typing import List, Tuple
from Lexer.ClassLexer import Lexer
from Lexer.ClassToken import Token, TokenData, TokenType
from Parser.ClassNodes import *


class ParserRecursive:
    def get_parameters(self, token_list: List[Token]) -> Tuple[List[Node], List[Token]]:
        node_list: List[Node] = []
        result_node: Node = None
        
        # Check if first token is a BOOL, INT, FLOAT or STRING
        if token_list[0].type in [TokenType.BOOL, TokenType.INT, TokenType.FLOAT, TokenType.STRING]:
            if token_list[1].type == TokenType.VARIABLE:
                result_node = VariableDeclarationNode(token_list[0].value, token_list[1].value)
        else:
            raise Exception("Expected variable type")
        
        if token_list[2].type == TokenType.COMMA:
            node_list, token_list = self.get_parameters(token_list[3:]) 
        return [result_node] + node_list, token_list[2:]

    def get_call_parameters(self, token_list: List[Token]) -> Tuple[List[Node], List[Token]]:
        node_list: List[Node] = []
        result_node: Node = None
        
        if token_list[0].type == TokenType.VARIABLE:
            result_node = VariableCallNode(token_list[0].value)
        elif token_list[0].type in [TokenType.DIGIT, TokenType.TRUE, TokenType.FALSE]:
            result_node = ValueNode(token_list[0].value)

        if token_list[1].type == TokenType.COMMA:
            node_list, token_list = self.get_call_parameters(token_list[2:])
        elif token_list[1].type in [TokenType.ADDIDION, TokenType.SUBTRACTION, TokenType.MULTIPLICATION, TokenType.DIVISION, TokenType.EQUALTO, TokenType.NOTEQUAL, TokenType.GREATERTHAN, TokenType.LESSTHAN]:
            conditional: Callable = token_list[1].value
            rhs, token_list= self.get_next_node_for_assignment_node(token_list[2:])
            return ExpressionNode(result_node, conditional, rhs), token_list
        return [result_node] + node_list, token_list[1:]

    def get_condition(self, token_list: List[Token]) -> Tuple[ExpressionNode, List[Token]]:
        if token_list[0].type == TokenType.VARIABLE:
            lhs: Node = VariableCallNode(token_list.pop(0).value)
        elif token_list[0].type in [TokenType.DIGIT, TokenType.TRUE, TokenType.FALSE]:
            lhs: Node = ValueNode(token_list.pop(0).value)
        
        conditional: Callable = token_list.pop(0).value

        if token_list[0].type == TokenType.VARIABLE:
            rhs: Node = VariableCallNode(token_list.pop(0).value)
        elif token_list[0].type in [TokenType.DIGIT, TokenType.TRUE, TokenType.FALSE]:
            rhs: Node = ValueNode(token_list.pop(0).value)

        return ExpressionNode(lhs, conditional, rhs), token_list

    def get_next_node_for_assignment_node(self, token_list: List[Token]) -> Tuple[Node, List[Token]]:
        lhs: Node = None

        if token_list[1].type == TokenType.LEFTROUNDBRACKET:
            function_name: str = token_list.pop(0).value
            token_list.pop(0) # Remove TokenType.LEFTROUNDBRACKET
            parameter_list, token_list = self.get_call_parameters(token_list)
            token_list.pop(0) # Remove TokenType.RIGHTROUNDBRACKET
            lhs = FunctionCallNode(function_name, parameter_list)
        else:
            if token_list[0].type == TokenType.VARIABLE:
                lhs = VariableCallNode(token_list.pop(0).value)
            elif token_list[0].type in [TokenType.DIGIT, TokenType.TRUE, TokenType.FALSE]:
                lhs = ValueNode(token_list.pop(0).value)
            else:
                raise Exception("Expected variable or value")

        if token_list[0].type in [TokenType.ADDIDION, TokenType.SUBTRACTION, TokenType.MULTIPLICATION, TokenType.DIVISION, TokenType.EQUALTO, TokenType.NOTEQUAL, TokenType.GREATERTHAN, TokenType.LESSTHAN]:
            conditional: Callable = token_list.pop(0).value
            rhs, token_list= self.get_next_node_for_assignment_node(token_list)
            return ExpressionNode(lhs, conditional, rhs), token_list
        elif token_list[0].type in [TokenType.SEMICOLON, TokenType.RIGHTROUNDBRACKET]:
            token_list.pop(0) # Remove TokenType.SEMICOLON or TokenType.RIGHTROUNDBRACKET
            return lhs, token_list
        elif token_list[0].type == TokenType.RIGHTCURLYBRACKET:
            return lhs, token_list
        else:
            raise Exception("Expected assignment operator or semicolon")

    def get_node_list(self, token_list: List[Token]) -> List[Node]:
        result_node: Node = None
        # Check if token_list is empty. If so, return empty list
        if len(token_list) == 0:
            return []

        # Check if the first token is a semicolon or EOF. If so pop it and call this function recursively.
        if token_list[0].type in [TokenType.SEMICOLON, TokenType.EOF]:
            token_list.pop(0)
            return self.get_node_list(token_list)

        # Check if the first token is a right bracket. If so return an empty list.
        if token_list[0].type == TokenType.RIGHTCURLYBRACKET:
            return []

        # Check if the second token is an expression.
        if token_list[1].type in [TokenType.ADDIDION, TokenType.SUBTRACTION, TokenType.MULTIPLICATION, TokenType.DIVISION, TokenType.EQUALTO, TokenType.NOTEQUAL, TokenType.GREATERTHAN, TokenType.GREATERTHANOREQUALTO, TokenType.LESSTHAN, TokenType.LESSTHANOREQUALTO]:
            result_node, token_list = self.get_condition(token_list)

        # Check if the first token is a digit
        elif token_list[0].type in [TokenType.DIGIT, TokenType.TRUE, TokenType.FALSE]:
            result_node = ValueNode(token_list.pop(0).value)

        # Check if first token is a BOOL, INT, FLOAT or STRING
        elif token_list[0].type in [TokenType.BOOL, TokenType.INT, TokenType.FLOAT, TokenType.STRING]:
            if token_list[1].type != TokenType.VARIABLE:
                raise Exception("Expected a variable name after the type")
            elif token_list[2].type == TokenType.SEMICOLON:
                # VariableDeclarationNode
                return_type: type = token_list.pop(0).value
                variable_name: str = token_list.pop(0).value
                result_node = VariableDeclarationNode(return_type, variable_name)
            elif token_list[2].type == TokenType.IS:
                # VariableDefinitionNode
                return_type: type = token_list.pop(0).value
                variable_name: str = token_list.pop(0).value
                token_list.pop(0) # Remove TokenType.IS
                value, token_list= self.get_next_node_for_assignment_node(token_list)
                result_node = VariableDefinitionNode(return_type, variable_name, value)
            elif token_list[2].type == TokenType.LEFTROUNDBRACKET:
                return_type: type = token_list.pop(0).value
                function_name: str = token_list.pop(0).value
                token_list.pop(0) # Remove TokenType.LEFTROUNDBRACKET
                parameter_list, token_list = self.get_parameters(token_list)
                token_list.pop(0) # Remove TokenType.RIGHTROUNDBRACKET
                if token_list[0].type == TokenType.LEFTCURLYBRACKET:
                    # FunctionDefinitionNode
                    token_list.pop(0) # Remove TokenType.LEFTCURLYBRACKET
                    body: List[Node] = self.get_node_list(token_list)
                    print("Middle: ", token_list)
                    token_list.pop(0) # Remove TokenType.RIGHTCURLYBRACKET
                    result_node = FunctionDefinitionNode(function_name, return_type, parameter_list, body)
                else:
                    # FunctionDeclarationNode
                    result_node = FunctionDeclarationNode(function_name, return_type, parameter_list)
        
        # Check if first token is a variable
        elif token_list[0].type == TokenType.VARIABLE:
            if token_list[1].type == TokenType.LEFTROUNDBRACKET:
                # FunctionCallNode
                function_name: str = token_list.pop(0).value
                token_list.pop(0) # Remove TokenType.LEFTROUNDBRACKET
                parameter_list, token_list = self.get_call_parameters(token_list)
                token_list.pop(0) # Remove TokenType.RIGHTROUNDBRACKET
                result_node = FunctionCallNode(function_name, parameter_list)
            elif token_list[1].type == TokenType.IS:
                # VariableAssignmentNode
                variable_name: str = token_list.pop(0).value
                token_list.pop(0) # Remove TokenType.IS
                value, token_list = self.get_next_node_for_assignment_node(token_list)
                result_node = VariableAssignmentNode(variable_name, value)
            else:
                # VariableCallNode
                variable_name: str = token_list.pop(0).value
                result_node = VariableCallNode(variable_name)
        
        # Check if first token is a Conditional
        elif token_list[0].type == TokenType.IF:
            token_list.pop(0) # Remove TokenType.IF
            token_list.pop(0) # Remove TokenType.LEFTROUNDBRACKET
            condition, token_list = self.get_condition(token_list)
            token_list.pop(0) # Remove TokenType.RIGHTROUNDBRACKET
            token_list.pop(0) # Remove TokenType.LEFTCURLYBRACKET
            body: List[Node] = self.get_node_list(token_list)
            token_list.pop(0) # Remove TokenType.RIGHTCURLYBRACKET
            result_node = ConditionalNode(condition, body)

        # Check if first token is a While
        elif token_list[0].type == TokenType.WHILE:
            token_list.pop(0) # Remove TokenType.WHILE
            token_list.pop(0) # Remove TokenType.LEFTROUNDBRACKET
            condition, token_list = self.get_condition(token_list)
            token_list.pop(0) # Remove TokenType.RIGHTROUNDBRACKET
            token_list.pop(0) # Remove TokenType.LEFTCURLYBRACKET
            body: List[Node] = self.get_node_list(token_list)
            token_list.pop(0) # Remove TokenType.RIGHTCURLYBRACKET
            result_node = WhileNode(condition, body)

        # Check if first token is a return
        elif token_list[0].type == TokenType.RETURN:
            token_list.pop(0) # Remove TokenType.RETURN
            return_node, token_list = self.get_next_node_for_assignment_node(token_list)
            result_node = ReturnNode(return_node)

        # Check if first token is a print
        elif token_list[0].type == TokenType.PRINT:
            token_list.pop(0) # Remove TokenType.PRINT
            token_list.pop(0) # Remove TokenType.LEFTROUNDBRACKET
            print_node, token_list = self.get_next_node_for_assignment_node(token_list)
            result_node = PrintNode(print_node)
            token_list.pop(0) # Remove TokenType.RIGHTROUNDBRACKET

        what_to_return = [result_node] + self.get_node_list(token_list)
        print("After:", token_list)
        return what_to_return

# ! HEY JOCHEM! De code blijft vasthangen op ADDITION omdat we in de fuctie single_token....blablabla
# ! alleen kijken naar 1 token naar node. Maar als je n is n plus 1 doet, werkt dat natuurlijk niet.
# ! Succes met oplossen!

if __name__ == '__main__':
    with open('dutchPlusPlusRecursive.txt', 'r') as file:
        data = file.read()

    # data = """
    # getal even(getal n, getal z);
    # """
  
    token_list: List[Token] = Lexer().get_token_list(data)

    # print(token_list)

    a = ParserRecursive().get_node_list(token_list)

    print("\nRESULT:")
    pprint(a)