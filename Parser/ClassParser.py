

from pprint import pprint
from typing import List, Tuple
from Lexer.ClassLexer import Lexer
from Lexer.ClassToken import Token, TokenType
from Parser.ClassNodes import *


class Parser:
    def get_parameters(self, token_list: List[Token]) -> Tuple[List[VariableDeclarationNode], List[Token]]:
        # Returns a list of VariableDeclarationNodes and a list of Tokens
        result_node: VariableDeclarationNode = None
        result_node_list: List[VariableDeclarationNode] = []
        if token_list[0].type == TokenType.RIGHTROUNDBRACKET:
            return [], token_list
        elif token_list[0].type in [TokenType.BOOL, TokenType.INT, TokenType.FLOAT, TokenType.STRING]:
            variable_type: type = token_list.pop(0).value
            variable_name: str = token_list.pop(0).value
            if token_list[0].type == TokenType.COMMA: token_list.pop(0) # Remove TokenType.COMMA if it exists
            result_node = VariableDeclarationNode(variable_type, variable_name)
            result_node_list, token_list = self.get_parameters(token_list)
        else:
            raise Exception("Expected variable type")
        return [result_node] + result_node_list, token_list

    def get_call_parameters(self, token_list: List[Token]) -> Tuple[List[Node], List[Token]]:
        # Returns a list of Nodes for the FunctionCallNode and a list of Tokens
        result_node: Node = None
        result_node_list: List[Node] = []

        if token_list[0].type == TokenType.RIGHTROUNDBRACKET:
            return [], token_list
        else:
            result_node, token_list = self.get_next_node(token_list)
            result_node_list, token_list = self.get_call_parameters(token_list)
            return [result_node] + result_node_list, token_list
        

    def get_next_node(self, token_list: List[Token]) -> Tuple[Node, List[Token]]:
        result_node: Node = None
        # Check if token_list is empty. If so, return empty list
        if len(token_list) == 0:
            return None, token_list

        # Check if the first token is a semicolon or EOF. If so pop it and call this function recursively.
        elif token_list[0].type in [TokenType.SEMICOLON, TokenType.EOF]:
            return None, token_list[1:]

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
                value, token_list= self.get_next_node(token_list)
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
                    body, token_list= self.get_node_list(token_list)
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
                value, token_list = self.get_next_node(token_list)
                result_node = VariableAssignmentNode(variable_name, value)
            else:
                # VariableCallNode
                variable_name: str = token_list.pop(0).value
                result_node = VariableCallNode(variable_name)
        
        # Check if first token is a Conditional
        elif token_list[0].type == TokenType.IF:
            token_list.pop(0) # Remove TokenType.IF
            token_list.pop(0) # Remove TokenType.LEFTROUNDBRACKET
            condition, token_list = self.get_next_node(token_list)
            token_list.pop(0) # Remove TokenType.RIGHTROUNDBRACKET
            token_list.pop(0) # Remove TokenType.LEFTCURLYBRACKET
            body, token_list= self.get_node_list(token_list)
            token_list.pop(0) # Remove TokenType.RIGHTCURLYBRACKET
            result_node = ConditionalNode(condition, body)

        # Check if first token is a While
        elif token_list[0].type == TokenType.WHILE:
            token_list.pop(0) # Remove TokenType.WHILE
            token_list.pop(0) # Remove TokenType.LEFTROUNDBRACKET
            condition, token_list = self.get_next_node(token_list)
            token_list.pop(0) # Remove TokenType.RIGHTROUNDBRACKET
            token_list.pop(0) # Remove TokenType.LEFTCURLYBRACKET
            body, token_list= self.get_node_list(token_list)
            token_list.pop(0) # Remove TokenType.RIGHTCURLYBRACKET
            result_node = WhileNode(condition, body)

        # Check if first token is a return
        elif token_list[0].type == TokenType.RETURN:
            token_list.pop(0) # Remove TokenType.RETURN
            return_node, token_list = self.get_next_node(token_list)
            result_node = ReturnNode(return_node)

        # Check if first token is a print
        elif token_list[0].type == TokenType.PRINT:
            token_list.pop(0) # Remove TokenType.PRINT
            token_list.pop(0) # Remove TokenType.LEFTROUNDBRACKET
            print_node, token_list = self.get_next_node(token_list)
            result_node = PrintNode(print_node)
            token_list.pop(0) # Remove TokenType.RIGHTROUNDBRACKET

        # Check if the second token is an expression.
        if token_list[0].type in [TokenType.ADDIDION, TokenType.SUBTRACTION, TokenType.MULTIPLICATION, TokenType.DIVISION, TokenType.EQUALTO, TokenType.NOTEQUAL, TokenType.GREATERTHAN, TokenType.GREATERTHANOREQUALTO, TokenType.LESSTHAN, TokenType.LESSTHANOREQUALTO]:
            lhs = result_node
            conditional: Callable = token_list.pop(0).value
            rhs, token_list = self.get_next_node(token_list)
            result_node = ExpressionNode(lhs, conditional, rhs)

        if token_list[0].type == TokenType.SEMICOLON:
            token_list.pop(0)

        return result_node, token_list

    # Get node list from a token list
    # This recursive function ends when the first token is EOF or a RIGHTCURLYBRACKET
    # Returns a list of Nodes and the token_list
    def get_node_list(self, token_list: List[Token]) -> Tuple[List[Node], List[Token]]:
        result_node: Node = None
        result_node_list: List[Node] = []

        if token_list[0].type in [TokenType.RIGHTCURLYBRACKET, TokenType.EOF]:
            return [], token_list
        else:
            result_node, token_list = self.get_next_node(token_list)
            result_node_list, token_list = self.get_node_list(token_list)
            return [result_node] + result_node_list, token_list

if __name__ == '__main__':
    with open('dutchPlusPlusRecursive.txt', 'r') as file:
        data = file.read()

    # data = """
    # getal even(getal n, getal z);
    # """
  
    token_list: List[Token] = Lexer().get_token_list(data)

    # print(token_list)

    node_list, token_list = Parser().get_node_list(token_list)

    print("\nRESULT:")
    pprint(node_list)