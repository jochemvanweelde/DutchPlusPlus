from typing import List
from pprint import pprint, pformat
from Lexer.ClassLexer import Lexer

from Lexer.ClassToken import *
from Lexer.ClassLexerOld import *
from Parser.ClassNodes import *

class Parser:
    def __init__(self, token_list : List[Token]) -> None:
        # self.token_list: List[Token ]= token_list
        # self.token_list_count = 0
        # self.token_list_iter = iter(self.token_list)
        # self.ast: Node = []
        pass

    def parse(self, token_list: List[Token]) -> List[Node]:
        token_list_count = 0
        tokens_for_node: List[Token] = []
        result: Node = None

        left_bracket_count = 0
        right_bracket_count = 0

        if len(token_list) == 0:
            return None

        while True:
            tokens_for_node.append(token_list.pop(0))
            token_list_count += 1
            if tokens_for_node[-1].type in [TokenType.SEMICOLON] and (left_bracket_count + right_bracket_count) == 0:
                break
            if tokens_for_node[-1].type == TokenType.LEFTCURLYBRACKET:
                left_bracket_count += 1
            if tokens_for_node[-1].type == TokenType.RIGHTCURLYBRACKET:
                right_bracket_count += 1
            if left_bracket_count == right_bracket_count and left_bracket_count != 0:
                break
            if len(token_list) == 0:
                break

        #Check len < 2 for VariableCall- or ValueNode
        if len(tokens_for_node) <= 2:

            #VariableCallNode
            if tokens_for_node[0].type == TokenType.VARIABLE:
                result = VariableCallNode(tokens_for_node[0].value)

            #ValueNode
            elif tokens_for_node[0].type in [TokenType.DIGIT, TokenType.TRUE, TokenType.FALSE]:
                result = ValueNode(tokens_for_node[0].value)

            #TODO ERROR

        #ConditionalNode
        elif tokens_for_node[0].type in [TokenType.IF]: # TokenType.ELSEIF, TokenType.ELSE]:
            tokens_for_node = tokens_for_node[2:]
            expression_token_list: List[Token] = []
            while True:
                expression_token_list.append(tokens_for_node.pop(0))
                if expression_token_list[-1].type == TokenType.RIGHTROUNDBRACKET:
                    expression_token_list.pop()
                    break
            expression_node: ExpressionNode = self.parse(expression_token_list)[0]
            body: List[Token] = self.parse(tokens_for_node[1:-1])
            result = ConditionalNode(expression_node, body)
        
        #WhileNode
        elif tokens_for_node[0].type in [TokenType.WHILE]:
            tokens_for_node = tokens_for_node[2:]
            expression_token_list: List[Token] = []
            while True:
                expression_token_list.append(tokens_for_node.pop(0))
                if expression_token_list[-1].type == TokenType.RIGHTROUNDBRACKET:
                    expression_token_list.pop()
                    break
            expression_node: ExpressionNode = self.parse(expression_token_list)[0]
            body: List[Token] = self.parse(tokens_for_node[1:-1])
            result = WhileNode(expression_node, body)
        
        #ReturnNode
        elif tokens_for_node[0].type in [TokenType.RETURN]:
            result = ReturnNode(self.parse(tokens_for_node[1:])[0])

        #PrintNode
        elif tokens_for_node[0].type in [TokenType.PRINT]:
            to_print : List[Token] = tokens_for_node[2:-1]
            result = PrintNode(self.parse(to_print)[0])

        #FunctionDefinitionNode
        elif tokens_for_node[-1].type == TokenType.RIGHTCURLYBRACKET:
            return_type: type = tokens_for_node[0].value
            function_name: str = tokens_for_node[1].value
            tokens_for_node = tokens_for_node[3:]
            parameter_list: List[Token] = []
            while True:
                parameter_list.append(tokens_for_node.pop(0))
                if parameter_list[-1].type == TokenType.RIGHTROUNDBRACKET:
                    parameter_list.pop()
                    break
            for parameter in parameter_list:
                if parameter.type == TokenType.COMMA:
                    parameter.type = TokenType.SEMICOLON
            parameter_list.append(Token(TokenType.SEMICOLON, ";", None))
            parameter_nodes: List[Node] = self.parse(parameter_list)
            body: List[Token] = self.parse(tokens_for_node[1:-1])
            result = FunctionDefinitionNode(function_name, return_type, parameter_nodes, body)

        #ExpressionNode
        elif tokens_for_node[1].type in [TokenType.ADDIDION, TokenType.SUBTRACTION, TokenType.MULTIPLICATION, TokenType.DIVISION, TokenType.EQUALTO, TokenType.NOTEQUAL, TokenType.GREATERTHAN, TokenType.GREATERTHANOREQUALTO, TokenType.LESSTHAN, TokenType.LESSTHANOREQUALTO]:
            lhs: Node = self.parse(tokens_for_node[:1])[0]
            expression_type: Callable = tokens_for_node[1].value
            rhs: Node = self.parse(tokens_for_node[2:])[0]
            result = ExpressionNode(lhs, expression_type, rhs)
            pass

        #VariableAssignmentNode
        elif tokens_for_node[1].type == TokenType.IS:
            variable_name: str = tokens_for_node[0].value
            value: Node = self.parse(tokens_for_node[2:])[0]
            result = VariableAssignmentNode(variable_name, value)

        #VariableDefinitionNode
        elif tokens_for_node[2].type == TokenType.IS:
            return_type: type = tokens_for_node[0].value
            variable_name: str = tokens_for_node[1].value
            value: Node = self.parse(tokens_for_node[3:])[0]
            result = VariableDefinitionNode(return_type, variable_name, value)
        
        #Check if FunctionDeclarion- or FunctionCallNode
        elif [x for x in tokens_for_node if x.type == TokenType.LEFTROUNDBRACKET]:
            #FunctionDeclarationNode
            if tokens_for_node[0].type in [TokenType.BOOL, TokenType.INT, TokenType.FLOAT, TokenType.STRING]:
                return_type: type = tokens_for_node[0].value
                function_name: str = tokens_for_node[1].value
                tokens_for_node = tokens_for_node[3:]
                parameter_list: List[Token] = []
                while True:
                    parameter_list.append(tokens_for_node.pop(0))
                    if parameter_list[-1].type == TokenType.RIGHTROUNDBRACKET:
                        parameter_list.pop()
                        break
                for parameter in parameter_list:
                    if parameter.type == TokenType.COMMA:
                        parameter.type = TokenType.SEMICOLON
                parameter_list.append(Token(TokenType.SEMICOLON, ";", None))
                parameter_nodes: List[Node] = self.parse(parameter_list)
                result = FunctionDeclarationNode(function_name, return_type, parameter_nodes)

            #FunctionCallNode
            else:
                function_name: str = tokens_for_node[0].value
                tokens_for_node = tokens_for_node[2:]
                parameter_list: List[Token] = []
                while True:
                    parameter_list.append(tokens_for_node.pop(0))
                    if parameter_list[-1].type == TokenType.RIGHTROUNDBRACKET:
                        parameter_list.pop()
                        break
                for parameter in parameter_list:
                    if parameter.type == TokenType.COMMA:
                        parameter.type = TokenType.SEMICOLON
                parameter_list.append(Token(TokenType.SEMICOLON, ";", None))
                parameter_nodes: List[Node] = self.parse(parameter_list)
                result = FunctionCallNode(function_name, parameter_nodes)

        #VariableDeclarationNode
        elif tokens_for_node[0].type in [TokenType.BOOL, TokenType.INT, TokenType.FLOAT, TokenType.STRING] and tokens_for_node[1].type == TokenType.VARIABLE:
            variable_type: type = tokens_for_node[0].value
            variable_name: str = tokens_for_node[1].value
            result = VariableDeclarationNode(variable_type, variable_name)
        
        #Error
        else:
            #TODO: error
            print("ERRRROORRR!!!")
            pass
        
        #Return only one node if list is empty
        if len(token_list) == 0 or token_list[0].type == TokenType.EOF:
            return [result]
        #Call recursively if there are more tokens left
        return [result] + self.parse(token_list)


if __name__ == '__main__':      

    with open('dutchPlusPlusLoopig.txt', 'r') as file:
        data = file.read()
  
    token_list: List[Token] = Lexer().get_token_list(data)

    a = Parser(token_list).parse(token_list)

    print(a)
