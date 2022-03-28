import operator
from abc import ABC, abstractmethod
from typing import Callable, List, Union
from Lexer.ClassToken import Token, TokenData

class Node(ABC):
    def __init__(self, token_data: TokenData) -> None:
        super().__init__()
    
    def __str__(self) -> str:
        return self.__repr__()

    @abstractmethod
    def __repr__(self) -> str:
        return super().__repr__()

class ValueNode(Node):
    def __init__(self, value: any) -> None:
        self.value = value
    
    def __repr__(self) -> str:
        return f"ValueNode(value: {self.value})"

class VariableDeclarationNode(Node):
    def __init__(self, return_type: type, name: str) -> None:
        self.return_type = return_type
        self.name = name

    def __repr__(self) -> str:
        return f"VariableDeclationNode(return_type: {self.return_type}, name: {self.name})"

class VariableDefinitionNode(Node):
    def __init__(self, return_type: type, name: str, value: Node) -> None:
        self.return_type = return_type
        self.name = name
        self.value = value
        #TODO: Error when return_type is not the same type as value

    def __repr__(self) -> str:
        return f"VariableDefinitionNode(return_type: {self.return_type}, name: {self.name}, value: {self.value})"
    
class VariableCallNode(Node):
    def __init__(self, name: str):
        self.name = name

    def __repr__(self) -> str:
        return f"VariableCallNode(name: {self.name})"

class FunctionDeclarationNode(Node):
    def __init__(self, name: str, return_type: type, parameters: List[VariableDeclarationNode]) -> None:
        self.name = name
        self.return_type = return_type
        self.parameters = parameters

    def __repr__(self) -> str:
        return f"FunctionDeclarationNode(name: {self.name}, return_type: {self.return_type}, parameters: {self.parameters})"

class FunctionDefinitionNode(Node):
    def __init__(self, name: str, return_type: type, parameters: List[VariableDeclarationNode], *body: Node) -> None:
        self.name = name
        self.return_type = return_type
        self.parameters = parameters
        self.body = body

    def __repr__(self) -> str:
        return f"FunctionDefinitionNode(name: {self.name}, return_type: {self.return_type}, parameters: {self.parameters}, body: {self.body})"

class FunctionCallNode(Node):
    def __init__(self, name: str, parameters: List[Union[VariableCallNode, ValueNode]]) -> None:
        self.name = name
        self.parameters = parameters

    def __repr__(self) -> str:
        return f"FunctionCallNode(name: {self.name}, parameters: {self.parameters})"

class ExpressionNode(Node):
    def __init__(self, lhs: Node, expression_type: Callable, rhs: Node):
        self.lhs = lhs
        self.expression_type = expression_type
        self.rhs = rhs

    def value(self) -> Union[int, float, str, bool]:
        return self.expression_type(self.lhs, self.rhs)

    def __repr__(self) -> str:
        return f"ExpressionNode(lhs: {self.lhs}, expression_type: {self.expression_type}, rhs: {self.rhs})"

class ConditionalNode(Node):
    def __init__(self, expression: ExpressionNode, *body: Node) -> None:
        self.expression = expression
        self.body = body
    
    def __repr__(self) -> str:
        return f"ConditionalNode(expression: {self.expression}, body: {self.body})"

class ReturnNode(Node):
    def __init__(self, value: Node):
        self.value = value

    def __repr__(self) -> str:
        return f"ReturnNode(value: {self.value})"

class WhileNode(Node):
    def __init__(self, expression: ExpressionNode, *body: Node):
        self.expression = expression
        self.body = body

    def __repr__(self) -> str:
        return f"WhileNode(expression: {self.expression}, body: {self.body})"