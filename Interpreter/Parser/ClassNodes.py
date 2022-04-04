import operator
from abc import ABC, abstractmethod
from typing import Callable, List, Union
from Interpreter.Lexer.ClassToken import Token, TokenData
from Runner.ClassLibrary import Library

class Node(ABC):
    '''Abstract class for all nodes in the AST.'''

    def __init__(self) -> None:
        super().__init__()
    
    def __str__(self) -> str:
        return self.__repr__()

    @abstractmethod
    def __repr__(self) -> str:
        return super().__repr__()

    @abstractmethod
    def run(self, library: Library):
        '''Runs the node and returns the result.'''
        return

class ValueNode(Node):
    '''Class for single values in the code'''
    def __init__(self, value: any) -> None:
        self.value = value
    
    def __repr__(self) -> str:
        return f"ValueNode(value: {self.value})"

    def run(self, library: Library):
        return self.value

class VariableDeclarationNode(Node):
    '''Class for variable declarations in the code'''
    def __init__(self, return_type: type, name: str) -> None:
        self.return_type = return_type
        self.name = name

    def __repr__(self) -> str:
        return f"VariableDeclationNode(return_type: {self.return_type}, name: {self.name})"

    def run(self, library: Library):
        if library.key_exists(self.name):
            #TODO: Error, variable declared somewhere else
            pass
        library[self.name] = None

class VariableDefinitionNode(Node):
    '''Class for variable definitions in the code'''
    def __init__(self, return_type: type, name: str, value: Node) -> None:
        self.return_type = return_type
        self.name = name
        self.value = value
        #TODO: Error when return_type is not the same type as value

    def __repr__(self) -> str:
        return f"VariableDefinitionNode(return_type: {self.return_type}, name: {self.name}, value: {self.value})"

    def run(self, library: Library):
        # if library.key_exists(self.name):
        #     #TODO: Error when value is declared but not with same type
        #     pass
        return_value = self.value.run(library)
        library[self.name] = return_value
        pass

class VariableAssignmentNode(Node):
    '''Class for variable assignments in the code'''
    def __init__(self, name: str, value: Node) -> None:
        self.name = name
        self.value = value

    def __repr__(self) -> str:
        return f"VariableAssignmentNode(name: {self.name}, value: {self.value})"

    def run(self, library: Library):
        if not library.key_exists(self.name):
            #TODO: Error, variable was never declared
            pass
        if type(library[self.name]) is not type(self.value):
            #TODO: Error, [type] cannot be assigned to [variable] with type [type]
            pass
        library[self.name] = self.value.run(library)
    
class VariableCallNode(Node):
    '''Class for variable calls in the code'''
    def __init__(self, name: str):
        self.name = name

    def __repr__(self) -> str:
        return f"VariableCallNode(name: {self.name})"

    def run(self, library: Library):
        if not self.name in library:
            #TODO: Error, variable was never declared
            pass
        return library[self.name]

class FunctionDeclarationNode(Node):
    '''Class for function declarations in the code'''
    def __init__(self, name: str, return_type: type, parameters: List[VariableDeclarationNode]) -> None:
        self.name = name
        self.return_type = return_type
        self.parameters = parameters

    def __repr__(self) -> str:
        return f"FunctionDeclarationNode(name: {self.name}, return_type: {self.return_type}, parameters: {self.parameters})"

    def run(self, library: Library):
        # if self.name in library:
        #     #TODO: Error, Function already declared somewhere else
        #     pass
        library.set_location(self.name)
        library['__return_type__'] = self.return_type
        library['__parameters__'] = self.parameters
        for parameter in self.parameters:
            library[parameter.name] = parameter.run(library)
        library.pop_location()


class FunctionDefinitionNode(Node):
    '''Class for function definitions in the code'''
    def __init__(self, name: str, return_type: type, parameters: List[VariableDeclarationNode], body: List[Node]) -> None:
        self.name = name
        self.return_type = return_type
        self.parameters = parameters
        self.body = body

    def __repr__(self) -> str:
        return f"FunctionDefinitionNode(name: {self.name}, return_type: {self.return_type}, parameters: {self.parameters}, body: {self.body})"

    def run(self, library: Library):
        library.set_location(self.name)
        library['__return_type__'] = self.return_type
        library['__parameters__'] = self.parameters
        for parameter in self.parameters:
            library[parameter.name] = parameter.run(library)
        library['__body__'] = self.body
        library.pop_location()

class FunctionCallNode(Node):
    '''Class for function calls in the code'''
    def __init__(self, name: str, parameters: List[Union[VariableCallNode, ValueNode]]) -> None:
        self.name = name
        self.parameters = parameters

    def __repr__(self) -> str:
        return f"FunctionCallNode(name: {self.name}, parameters: {self.parameters})"

    def run(self, library: Library):
        if not self.name in library:
            #TODO: Error, function is never declared
            pass

        parameter_values = []
        for parameter in self.parameters:
            parameter_values.append(parameter.run(library))

        library.set_location(self.name)

        parameter_name_value_zip = zip(library['__parameters__'], parameter_values)
        for parameter_name, parameter_value in parameter_name_value_zip:
            library[parameter_name.name] = parameter_value
        

        if not '__body__' in library:
            #TODO: Error, function was declared but never defined
            pass
        if not len(self.parameters) == len(library['__parameters__']):
            #TODO: Error, function call does not have all the required parameters
            pass
        for node in library['__body__']:
            node.run(library)
            if '__return_value__' in library:
                return_value = library['__return_value__']
                library.pop_location()
                return return_value
        library.pop_location()
        return None

class ExpressionNode(Node):
    '''Class for expressions in the code'''
    def __init__(self, lhs: Node, expression_type: Callable, rhs: Node):
        self.lhs = lhs
        self.expression_type = expression_type
        self.rhs = rhs

    def __repr__(self) -> str:
        return f"ExpressionNode(lhs: {self.lhs}, expression_type: {self.expression_type}, rhs: {self.rhs})"

    def run(self, library: Library):
        return self.expression_type(self.lhs.run(library), self.rhs.run(library))

class ConditionalNode(Node):
    '''Class for conditionals in the code'''
    def __init__(self, expression: ExpressionNode, body: List[Node]) -> None:
        self.expression = expression
        self.body = body
    
    def __repr__(self) -> str:
        return f"ConditionalNode(expression: {self.expression}, body: {self.body})"

    def run(self, library: Library):
        if self.expression.run(library):
            for node in self.body:
                node.run(library)

class ReturnNode(Node):
    '''Class for return statements in the code'''
    def __init__(self, value: Node):
        self.value = value

    def __repr__(self) -> str:
        return f"ReturnNode(value: {self.value})"

    def run(self, library: Library):
        if not '__return_type__' in library:
            pass
        if not type(library['__return_type__']) is type(self.value):
            pass
        library['__return_value__'] = self.value.run(library)

class WhileNode(Node):
    '''Class for while statements in the code'''
    def __init__(self, expression: ExpressionNode, body: List[Node]):
        self.expression = expression
        self.body = body

    def __repr__(self) -> str:
        return f"WhileNode(expression: {self.expression}, body: {self.body})"

    def run(self, library: Library):
        while self.expression.run(library):
            for node in self.body:
                node.run(library)

class PrintNode(Node):
    '''Class for print statements in the code'''
    def __init__(self, value: Node):
        self.value = value

    def __repr__(self) -> str:
        return f"PrintNode(value: {self.value})"

    def run(self, library: Library):
        print(self.value.run(library))