from abc import ABC, abstractmethod
from typing import Callable, List, Tuple, Union
from Runner.ClassLibrary import Library

NODES_DEBUG = False

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

# Debug Decorator for nodes
def debug_decorator(func: Callable) -> Callable:
    """A debug decorator for the Run function of all nodes. This helps the user to see what node is running in what order.

    Args:
        func (Callable): The run function of a Node

    Returns:
        Callable: Returns the run function of a Node or the wrapper inside this function
    """
    def wrapper(*args, **kwargs):
        print(f"{bcolors.OKGREEN}Running node:{bcolors.ENDC} {args[0]}")
        return func(*args, **kwargs)
    if NODES_DEBUG:
        return wrapper
    return func

class Node(ABC):
    """Abstract class for all nodes in the AST.

    Args:
        ABC (_type_): Asbsract class inheritance
    """

    def __init__(self) -> None:
        super().__init__()
    
    def __str__(self) -> str:
        return self.__repr__()

    @abstractmethod
    def __repr__(self) -> str:
        return super().__repr__()

    @abstractmethod
    def run(self, library: Library) -> None:
        """Runs the node and excecute/run nested nodes

        Args:
            library (Library): The library for variable and function storage
        """
        return

class ValueNode(Node):
    '''Class for single values in the code'''
    def __init__(self, value: any) -> None:
        self.value = value
    
    def __repr__(self) -> str:
        return f"ValueNode(value: {self.value})"

    @debug_decorator
    def run(self, library: Library):
        return self.value

class VariableDeclarationNode(Node):
    '''Class for variable declarations in the code'''
    def __init__(self, return_type: type, name: str) -> None:
        self.return_type = return_type
        self.name = name

    def __repr__(self) -> str:
        return f"VariableDeclationNode(return_type: {self.return_type}, name: {self.name})"

    @debug_decorator
    def run(self, library: Library):
        if library.key_exists(self.name):
            raise Exception(f"Variable {self.name} declared somewhere else.")
        library[self.name] = None

class VariableDefinitionNode(Node):
    '''Class for variable definitions in the code'''
    def __init__(self, return_type: type, name: str, value: Node) -> None:
        self.return_type = return_type
        self.name = name
        self.value = value

        # TODO: Check if the value is of the correct type
        # if type(self.value) != self.return_type:
        #     raise Exception(f"Variable {self.name} is not of type {self.return_type}.\n Value is of type {type(self.value)}")

    def __repr__(self) -> str:
        return f"VariableDefinitionNode(return_type: {self.return_type}, name: {self.name}, value: {self.value})"

    @debug_decorator
    def run(self, library: Library):
        # if library.key_exists(self.name):
        #     #TODO: Error when value is declared but not with same type
        #     pass
        return_value = self.value.run(library)
        library[self.name] = return_value

class VariableAssignmentNode(Node):
    '''Class for variable assignments in the code'''
    def __init__(self, name: str, value: Node) -> None:
        self.name = name
        self.value = value

    def __repr__(self) -> str:
        return f"VariableAssignmentNode(name: {self.name}, value: {self.value})"

    @debug_decorator
    def run(self, library: Library):
        if not library.key_exists(self.name):
            raise Exception(f"Variable {self.name} is not declared.")
        # TODO: Check if the value is of the correct type
        # if type(library[self.name]) is not type(self.value):
        #     raise Exception(f"Variable {self.name} is not of type {type(self.value)}.")
        library[self.name] = self.value.run(library)
    
class VariableCallNode(Node):
    '''Class for variable calls in the code'''
    def __init__(self, name: str):
        self.name = name

    def __repr__(self) -> str:
        return f"VariableCallNode(name: {self.name})"

    @debug_decorator
    def run(self, library: Library):
        if not self.name in library:
            raise Exception(f"Variable {self.name} is not declared.")
        return library[self.name]

class FunctionDeclarationNode(Node):
    '''Class for function declarations in the code'''
    def __init__(self, name: str, return_type: type, parameters: List[VariableDeclarationNode]) -> None:
        self.name = name
        self.return_type = return_type
        self.parameters = parameters

    def __repr__(self) -> str:
        return f"FunctionDeclarationNode(name: {self.name}, return_type: {self.return_type}, parameters: {self.parameters})"

    @debug_decorator
    def run(self, library: Library):
        library.set_location(self.name) # Set the location of the library to the function
        library['__return_type__'] = self.return_type # Set the return type of the function
        library['__parameters__'] = self.parameters # Set the parameter nodes of the function
        library.pop_location() # Remove the location of the library to the function

class FunctionDefinitionNode(Node):
    '''Class for function definitions in the code'''
    def __init__(self, name: str, return_type: type, parameters: List[VariableDeclarationNode], body: List[Node]) -> None:
        self.name = name
        self.return_type = return_type
        self.parameters = parameters
        self.body = body

    def __repr__(self) -> str:
        return f"FunctionDefinitionNode(name: {self.name}, return_type: {self.return_type}, parameters: {self.parameters}, body: {self.body})"

    @debug_decorator
    def run(self, library: Library):
        # TODO: Check if the function is already defined
        
        library.set_location(self.name) # Set the location of the library to the function
        library['__return_type__'] = self.return_type # Set the return type of the function
        library['__parameters__'] = self.parameters # Set the parameter nodes of the function
        list(map(lambda x: library.set_value(x.name, x.run(library)), self.parameters)) # Fill the library with the parameters.
        library['__body__'] = self.body # Set the body of the function
        library.pop_location() # Remove the location of the library to the function

class FunctionCallNode(Node):
    '''Class for function calls in the code'''
    def __init__(self, name: str, parameters: List[Union[VariableCallNode, ValueNode]]) -> None:
        self.name = name
        self.parameters = parameters

    def __repr__(self) -> str:
        return f"FunctionCallNode(name: {self.name}, parameters: {self.parameters})"

    def run_body(self, body: List[Node], library: Library):
        if len(body) == 0: # If the body is empty, return None
            library.pop_location()
            return None
        body[0].run(library)
        if '__return_value__' in library: # If the function returns a value, return it
            return_value = library.pop('__return_value__')
            library.pop_location()
            return return_value
        return self.run_body(body[1:], library) # Otherwise, run the next line of the body

    @debug_decorator
    def run(self, library: Library):
        if not library.key_exists(self.name):
            raise Exception(f"Function {self.name} is not declared.")
        
        # Get the value of every parameter in this function call
        parameter_values = list(map(lambda x: x.run(library), self.parameters))
        # Set the location to this function call
        library.set_location(self.name)
        # Zip the names of the parameters with the corresponding values
        parameter_name_value_zip: Tuple[List[VariableDeclarationNode], any] = zip(library['__parameters__'], parameter_values)


        if not '__body__' in library:
            raise Exception(f"Function {self.name} is declared but never defined.")
        if not len(self.parameters) == len(library['__parameters__']):
            raise Exception(f"Function {self.name} is declared with {len(library['__parameters__'])} parameters but called with {len(self.parameters)} parameters.")
        # if not all(map(lambda x: type(x[1]) is type(x[0]), parameter_name_value_zip)):
        #     raise Exception(f"Function {self.name} is declared with parameters of type {type(library['__parameters__'][0])} but called with parameters of type {type(self.parameters[0])}.")
        # Set the values of the parameters in the library
        list(map(lambda x: library.set_value(x[0].name, x[1]), parameter_name_value_zip))

        return self.run_body(library['__body__'], library)

class ExpressionNode(Node):
    '''Class for expressions in the code'''
    def __init__(self, lhs: Node, expression_type: Callable, rhs: Node):
        self.lhs = lhs
        self.expression_type = expression_type
        self.rhs = rhs

    def __repr__(self) -> str:
        return f"ExpressionNode(lhs: {self.lhs}, expression_type: {self.expression_type}, rhs: {self.rhs})"

    @debug_decorator
    def run(self, library: Library):
        return self.expression_type(self.lhs.run(library), self.rhs.run(library)) # Run the expression

class ConditionalNode(Node):
    '''Class for conditionals in the code'''
    def __init__(self, expression: ExpressionNode, body: List[Node]) -> None:
        self.expression = expression
        self.body = body
    
    def __repr__(self) -> str:
        return f"ConditionalNode(expression: {self.expression}, body: {self.body})"

    @debug_decorator
    def run(self, library: Library):
        if self.expression.run(library): # If the expression is true
            list(map(lambda x: x.run(library), self.body)) # Run the body

class ReturnNode(Node):
    '''Class for return statements in the code'''
    def __init__(self, value: Node):
        self.value = value

    def __repr__(self) -> str:
        return f"ReturnNode(value: {self.value})"

    @debug_decorator
    def run(self, library: Library):
        if not '__return_type__' in library:
            pass
        if not type(library['__return_type__']) is type(self.value): 
            pass
        library['__return_value__'] = self.value.run(library) # Set the return value

class WhileNode(Node):
    '''Class for while statements in the code'''
    def __init__(self, expression: ExpressionNode, body: List[Node]):
        self.expression = expression
        self.body = body

    def __repr__(self) -> str:
        return f"WhileNode(expression: {self.expression}, body: {self.body})"

    @debug_decorator
    def run(self, library: Library):
        if self.expression.run(library): # If the expression is true
            list(map(lambda x: x.run(library), self.body)) # Run the body
            self.run(library) # Run this node again

class PrintNode(Node):
    '''Class for print statements in the code'''
    def __init__(self, value: Node):
        self.value = value

    def __repr__(self) -> str:
        return f"PrintNode(value: {self.value})"

    @debug_decorator
    def run(self, library: Library):
        print(self.value.run(library)) # Print the value