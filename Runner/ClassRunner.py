from typing import List

from Lexer.ClassLexer import Lexer
from Lexer.ClassToken import Token
from Parser.ClassNodes import Node
from Parser.ClassParser import Parser
from Runner.ClassLibrary import Library

# A recursive function that runs all the nodes in the AST
def run_nodes(node_list: List[Node], library: Library = Library(), debug: bool = False) -> Library:
    if len(node_list) == 0:
        return library
    if debug:
        print(f"Running node: {node_list[0]}")
    node_list[0].run(library)
    return run_nodes(node_list[1:], library, debug)

def run_nodes_two(node_list: List[Node]):
    library = Library()
    list(map(lambda node: node.run(library), node_list))
    return library

if __name__ == '__main__':        
    with open('dutchPlusPlusRecursive.txt', 'r') as file:
        recursive_string = file.read()

    with open('dutchPlusPlusLoopig.txt', 'r') as file:
        loopig_string = file.read()

    with open('dutchPlusPlusTest.txt', 'r') as file:
        test_string = file.read()

    token_list: List[Token] = Lexer().get_token_list(test_string)

    node_list, token_list = Parser().get_node_list(token_list)

    library_result = run_nodes_two(node_list)

    print(library_result)

