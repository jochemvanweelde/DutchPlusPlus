from typing import List

from Lexer.ClassLexer import Lexer
from Lexer.ClassToken import Token
from Parser.ClassNodes import Node
from Parser.ClassParser import Parser
from Runner.ClassLibrary import Library

# A function that gets an AST list of Nodes and runs them.
def run_nodes(node_list: List[Node], debug: bool = False) -> Library:
    library = Library()
    for node in node_list:
        if debug:
            print("Now running: " + str(node))
        node.run(library)
    return library

if __name__ == '__main__':        
    with open('dutchPlusPlusRecursive.txt', 'r') as file:
        recursive_string = file.read()

    with open('dutchPlusPlusLoopig.txt', 'r') as file:
        loopig_string = file.read()

    token_list: List[Token] = Lexer().get_token_list(loopig_string)

    node_list, token_list = Parser().get_node_list(token_list)

    library_result = run_nodes(node_list)

    print(library_result)

