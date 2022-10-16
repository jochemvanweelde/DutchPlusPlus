from typing import List
import sys, getopt

from Lexer.ClassLexer import Lexer
from Lexer.ClassToken import Token
from Parser.ClassNodes import Node
from Parser.ClassParser import Parser
from Runner.ClassLibrary import Library

# run_nodes :: List[Node] -> None
def run_nodes(node_list: List[Node]):
    # Run the nodes in the node_list
    library = Library()
    list(map(lambda node: node.run(library), node_list))
    return library

if __name__ == '__main__':        
    # Get the file name from the command line
    dutch_plus_plus_file = sys.argv[1]
    
    #Open file
    dutch_plus_plus_string = open(dutch_plus_plus_file, "r").read() 
    
    # Lexer
    token_list: List[Token] = Lexer().get_token_list(dutch_plus_plus_string)

    # Parser
    node_list, token_list = Parser().get_node_list(token_list)

    # Runner
    library_result = run_nodes(node_list)

    # print(library_result)

