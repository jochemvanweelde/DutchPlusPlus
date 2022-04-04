from Interpreter.Parser.ClassParser import *
from Interpreter.Parser.ClassNodes import *
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

    token_list: List[Token] = get_token_list(recursive_string)

    node_list = Parser(token_list).parse(token_list)

    library_result = run_nodes(node_list)

