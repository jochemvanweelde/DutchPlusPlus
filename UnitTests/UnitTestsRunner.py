from typing import List
import unittest
from Lexer.ClassLexer import Lexer
from Lexer.ClassToken import Token
from Parser.ClassParser import Parser

from Runner.ClassLibrary import Library
import runner

      
class TestPrograms(unittest.TestCase):
    def test_program_looping(self):
        
        def python_program(n: int) -> int:
            result: int = 0
            while n >= 1:
                result += n
                n -= 1
            return result
        
        dutch_plus_plus_program = """
            getal sommig(getal n){
                getal resultaat is 0;
                zolang(n groterofgelijkaan 1){
                    resultaat is resultaat plus n;
                    n is n min 1;
                }
                geefterug resultaat;
            }

            getal a is sommig(X);"""
            
        for i in range(0, 100):
            dutch_plus_plus_program_with_i = dutch_plus_plus_program.replace("X", str(i))
            token_list: List[Token] = Lexer().get_token_list(dutch_plus_plus_program_with_i)
            node_list, token_list = Parser().get_node_list(token_list)
            library_result = runner.run_nodes(node_list)
            result_dutch_plus_plus_program = library_result.get_value("a")
            
            self.assertEqual(python_program(i), result_dutch_plus_plus_program)
            
    def test_program_recursive(self):
        
        def python_program(n: int) -> int:
            def oneven(n: int) -> int:
                if n == 0:
                    return False
                return even(n - 1)
            
            def even(n: int) -> int:
                if n == 0:
                    return True
                return oneven(n - 1)
            
            return oneven(n)
        
        dutch_plus_plus_program = """
            janee oneven(getal n);
            janee even(getal n);

            janee oneven(getal n) {
                als (n gelijkaan 0){
                    geefterug onwaar;
                }
                geefterug even(n min 1);
            }

            janee even(getal n) {
                als (n gelijkaan 0){
                    geefterug waar;
                }
                geefterug oneven(n min 1);
            }

            janee a is oneven(X);
            """
            
        for i in range(0, 100):
            dutch_plus_plus_program_with_i = dutch_plus_plus_program.replace("X", str(i))
            token_list: List[Token] = Lexer().get_token_list(dutch_plus_plus_program_with_i)
            node_list, token_list = Parser().get_node_list(token_list)
            library_result = runner.run_nodes(node_list)
            result_dutch_plus_plus_program = library_result.get_value("a")
            
            self.assertEqual(python_program(i), result_dutch_plus_plus_program)
            
class TestLibrary(unittest.TestCase):
    def test_library_set_location(self):
        lib = Library()
        lib.set_location("test")
        self.assertEqual(lib.location, ["", "test"])
        self.assertDictEqual(lib.library, {"test": {}})
        
    def test_library_pop(self):
        lib = Library()
        lib.set_value("test", "test")
        self.assertEqual(lib.pop("test"), "test")
        self.assertDictEqual(lib.library, {})
        
    def test_library_pop_location(self):
        lib = Library()
        lib.set_location("test")
        self.assertEqual(lib.pop_location(), "test")
        self.assertEqual(lib.location, [""])
        
    def test_library_get_location(self):
        lib = Library()
        lib.set_location("test")
        self.assertEqual(lib.get_location(), "test")
        
    def test_library_set_value(self):
        lib = Library()
        lib.set_value("test", "test")
        self.assertDictEqual(lib.library, {"test": "test"})
        
    def test_library_get_value(self):
        lib = Library()
        lib.set_value("test", "test")
        self.assertEqual(lib.get_value("test"), "test")
        self.assertRaises(KeyError, lib.get_value, "test2")
        
    def test_library_key_exists(self):
        lib = Library()
        lib.set_value("test", "test")
        self.assertTrue(lib.key_exists("test"))
        self.assertFalse(lib.key_exists("test2"))