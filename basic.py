##############################################################################################
# IMPORTS
##############################################################################################

import string
import math
from Lexer import Lexer, Error
from Parser import Parser
from Interpreter import Interpreter, SymbolTable, Context, Number

##############################################################################################
# RUN
##############################################################################################

global_symbol_table = SymbolTable()

# Predefined global variables
global_symbol_table.set("True", Number.true)
global_symbol_table.set("False", Number.false)
global_symbol_table.set("Null", Number.null)
global_symbol_table.set("PI", Number(math.pi))
global_symbol_table.set("TWO_PI", Number(math.pi*2))
global_symbol_table.set("HALF_PI", Number(math.pi/2))
global_symbol_table.set("Infinity", Number(math.inf))

def run(fn: str, text: str) -> (float, Error):
	# generate tokens from source with lexical analysis
	lexer = Lexer(fn, text)
	tokens, error = lexer.make_tokens()
	if error: return None, error

	# generate an abstract syntax tree by parsing the text, also know as syntax analysis
	parser = Parser(tokens)
	ast = parser.parse(global_symbol_table)
	if ast.error: return None, ast.error

	# interpret the ast
	interpreter = Interpreter()
	context = Context("<Program>")
	context.symbol_table = global_symbol_table
	result = interpreter.visit(ast.node, context)

	return result.value, result.error
