##############################################################################################
# IMPORTS
##############################################################################################

import string
import math
from Lexer import Lexer, Error
from Parser import Parser
import Interpreter
Interpreter, SymbolTable, Context, Number, NoneType, BuiltInFunction = Interpreter.Interpreter,  Interpreter.SymbolTable,  Interpreter.Context,  Interpreter.Number,  Interpreter.NoneType,  Interpreter.BuiltInFunction

##############################################################################################
# RUN
##############################################################################################

global_symbol_table = SymbolTable()

# Predefined global variables
global_symbol_table.set("True", Number.true)
global_symbol_table.set("False", Number.false)
global_symbol_table.set("Null", NoneType())
global_symbol_table.set("PI", Number(math.pi))
global_symbol_table.set("TWO_PI", Number(math.pi*2))
global_symbol_table.set("HALF_PI", Number(math.pi/2))
global_symbol_table.set("Infinity", Number(math.inf))
global_symbol_table.set("None", NoneType())
global_symbol_table.set("print", BuiltInFunction("print"))
global_symbol_table.set("input", BuiltInFunction("input"))
global_symbol_table.set("clear", BuiltInFunction("clear"))
global_symbol_table.set("int", BuiltInFunction("int"))
global_symbol_table.set("float", BuiltInFunction("float"))
global_symbol_table.set("list", BuiltInFunction("list"))
global_symbol_table.set("run", BuiltInFunction("run"))
global_symbol_table.set("len", BuiltInFunction("len"))


def run(fn: str, text: str) -> (float, Error):
	# generate tokens from source with lexical analysis
	lexer = Lexer(fn, text)
	tokens, error = lexer.make_tokens()
	if error: return None, error

	if len(tokens) <= 1:
		return None, error

	# generate an abstract syntax tree by parsing the text, also know as syntax analysis
	parser = Parser(tokens)
	ast = parser.parse()
	if ast.error: return None, ast.error

	# interpret the ast
	interpreter = Interpreter()
	context = Context("<Program>")
	context.symbol_table = global_symbol_table
	result = interpreter.visit(ast.node, context)

	return result.value, result.error
