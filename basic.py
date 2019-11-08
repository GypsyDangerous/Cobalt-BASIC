##############################################################################################
# IMPORTS
##############################################################################################

from string_with_arrows import *
import string
import math
from Token import *
from Errors import *
from Lexer import *
from Nodes import *
from Parser import *

##############################################################################################
# CONSTANTS
##############################################################################################


DIGITS = "0123456789."
from string import ascii_letters as LETTERS
LETTERS += "_:"
LETTERS_DIGITS = LETTERS + DIGITS
KEYWORDS = [
	"let",  
	'and', 
	'or', 
	'not', 
	"if", 
	"else", 
	"elif", 
	":",
	"for",
	"while",
	"to", 
	"step",
	"def"
]

class Position:
	def __init__(self, index, line_num, col_num, fn, ftxt):
		self.index = index
		self.line_num = line_num
		self.col_num = col_num
		self.fn = fn
		self.ftxt = ftxt

	def advance(self, current_char=None):
		self.index += 1
		self.col_num += 1
		if current_char == "\n":
			self.line_num+=1
			self.col_num = 0
		return self
	
	def copy(self):
		return Position(self.index, self.line_num, self.col_num, self.fn, self.ftxt)

	@property
	def idx(self):
		return self.index
	
	@property
	def ln(self):
		return self.line_num

	@property
	def col(self):
		return self.col_num


##############################################################################################
# CONTEXT
##############################################################################################


class Context:
	def __init__(self, display_name, parent=None, parent_entry_pos=None):
		self.display_name = display_name
		self.parent = parent
		self.parent_entry_pos = parent_entry_pos
		self.symbol_table = None


##############################################################################################
# SYMBOLTABLE
##############################################################################################


class SymbolTable:
	def __init__(self, parent = None):
		self.symbols = {}
		self.parent = parent
	
	def get(self, name):
		value = self.symbols.get(name, None)
		if value == None and self.parent:
			return self.parent.get(name)
		return value

	def set(self, name, value):
		self.symbols[name]=value

	def remove(self, name):
		del self.symbols[name]


##############################################################################################
# RUN
##############################################################################################


global_symbol_table = SymbolTable()

# Predefined global variable
global_symbol_table.set("True", Number(1))
global_symbol_table.set("False", Number(0))
global_symbol_table.set("Null", Number(0))
global_symbol_table.set("PI", Number(math.pi))
global_symbol_table.set("TWO_PI", Number(math.pi*2))
global_symbol_table.set("HALF_PI", Number(math.pi/2))

def run(fn: str, text: str) -> (float, Error):
	# generate tokens from source with lexical analysis
	lexer = Lexer(fn, text)
	tokens, error = lexer.make_tokens()
	if error: return None, error

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
