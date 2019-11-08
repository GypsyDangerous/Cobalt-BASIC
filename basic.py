##############################################################################################
# IMPORTS
##############################################################################################

from string_with_arrows import *
import string
import math
from Token import *


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

##############################################################################################
# TOKEN CONSTANTS
##############################################################################################


TT_INT = "INT"
TT_FLOAT = "FLOAT"
TT_PLUS = "PLUS"
TT_IDENTIFER = "IDENTFIER" # identifier == variable name
TT_KEYWORD = "KEYWORD"
TT_MINUS = "MINUS"
TT_MUL = "MUL"
TT_DIV = "DIV"
TT_FDIV = "FDIV"
TT_EQ = "EQ"
TT_EE = "EE"
TT_NE = "NE"
TT_LT = "LT"
TT_GT = "GT"
TT_GTE = "GTE"
TT_LTE = "LTE"
TT_LPAREN = "LPAREN"
TT_RPAREN = "RPAREN"
TT_MOD = "MOD"
TT_EOF = "EOF"
TT_COMMA = "COMMA"
TT_ARROW = "ARROW"
TT_POW = "POW"

##############################################################################################
# ERRORS
##############################################################################################


# General Error
###############################################################################################
class Error:
	def __init__(self, pos_start, pos_end, error_name, msg):
		self.error_name = error_name
		self.pos_start = pos_start
		self.pos_end = pos_end
		self.msg = msg
	
	def as_string(self):
		return str(self)

	def __str__(self):
		return f"{self.error_name}: {self.msg}\nFile {self.pos_start.fn}, line {self.pos_start.line_num+1}" + '\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)

	def __repr__(self):
		return str(self)

# Illegal Character Error
###############################################################################################

class IllegalCharError(Error):
	def __init__(self, pos_start, pos_end, msg):
		super().__init__(pos_start, pos_end, "Illegal Character", msg)

# Invalid Syntax Error
###############################################################################################

class InvalidSyntaxError(Error):
	def __init__(self, pos_start, pos_end, msg=""):
		super().__init__(pos_start, pos_end, "Invalid Syntax", msg)

# Expected Character Error
###############################################################################################

class ExpectedCharError(Error):
	def __init__(self, pos_start, pos_end, msg=""):
		super().__init__(pos_start, pos_end, "Expected Character", msg)

# RunTime Error
###############################################################################################

class RunTimeError(Error):
	def __init__(self, pos_start, pos_end, msg, context):
		super().__init__(pos_start, pos_end, "Runtime Error", msg)
		self.context = context

	def as_string(self):
		return str(self)

	def __str__(self):
		result = self.generate_traceback()
		return result + f"{self.error_name}: {self.msg}" + '\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)

	def __repr__(self):
		return str(self)

	def generate_traceback(self):
		result = ""
		pos = self.pos_start
		ctx = self.context

		while ctx:
			result = f"  File {pos.fn}, line {str(pos.ln+1)}, in {ctx.display_name}\n" + result
			pos = ctx.parent_entry_pos
			ctx = ctx.parent
		return "Traceback (most recent call last):\n"+result



##############################################################################################
# POSITION
##############################################################################################


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
# LEXER
##############################################################################################


class Lexer:
	def __init__(self, fn, text):
		self.fn = fn
		self.text = text
		self.pos = Position(-1, 0, -1, fn, text)
		self.current_char = None
		self.advance()
	
	def advance(self):
		self.pos.advance(self.current_char)
		pos = self.pos.index
		self.current_char = self.text[pos] if pos < len(self.text) else None

	def make_tokens(self):
		tokens = []
		while self.current_char:
			if self.current_char in ' \t':
				self.advance() 
			elif self.current_char in LETTERS:
				tokens.append(self.make_identifier())
			elif self.current_char in DIGITS:
				tokens.append(self.make_number())
			elif self.current_char == "+":
				tokens.append(Token(TT_PLUS, pos_start = self.pos))
				self.advance()
			elif self.current_char == "-":
				tokens.append(self.make_minus_or_arrow())
			elif self.current_char == "*":
				tokens.append(self.make_double_token("*", TT_MUL, TT_POW))
			elif self.current_char == "/":
				tokens.append(self.make_double_token("/", TT_DIV, TT_FDIV))
			elif self.current_char == "(":
				tokens.append(Token(TT_LPAREN, pos_start = self.pos))
				self.advance()
			elif self.current_char == ")":
				tokens.append(Token(TT_RPAREN, pos_start = self.pos))
				self.advance()
			elif self.current_char == "%":
				tokens.append(Token(TT_MOD, pos_start = self.pos))
				self.advance()
			elif self.current_char == "!":
				token, error = self.make_not_equal()
				if error: return [], error
				tokens.append(token)
			elif self.current_char == "=":
				tokens.append(self.make_equals())
			elif self.current_char == "<":
				tokens.append(self.make_less_than())
			elif self.current_char == ">":
				tokens.append(self.make_greater_than())
			elif self.current_char == ",":
				tokens.append(Token(TT_COMMA, pos_start = self.pos))
				self.advance()
			else:
				pos_start = self.pos.copy()
				char = self.current_char
				self.advance()
				return [], IllegalCharError(pos_start, self.pos, f"'{char}' is not supported")


		tokens.append(Token(TT_EOF, pos_start = self.pos))

		return tokens, None

	def make_minus_or_arrow(self):
		return self.make_double_token(">", TT_MINUS, TT_ARROW)
	
	def make_double_token(self, double, tt_1, tt_2):
		return self.make_triple_token(double, double, tt_1, tt_2, tt_2)

	def make_triple_token(self, first, second, tt_1, tt_2, tt_3):
		token_type = tt_1
		pos_start = self.pos.copy()
		self.advance()
		if self.current_char == first:
			self.advance()
			token_type = tt_2
		elif self.current_char == second:
			self.advance()
			token_type = tt_3
		
		return Token(token_type, pos_start=pos_start, pos_end=self.pos)

	def make_greater_than(self):
		return self.make_double_token("=", TT_GT, TT_GTE)

	def make_less_than(self):
		return self.make_double_token("=", TT_LT, TT_LTE)

	def make_equals(self):
		return self.make_triple_token("=", ">", TT_EQ, TT_EE, TT_ARROW)

	def make_not_equal(self):
		pos_start = self.pos.copy()
		self.advance()
		
		if self.current_char == "=":
			self.advance()
			return Token(TT_NE, pos_start=pos_start, pos_end=self.pos), None
		
		self.advance()
		return None, ExpectedCharError(pos_start, self.pos, "'=' (after '!' mark)")

	def make_identifier(self):
		id_str = ""
		pos_start = self.pos.copy()
		colon_next = False
		while self.current_char != None and self.current_char in LETTERS_DIGITS:
			try:
				if self.text[self.pos.index+1] == ":":
					colon_next = True
				else:
					raise Exception()
			except:
				pass

			id_str+=self.current_char
			self.advance()
			if colon_next: break
				
		
		tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFER
		return Token(tok_type, id_str, pos_start, self.pos)


	def make_number(self):
		num_str = ""
		dot_count = 0
		pos_start = self.pos.copy()
		while self.current_char and self.current_char in DIGITS:
			if self.current_char == ".":
				if dot_count == 1: break
				dot_count+=1
				num_str += "."
			else:
				num_str += self.current_char
			self.advance()
		
		return Token(TT_INT, int(num_str), pos_start = pos_start, pos_end = self.pos) if dot_count == 0 else Token(TT_FLOAT, float(num_str), pos_start = pos_start, pos_end = self.pos)


##############################################################################################
# NODES
##############################################################################################

# Number Node
###############################################################################################

class NumberNode:
	def __init__(self, token):
		self.token = token
		self.pos_start = token.pos_start
		self.pos_end = token.pos_end
	
	def __str__(self):
		return str(self.token)

	def __repr__(self):
		return str(self)

	
# Binary operation Node
###############################################################################################

class BinOpNode:
	def __init__(self, left_node, op_token, right_node):
		self.left_node = left_node
		self.op_token = op_token
		self.right_node = right_node

		self.pos_start = left_node.pos_start
		self.pos_end = right_node.pos_end
	
	def __str__(self):
		return f"({self.left_node}, {self.op_token}, {self.right_node})"

	def __repr__(self):
		return str(self)

# Unary operation Node 
###############################################################################################

class UnaryOpNode:
	def __init__(self, op_token, node):
		self.op_token = op_token
		self.node = node

		self.pos_start = op_token.pos_start
		self.pos_end = op_token.pos_end

	def __str__(self):
		return f"({self.op_token}: {self.node})"
	
	def __repr__(self):
		return str(self)

# Variable Access Node
###############################################################################################

class VarAccessNode:
	def __init__(self, name):
		self.var_name_token = name
		self.pos_start = name.pos_start
		self.pos_end = name.pos_end

# Variable Assignment Node
###############################################################################################

class VarAssignNode:
	def __init__(self, name, value):
		self.var_name_token = name
		self.value_node = value
		self.pos_start = name.pos_start
		self.pos_end = value.pos_end

# If conditional Node
###############################################################################################

class IfNode:
	def __init__(self, cases, else_case):
		self.cases = cases
		self.else_case = else_case

		self.pos_start = self.cases[0][0].pos_start
		self.pos_end = (else_case or self.cases[-1][0]).pos_end

# For Loop Node
###############################################################################################

class ForNode:
	def __init__(self, var_name_token, start_value_node, end_value_node, step_value_node, body_node):
		self.var_name_token = var_name_token
		self.start_value_node = start_value_node
		self.end_value_node = end_value_node
		self.step_value_node = step_value_node
		self.body_node = body_node

		self.pos_start = self.var_name_token.pos_start
		self.pos_end = self.body_node.pos_end
		

# While Loop Node
###############################################################################################

class WhileNode:
	def __init__(self, condition_node, body_node):
		self.condition_node = condition_node
		self.body_node = body_node

		self.pos_start = condition_node.pos_start
		self.pos_end = body_node.pos_end


# Function Definition Node
###############################################################################################

class FuncDefNode:
	def __init__(self, var_name_token, arg_name_tokens, body_node):
		self.var_name_token = var_name_token
		self.arg_name_tokens = arg_name_tokens
		self.body_node = body_node

		if self.var_name_token:
			self.pos_start = self.var_name_token.pos_start
		elif len(arg_name_tokens) > 0:
			self.pos_start = self.arg_name_tokens[0].pos_start
		else:
			self.pos_start = self.body_node.pos_start
		
		self.pos_end = self.body_node.pos_end

class CallNode:
	def __init__(self, node_to_call, arg_nodes):
		self.node_to_call = node_to_call
		self.arg_nodes = arg_nodes

		self.pos_start = node_to_call.pos_start

		if len(self.arg_nodes) > 0:
			self.pos_end = self.arg_nodes[-1].pos_end
		else:
			self.pos_end = self.node_to_call.pos_end

		


##############################################################################################
# PARESE RESULT
##############################################################################################


class ParseResult:
	def __init__(self):
		self.error = None
		self.node = None
		self.advance_count = 0
	
	def register_advancement(self):
		self.advance_count+=1

	def register(self, res):
		self.advance_count += res.advance_count
		if res.error: self.error = res.error
		return res.node

	def success(self, node):
		self.node = node
		return self

	def failure(self, error):
		if not self.error or self.advance_count == 0:
			self.error = error
		return self


##############################################################################################
# PARSER
##############################################################################################


class Parser:
	def __init__(self, tokens):
		self.tokens = tokens
		self.token_index = -1
		self.current_token = None
		self.advance()

	def advance(self):
		self.token_index += 1
		if self.token_index < len(self.tokens):
			self.current_token = self.tokens[self.token_index]
		return self.current_token
	
###############################################################################################

	def parse(self):
		'''
		general parse method that calls all the other necessary methods
		based on the grammar rules and the current text to parse
		'''
		res = self.expr()
		if not res.error and self.current_token.type != TT_EOF:
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start, 
				self.current_token.pos_end, 
				"Expected '+', '-', '*', '/', '**', '==', '!=', '<', '>', <=', '>=', 'AND' or 'OR'"
			))
		return res
	
###############################################################################################

	def if_expr(self):
		'''
		method for parsing 'if-else' expressions
		'''
		res = ParseResult()

		cases = []
		else_case = None

		if not self.current_token.matches(TT_KEYWORD, "if"):
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start, 
				self.current_token.pos_end,
				"Expected 'if'"
			))

		res.register_advancement()
		self.advance()

		condition = res.register(self.expr())
		if res.error: return res

		if not self.current_token.matches(TT_KEYWORD, ":"):
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start, 
				self.current_token.pos_end,
				"Expected ':'"
			))

		res.register_advancement()
		self.advance()

		expr = res.register(self.expr())
		if res.error: return res
		cases.append((condition, expr))

		while self.current_token.matches(TT_KEYWORD, "elif"):
			res.register_advancement()
			self.advance()

			condition = res.register(self.expr())
			if res.error: return res

			if not self.current_token.matches(TT_KEYWORD, ":"):
				return res.failure(InvalidSyntaxError(
					self.current_token.pos_start, 
					self.current_token.pos_end,
					"Expected ':'"
				))
			
			res.register_advancement()
			self.advance()

			expr = res.register(self.expr())
			if res.error: return res
			cases.append((condition, expr))

		if self.current_token.matches(TT_KEYWORD, "else"):
			res.register_advancement()
			self.advance()

			if not self.current_token.matches(TT_KEYWORD, ":"):
				return res.failure(InvalidSyntaxError(
					self.current_token.pos_start, 
					self.current_token.pos_end,
					"Expected ':'"
				))
			
			res.register_advancement()
			self.advance()

			else_case = res.register(self.expr())
			if res.error: return res

		return res.success(IfNode(cases, else_case))

	def for_expr(self):
		'''
		method for parsing 'for loop' expressions
		'''
		res = ParseResult()

		if not self.current_token.matches(TT_KEYWORD, "for"):
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start, 
				self.current_token.pos_end,
				"Expected 'for'"
			))
		
		res.register_advancement()
		self.advance()

		if self.current_token.type != TT_IDENTIFER:
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start,
				self.current_token.pos_end,
				"Expected Identifier"
			))

		var_name = self.current_token
		res.register_advancement()
		self.advance()

		if self.current_token.type !=  TT_EQ:
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start,
				self.current_token.pos_end,
				"Expected '='"
			))

		res.register_advancement()
		self.advance()

		start_value = res.register(self.expr())
		if res.error: return res

		if not self.current_token.matches(TT_KEYWORD, "to"):
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start, 
				self.current_token.pos_end,
				"Expected 'to'"
			))

		res.register_advancement()
		self.advance()

		end_value = res.register(self.expr())
		if res.error: return res

		if self.current_token.matches(TT_KEYWORD, "step"):
			res.register_advancement()
			self.advance()

			step_value = res.register(self.expr())
			if res.error: return res
		else:
			step_value = None

		if not self.current_token.matches(TT_KEYWORD, ":"):
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start, 
				self.current_token.pos_end,
				"Expected ':'"
			))
		
		res.register_advancement()
		self.advance()

		body = res.register(self.expr())
		if res.error: return res

		return res.success(ForNode(var_name, start_value, end_value, step_value, body))


	def while_expr(self):
		res = ParseResult()

		if not self.current_token.matches(TT_KEYWORD, "while"):
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start, 
				self.current_token.pos_end,
				"Expected 'while'"
			))

		res.register_advancement()
		self.advance()

		condition = res.register(self.expr())
		if res.error: return res

		if not self.current_token.matches(TT_KEYWORD, ":"):
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start, 
				self.current_token.pos_end,
				"Expected ':'"
			))

		res.register_advancement()
		self.advance()

		body = res.register(self.expr())
		if res.error: return res

		return res.success(WhileNode(condition, body))
	
###############################################################################################

	def call(self):	
		res = ParseResult()
		atom = res.register(self.atom())
		if res.error: return res

		if self.current_token.type == TT_LPAREN:
			res.register_advancement()
			self.advance()
			arg_nodes = []

			if self.current_token.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
			else:
				arg_nodes.append(res.register(self.expr()))
				if res.error:
					return res.failure(InvalidSyntaxError(
						self.current_token.pos_start,
						self.current_token.pos_end,
						"Expected ')', 'let', 'if', 'for', 'while', 'def' int, float, identifier, '+', '-', or '('"
					))
				
				while self.current_token.type == TT_COMMA:
					res.register_advancement()
					self.advance()

					arg_nodes.append(res.register(self.expr()))
					if res.error: return res

				if self.current_token.type != TT_RPAREN:
					return res.failure(InvalidSyntaxError(
						self.current_token.pos_start,
						self.current_token.pos_end,
						"Expected ')' or ','"
					))

				res.register_advancement()
				self.advance()
			return res.success(CallNode(atom, arg_nodes))
		return res.success(atom)






###############################################################################################

	def atom(self):
		res = ParseResult()
		token = self.current_token

		if token.type in (TT_INT, TT_FLOAT):
			res.register_advancement()
			self.advance()
			return res.success(NumberNode(token))

		if token.type == TT_IDENTIFER:
			res.register_advancement()
			self.advance()
			return res.success(VarAccessNode(token))

		elif token.type == TT_LPAREN:
			res.register_advancement()
			self.advance()
			expr = res.register(self.expr())
			if res.error: return res
			if self.current_token.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
				return res.success(expr)
			else:
				return res.failure(InvalidSyntaxError(
					self.current_token.pos_start,
					self.current_token.pos_end,
					"Expected ')'"
				))

		elif token.matches(TT_KEYWORD, "if"):
			if_expr = res.register(self.if_expr())
			if res.error: return res
			return res.success(if_expr)

		elif token.matches(TT_KEYWORD, "for"):
			for_expr = res.register(self.for_expr())
			if res.error: return res
			return res.success(for_expr)

		elif token.matches(TT_KEYWORD, "while"):
			while_expr = res.register(self.while_expr())
			if res.error: return res
			return res.success(while_expr)

		elif token.matches(TT_KEYWORD, "def"):
			func_def = res.register(self.func_def())
			if res.error: return res
			return res.success(func_def)

		return res.failure(InvalidSyntaxError(
			self.current_token.pos_start,
			self.current_token.pos_end,
			"Expected 'let', 'if', 'for', 'while', 'def' int, float, identifier, '+', '-', or '('"
		))
	
###############################################################################################

	def power(self):
		return self.bin_op(self.call, (TT_POW, ), self.factor)
	
###############################################################################################

	def factor(self):
		res = ParseResult()
		token = self.current_token

		if token.type in (TT_PLUS, TT_MINUS):
			res.register_advancement()
			self.advance()
			factor = res.register(self.factor())
			if res.error: return res
			return res.success(UnaryOpNode(token, factor))
		
		return self.power()

###############################################################################################

	def term(self):
		return self.bin_op(self.factor, (TT_MUL, TT_DIV, TT_MOD, TT_FDIV))

###############################################################################################

	def arith_expr(self):
		return self.bin_op(self.term, (TT_PLUS, TT_MINUS))
	
###############################################################################################

	def comp_expr(self):
		res = ParseResult()
		
		if self.current_token.matches(TT_KEYWORD, "not"):
			op_token = self.current_token
			res.register_advancement()
			self.advance()

			node = res.register(self.comp_expr())
			if res.error: return res
			return res.success(UnaryOpNode(op_token, node))
		
		node = res.register(self.bin_op(self.arith_expr, (TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE)))
		if res.error:
			return res.failure(InvalidSyntaxError(
			self.current_token.pos_start,
			self.current_token.pos_end,
			"Expected int, float, variable identifier, '+', '-', '(', 'not'"
		))
		return res.success(node)

###############################################################################################
	
	def expr(self):
		res = ParseResult()

		if self.current_token.matches(TT_KEYWORD, "let"):
			res.register_advancement()
			self.advance()
			if self.current_token.type != TT_IDENTIFER:
				return res.failure(InvalidSyntaxError(
					self.current_token.pos_start,
					self.current_token.pos_end,
					"Expected variable identifier"
				))

			var_name = self.current_token
			res.register_advancement()
			self.advance()

			if self.current_token.type != TT_EQ:
				return res.failure(InvalidSyntaxError(
					self.current_token.pos_start,
					self.current_token.pos_end,
					"Expected variable assignment key '='"
				))

			res.register_advancement()
			self.advance()
			expr = res.register(self.expr())
			if res.error: return res
			return res.success(VarAssignNode(var_name, expr))

		node = res.register(self.bin_op(self.comp_expr, ((TT_KEYWORD, "and"), (TT_KEYWORD, "or"))))

		if res.error: 
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start,
				self.current_token.pos_end,
				"Expected 'let', int, float, identifier, '+', '-', or '('"
			))
		return res.success(node)

###############################################################################################

	def func_def(self):
		res = ParseResult()

		if not self.current_token.matches(TT_KEYWORD, "def"):
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start, 
				self.current_token.pos_end,
				"Expected 'def'"
			))

		res.register_advancement()
		self.advance()

		if self.current_token.type == TT_IDENTIFER:
			var_name_token = self.current_token
			res.register_advancement()
			self.advance()
			if self.current_token.type != TT_LPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_token.pos_start,
					self.current_token.pos_end,
					"Expected '('"
				))
		else:
			var_name_token = None
			if self.current_token.type != TT_LPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_token.pos_start,
					self.current_token.pos_end,
					"Expected identfier or '('"
				))
		
		res.register_advancement()
		self.advance()

		arg_name_tokens = []

		if self.current_token.type == TT_IDENTIFER:
			arg_name_tokens.append(self.current_token)
			res.register_advancement()
			self.advance()

			while self.current_token.type == TT_COMMA:
				res.register_advancement()
				self.advance()

				if self.current_token.type != TT_IDENTIFER:
					return res.failure(InvalidSyntaxError(
						self.current_token.pos_start,
						self.current_token.pos_end,
						"Expected identfier"
					))
				
				arg_name_tokens.append(self.current_token)
				res.register_advancement()
				self.advance()

			if self.current_token.type != TT_RPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_token.pos_start,
					self.current_token.pos_end,
					"Expected ',' or ')'"
				))
		else:
			if self.current_token.type != TT_RPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_token.pos_start,
					self.current_token.pos_end,
					"Expected 'identifier' or ')'"
				))

		res.register_advancement()
		self.advance()

		if self.current_token.type != TT_ARROW:
				return res.failure(InvalidSyntaxError(
					self.current_token.pos_start,
					self.current_token.pos_end,
					"Expected '->'"
				))
			
		res.register_advancement()
		self.advance()

		node_to_return = res.register(self.expr())
		if res.error: return res

		return res.success(FuncDefNode(
			var_name_token,
			arg_name_tokens,
			node_to_return
		))







###############################################################################################

	def bin_op(self, func, ops, func_b=None):
		func_b = func_b or func
		res = ParseResult()
		left = res.register(func())
		if res.error: return res

		while self.current_token.type in ops or (self.current_token.type, self.current_token.value) in ops:
			op_token = self.current_token
			res.register_advancement()
			self.advance()
			right = res.register(func_b())
			if res.error: return res
			left = BinOpNode(left, op_token, right)

		return res.success(left)


##############################################################################################
# RUNTIME RESULT
##############################################################################################


class RTResult:
	def __init__(self):
		self.value = None
		self.error = None

	def register(self, res):
		if res.error: self.error = res.error
		return res.value
	
	def success(self, val):
		self.value = val
		return self

	def failure(self, error):
		self.error = error
		return self


##############################################################################################
# VALUES
##############################################################################################

class Value:
	def __init__(self):
		self.set_pos()
		self.set_context()
	
	def set_pos(self, pos_start=None, pos_end=None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self

	def set_context(self, context=None):
		self.context = context
		return self

	def added_to(self, other):
		return None, self.illegal_operation(other)
	
	def subbed_by(self, other):
		return None, self.illegal_operation(other)

	def multed_by(self, other):
		return None, self.illegal_operation(other)

	def dived_by(self, other):
		return None, self.illegal_operation(other)

	def raised_to(self, other):
		return None, self.illegal_operation(other)

	def modded_to(self, other):
		return None, self.illegal_operation(other)
	
	def floor_dived_to(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_eq(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_ne(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_lt(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_gt(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_lte(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_gte(self, other):
		return None, self.illegal_operation(other)

	def anded_by(self, other):
		return None, self.illegal_operation(other)

	def ored_by(self, other):
		return None, self.illegal_operation(other)
		
	def notted(self):
		return None, self.illegal_operation(self)

	def copy(self):
		raise Exception('No copy method defined')

	def execute(self, args):
		return RTResult().failure(self.illegal_operation())

	def is_true(self):
		return False

	def illegal_operation(self, other=None):
		if not other: other = self
		return RunTimeError(
			self.pos_start, other.pos_end,
			'Illegal operation',
			self.context
		)


class Number(Value):
	def __init__(self, value):
		super().__init__()
		self.value = value

	# Binary Operations 
###############################################################################################

	def added_to(self, other):
		if isinstance(other, Number):
			return Number(self.value + other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)
	
	def subbed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value - other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def multed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value * other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def dived_by(self, other):
		if isinstance(other, Number):
			if other.value == 0:
				return None, RunTimeError(self.pos_start, self.pos_end, "Division By Zero", self.context)
			else:
				return Number(self.value / other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def raised_to(self, other):
		if isinstance(other, Number):
			return Number(self.value**other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def modded_to(self, other):
		if isinstance(other, Number):
			return Number(self.value%other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)
	
	def floor_dived_to(self, other):
		if isinstance(other, Number):
			if other.value == 0:
				return None, RunTimeError(self.pos_start, self.pos_end, "Division By Zero", self.context)
			else:
				return Number(self.value // other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_eq(self, other):
		if isinstance(other, Number):
			return Number(int(self.value == other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_ne(self, other):
		if isinstance(other, Number):
			return Number(int(self.value != other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_lt(self, other):
		if isinstance(other, Number):
			return Number(int(self.value < other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_gt(self, other):
		if isinstance(other, Number):
			return Number(int(self.value > other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_lte(self, other):
		if isinstance(other, Number):
			return Number(int(self.value <= other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_gte(self, other):
		if isinstance(other, Number):
			return Number(int(self.value >= other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def anded_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.value and other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def ored_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.value or other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

# Unary Operations 
###############################################################################################
		
	def notted(self):
		return Number(1 if self.value == 0 else 0).set_context(self.context), None

	def copy(self):
		copy = Number(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy

	def __str__(self):
		return str(self.value)

	def __repr__(self):
		return str(self)

	def is_true(self):
		return self.value != 0


class Function(Value):
	def __init__(self, name, body_node, arg_names):
		super().__init__()
		self.name = name or "<anonymous>"
		self.body_node = body_node
		self.arg_names = arg_names

	def execute(self, args):
		res = RTResult()
		interpreter = Interpreter()
		new_context = Context(self.name, self.context, self.pos_start)
		new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)

		if len(args) > len(self.arg_names):
			return res.failure(RunTimeError(
				self.pos_start, self.pos_end,
				f"{len(args) - len(self.arg_names)} too many args passed into '{self.name}'",
				self.context
			))
		
		if len(args) < len(self.arg_names):
			return res.failure(RunTimeError(
				self.pos_start, self.pos_end,
				f"{len(self.arg_names) - len(args)} too few args passed into '{self.name}'",
				self.context
			))

		for i in range(len(args)):
			arg_name = self.arg_names[i]
			arg_value = args[i]
			arg_value.set_context(new_context)
			new_context.symbol_table.set(arg_name, arg_value)

		value = res.register(interpreter.visit(self.body_node, new_context))
		if res.error: return res
		return res.success(value)

	def copy(self):
		copy = Function(self.name, self.body_node, self.arg_names)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		return copy

	def __str__(self):
		return f"<function {self.name}>"

	def __repr__(self):
		return str(self)


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
# INTERPRETER
##############################################################################################


class Interpreter:
	def visit(self, node, context):
		method_name = f"visit_{type(node).__name__}"
		method = getattr(self, method_name, self.no_visit_method)
		return method(node, context)

	def no_visit_method(self, node, context):
		raise Exception(f"No visit_{type(node).__name__} method defined")

	def visit_NumberNode(self, node, context):
		return RTResult().success(Number(node.token.value).set_context(context).set_pos(node.pos_start, node.pos_end))

	def visit_BinOpNode(self, node, context):
		res = RTResult()
		left = res.register(self.visit(node.left_node, context))
		if res.error: return res
		error = None
		right = res.register(self.visit(node.right_node, context))
		if res.error: return res
		if node.op_token.type == TT_PLUS:
			result, error = left.added_to(right)
		elif node.op_token.type == TT_MINUS:
			result, error = left.subbed_by(right)
		elif node.op_token.type == TT_DIV:
			result, error = left.dived_by(right)
		elif node.op_token.type == TT_MUL:
			result, error = left.multed_by(right)
		elif node.op_token.type == TT_POW:
			result, error = left.raised_to(right)
		elif node.op_token.type == TT_MOD:
			result, error = left.modded_to(right)
		elif node.op_token.type == TT_FDIV:
			result, error = left.floor_dived_to(right)
		elif node.op_token.type == TT_EE:
			result, error = left.get_comparison_eq(right)
		elif node.op_token.type == TT_NE:
			result, error = left.get_comparison_ne(right)
		elif node.op_token.type == TT_LT:
			result, error = left.get_comparison_lt(right)
		elif node.op_token.type == TT_GT:
			result, error = left.get_comparison_gt(right)
		elif node.op_token.type == TT_LTE:
			result, error = left.get_comparison_lte(right)
		elif node.op_token.type == TT_GTE:
			result, error = left.get_comparison_gte(right)
		elif node.op_token.matches(TT_KEYWORD, 'and'):
			result, error = left.anded_by(right)
		elif node.op_token.matches(TT_KEYWORD, 'or'):
			result, error = left.ored_by(right)

		if error:
			return res.failure(error)
		else:
			return res.success(result.set_pos(node.pos_start, node.pos_end))

	def visit_UnaryOpNode(self, node, context):
		res = RTResult()
		num = res.register(self.visit(node.node, context))
		if res.error: return res
		error = None
		if node.op_token.type == TT_MINUS:
			num, error = num.multed_by(Number(-1))
		elif node.op_token.matches(TT_KEYWORD, 'not'):
			num, error = num.notted()
		
		if error:
			return res.failure(error)
		else:
			return res.success(num.set_pos(node.pos_start, node.pos_end))

	def visit_VarAssignNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_token.value
		value = res.register(self.visit(node.value_node, context))
		if res.error: return res
		
		context.symbol_table.set(var_name, value)
		return res.success(value)

	def visit_VarAccessNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_token.value
		value = context.symbol_table.get(var_name)

		if value == None:
			return res.failure(RunTimeError(
				node.pos_start,
				node.pos_end,
				f"'{var_name}' is not defined",
				context
			))
		
		value = value.copy().set_pos(node.pos_start, node.pos_end)
		return res.success(value)

	def visit_IfNode(self, node, context):
		res = RTResult()

		for condition, expr in node.cases:
			condition_value = res.register(self.visit(condition, context))
			if res.error: return res 
			
			if condition_value.is_true():
				expr_value = res.register(self.visit(expr, context))
				if res.error: return res
				return res.success(expr_value)

		if node.else_case:
			else_value = res.register(self.visit(node.else_case, context))
			if res.error: return res
			return res.success(else_value)

		return res.success(None)

	def visit_ForNode(self, node, context):
		res = RTResult()

		start_value = res.register(self.visit(node.start_value_node, context))
		if res.error: return res

		end_value = res.register(self.visit(node.end_value_node, context))
		if res.error: return res

		if node.step_value_node:
			step_value = res.register(self.visit(node.step_value_node, context))
			if res.error: return res
		else:
			step_value = Number(1)

		i = start_value.value

		if step_value.value >= 0:
			condition = lambda: i < end_value.value
		else:
			condition = lambda: i > end_value.value

		result = None

		while condition():
			context.symbol_table.set(node.var_name_token.value, Number(i))
			i += step_value.value

			result = res.register(self.visit(node.body_node, context))
			if res.error: return res

		return res.success(result)



	def visit_WhileNode(self, node, context):
		res = RTResult()
		result = None
		while True:
			condition = res.register(self.visit(node.condition_node, context))
			if res.error: return res

			if not condition.is_true(): break

			result = res.register(self.visit(node.body_node, context))
			if res.error: return res

		return result.success(result)

	def visit_FuncDefNode(self, node, context):
		res = RTResult()

		func_name = node.var_name_token.value if node.var_name_token else None
		body_node = node.body_node
		arg_names = [arg_name.value for arg_name in node.arg_name_tokens]
		func_value = Function(func_name, body_node, arg_names).set_context(context).set_pos(node.pos_start, node.pos_end)

		if node.var_name_token:
			context.symbol_table.set(func_name, func_value)

		return res.success(func_value)


	def visit_CallNode(self, node, context):
		res = RTResult()

		args = []
		value_to_call = res.register(self.visit(node.node_to_call, context))
		if res.error: return res
		value_to_call = value_to_call.copy().set_pos(node.pos_start, node.pos_end)

		for arg_node in node.arg_nodes:
			args.append(res.register(self.visit(arg_node, context)))
			if res.error: return res

		return_value = res.register(value_to_call.execute(args))
		if res.error: return res
		return res.success(return_value)

		


##############################################################################################
# RUN
##############################################################################################


global_symbol_table = SymbolTable()

# Predefined global variable
global_symbol_table.set("True", Number(1))
global_symbol_table.set("False", Number(0))
global_symbol_table.set("Null", Number(0))
global_symbol_table.set("Pi", Number(math.pi))

def run(fn: str, text: str) -> (float, Error):
	# generate tokens from source with lexical analysis
	lexer = Lexer(fn, text)
	tokens, error = lexer.make_tokens()
	if error: return None, error

	# generate an abstract syntax tree through syntax analysis, also known as parsing the tokens
	parser = Parser(tokens)
	ast = parser.parse()
	if ast.error: return None, ast.error

	# interpret the abs
	interpreter = Interpreter()
	context = Context("<Program>")
	context.symbol_table = global_symbol_table
	result = interpreter.visit(ast.node, context)

	return result.value, result.error
