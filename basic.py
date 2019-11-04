##############################################################################################
# CONSTANTS
##############################################################################################

from string_with_arrows import *
import string
import math

##############################################################################################
# CONSTANTS
##############################################################################################


DIGITS = "0123456789."
from string import ascii_letters as LETTERS
LETTERS += "_:"
LETTERS_DIGITS = LETTERS + DIGITS
KEYWORDS = [
	"let",  'and', 'or', 'not', "if", "else", "elif", ":"
]

##############################################################################################
# TOKEN CONSTANTS
##############################################################################################


TT_INT = "INT"
TT_FLOAT = "FLOAT"
TT_PLUS = "PLUS"
TT_IDENTIFER = "IDENTFIER"
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
TT_POW = "POW"


##############################################################################################
# TOKEN
##############################################################################################


class Token:
	def __init__(self, type, val=None, pos_start = None, pos_end = None):
		self.type = type
		self.value = val
		if pos_start:
			self.pos_start = pos_start.copy()
			self.pos_end = pos_start.copy()
			self.pos_end.advance()

		if pos_end:
			self.pos_end = pos_end

	def matches(self, type, value):
		return self.type == type and self.value == value

	def __str__(self):
		return f"{self.type}: {self.value}" if self.value else f"{self.type}"

	def __repr__(self):
		return str(self)


##############################################################################################
# ERRORS
##############################################################################################


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

###############################################################################################

class IllegalCharError(Error):
	def __init__(self, pos_start, pos_end, msg):
		super().__init__(pos_start, pos_end, "Illegal Character", msg)

###############################################################################################

class InvalidSyntaxError(Error):
	def __init__(self, pos_start, pos_end, msg=""):
		super().__init__(pos_start, pos_end, "Invalid Syntax", msg)

###############################################################################################

class ExpectedCharError(Error):
	def __init__(self, pos_start, pos_end, msg=""):
		super().__init__(pos_start, pos_end, "Expected Character", msg)

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
				tokens.append(Token(TT_MINUS, pos_start = self.pos))
				self.advance()
			elif self.current_char == "*":
				try:
					if self.current_char+self.text[self.pos.index+1] == "**":
						self.advance()
						tokens.append(Token(TT_POW, pos_start = self.pos))
					else:
						raise Exception()
				except:
					tokens.append(Token(TT_MUL, pos_start = self.pos))
				self.advance()
			elif self.current_char == "/":
				try:
					if self.current_char+self.text[self.pos.index+1] == "//":
						self.advance()
						tokens.append(Token(TT_FDIV, pos_start = self.pos))
					else:
						raise Exception()
				except:
					tokens.append(Token(TT_DIV, pos_start = self.pos))
				self.advance()
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
			else:
				pos_start = self.pos.copy()
				char = self.current_char
				self.advance()
				return [], IllegalCharError(pos_start, self.pos, f"'{char}' is not supported")


		tokens.append(Token(TT_EOF, pos_start = self.pos))

		return tokens, None

	def make_greater_than(self):
		tok_type = TT_GT
		pos_start = self.pos.copy()
		self.advance()
		
		if self.current_char == '=':
			self.advance()
			tok_type = TT_GTE
		
		return Token(tok_type, pos_start = pos_start, pos_end=self.pos)

	def make_less_than(self):
		tok_type = TT_LT
		pos_start = self.pos.copy()
		self.advance()
		
		if self.current_char == '=':
			self.advance()
			tok_type = TT_LTE
		
		return Token(tok_type, pos_start = pos_start, pos_end=self.pos)

	def make_equals(self):
		tok_type = TT_EQ
		pos_start = self.pos.copy()
		self.advance()
		
		if self.current_char == '=':
			self.advance()
			tok_type = TT_EE
		
		return Token(tok_type, pos_start = pos_start, pos_end=self.pos)

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

		while self.current_char != None and self.current_char in LETTERS_DIGITS:
			id_str+=self.current_char
			self.advance()
			if id_str in KEYWORDS:
				tok_type = TT_KEYWORD
				return Token(tok_type, id_str, pos_start, self.pos)
		
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


class NumberNode:
	def __init__(self, token):
		self.token = token
		self.pos_start = token.pos_start
		self.pos_end = token.pos_end
	
	def __str__(self):
		return str(self.token)

	def __repr__(self):
		return str(self)

###############################################################################################

class BinOpNode:
	def __init__(self, left_node, op_tok, right_node):
		self.left_node = left_node
		self.op_tok = op_tok
		self.right_node = right_node

		self.pos_start = left_node.pos_start
		self.pos_end = right_node.pos_end
	
	def __str__(self):
		return f"({self.left_node}, {self.op_tok}, {self.right_node})"

	def __repr__(self):
		return str(self)

###############################################################################################

class UnaryOpNode:
	def __init__(self, op_tok, node):
		self.op_tok = op_tok
		self.node = node

		self.pos_start = op_tok.pos_start
		self.pos_end = op_tok.pos_end

	def __str__(self):
		return f"({self.op_tok}: {self.node})"
	
	def __repr__(self):
		return str(self)

###############################################################################################

class VarAccessNode:
	def __init__(self, name):
		self.var_name_tok = name
		self.pos_start = name.pos_start
		self.pos_end = name.pos_end

###############################################################################################

class VarAssignNode:
	def __init__(self, name, value):
		self.var_name_tok = name
		self.value_node = value
		self.pos_start = name.pos_start
		self.pos_end = value.pos_end

###############################################################################################

class IfNode:
	def __init__(self, cases, else_case):
		self.cases = cases
		self.else_case = else_case

		self.pos_start = self.cases[0][0].pos_start
		self.pos_end = (else_case or self.cases[-1][0]).pos_end


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

	def parse(self):
		res = self.expr()
		if not res.error and self.current_token.type != TT_EOF:
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start, 
				self.current_token.pos_end, 
				"Expected '+', '-', '*', '/', '**', '==', '!=', '<', '>', <=', '>=', 'AND' or 'OR'"
			))
		return res

	def if_expr(self):
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

	def atom(self):
		res = ParseResult()
		tok = self.current_token

		if tok.type in (TT_INT, TT_FLOAT):
			res.register_advancement()
			self.advance()
			return res.success(NumberNode(tok))

		if tok.type == TT_IDENTIFER:
			res.register_advancement()
			self.advance()
			return res.success(VarAccessNode(tok))

		elif tok.type == TT_LPAREN:
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

		elif tok.matches(TT_KEYWORD, "if"):
			if_expr = res.register(self.if_expr())
			if res.error: return res
			return res.success(if_expr)

		return res.failure(InvalidSyntaxError(
			self.current_token.pos_start,
			self.current_token.pos_end,
			"Expected int, float, variable identifier, '+', '-' or ')'"
		))

	def power(self):
		return self.bin_op(self.atom, (TT_POW, ), self.factor)

	def factor(self):
		res = ParseResult()
		tok = self.current_token

		if tok.type in (TT_PLUS, TT_MINUS):
			res.register_advancement()
			self.advance()
			factor = res.register(self.factor())
			if res.error: return res
			return res.success(UnaryOpNode(tok, factor))
		
		return self.power()

	def term(self):
		return self.bin_op(self.factor, (TT_MUL, TT_DIV, TT_MOD, TT_FDIV))

	def arith_expr(self):
		return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

	def comp_expr(self):
		res = ParseResult()
		
		if self.current_token.matches(TT_KEYWORD, "not"):
			op_tok = self.current_token
			res.register_advancement()
			self.advance()

			node = res.register(self.comp_expr())
			if res.error: return res
			return res.success(UnaryOpNode(op_tok, node))
		
		node = res.register(self.bin_op(self.arith_expr, (TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE)))
		if res.error:
			return res.failure(InvalidSyntaxError(
			self.current_token.pos_start,
			self.current_token.pos_end,
			"Expected int, float, variable identifier, '+', '-', '(', 'not'"
		))
		return res.success(node)


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

	def bin_op(self, func, ops, func_b=None):
		func_b = func_b or func
		res = ParseResult()
		left = res.register(func())
		if res.error: return res

		while self.current_token.type in ops or (self.current_token.type, self.current_token.value) in ops:
			op_tok = self.current_token
			res.register_advancement()
			self.advance()
			right = res.register(func_b())
			if res.error: return res
			left = BinOpNode(left, op_tok, right)

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
	def __init__(self, value):
		self.value = value
		self.set_pos()
		self.set_context()
	
	def set_pos(self, pos_start=None, pos_end=None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self

	def set_context(self, context=None):
		self.context = context
		return self

###############################################################################################

	def added_to(self, other):
		if isinstance(other, Number):
			return Number(self.value + other.value).set_context(self.context), None
	
	def subbed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value - other.value).set_context(self.context), None

	def multed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value * other.value).set_context(self.context), None

	def dived_by(self, other):
		if isinstance(other, Number):
			if other.value == 0:
				return None, RunTimeError(self.pos_start, self.pos_end, "Division By Zero", self.context)
			else:
				return Number(self.value / other.value).set_context(self.context), None

	def raised_to(self, other):
		if isinstance(other, Number):
			return Number(self.value**other.value).set_context(self.context), None

	def modded_to(self, other):
		if isinstance(other, Number):
			return Number(self.value%other.value).set_context(self.context), None
	
	def floor_dived_to(self, other):
		if isinstance(other, Number):
			if other.value == 0:
				return None, RunTimeError(self.pos_start, self.pos_end, "Division By Zero", self.context)
			else:
				return Number(self.value // other.value).set_context(self.context), None

	def get_comparison_eq(self, other):
		if isinstance(other, Number):
			return Number(int(self.value == other.value)).set_context(self.context), None

	def get_comparison_ne(self, other):
		if isinstance(other, Number):
			return Number(int(self.value != other.value)).set_context(self.context), None

	def get_comparison_lt(self, other):
		if isinstance(other, Number):
			return Number(int(self.value < other.value)).set_context(self.context), None

	def get_comparison_gt(self, other):
		if isinstance(other, Number):
			return Number(int(self.value > other.value)).set_context(self.context), None

	def get_comparison_lte(self, other):
		if isinstance(other, Number):
			return Number(int(self.value <= other.value)).set_context(self.context), None

	def get_comparison_gte(self, other):
		if isinstance(other, Number):
			return Number(int(self.value >= other.value)).set_context(self.context), None

	def anded_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.value and other.value)).set_context(self.context), None

	def ored_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.value or other.value)).set_context(self.context), None

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

class Number(Value):
	def __init__(self, value):
		super().__init__(value)


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
	def __init__(self):
		self.symbols = {}
		self.parent = None
	
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
		if node.op_tok.type == TT_PLUS:
			result, error = left.added_to(right)
		elif node.op_tok.type == TT_MINUS:
			result, error = left.subbed_by(right)
		elif node.op_tok.type == TT_DIV:
			result, error = left.dived_by(right)
		elif node.op_tok.type == TT_MUL:
			result, error = left.multed_by(right)
		elif node.op_tok.type == TT_POW:
			result, error = left.raised_to(right)
		elif node.op_tok.type == TT_MOD:
			result, error = left.modded_to(right)
		elif node.op_tok.type == TT_FDIV:
			result, error = left.floor_dived_to(right)
		elif node.op_tok.type == TT_EE:
			result, error = left.get_comparison_eq(right)
		elif node.op_tok.type == TT_NE:
			result, error = left.get_comparison_ne(right)
		elif node.op_tok.type == TT_LT:
			result, error = left.get_comparison_lt(right)
		elif node.op_tok.type == TT_GT:
			result, error = left.get_comparison_gt(right)
		elif node.op_tok.type == TT_LTE:
			result, error = left.get_comparison_lte(right)
		elif node.op_tok.type == TT_GTE:
			result, error = left.get_comparison_gte(right)
		elif node.op_tok.matches(TT_KEYWORD, 'and'):
			result, error = left.anded_by(right)
		elif node.op_tok.matches(TT_KEYWORD, 'or'):
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
		if node.op_tok.type == TT_MINUS:
			num, error = num.multed_by(Number(-1))
		elif node.op_tok.matches(TT_KEYWORD, 'not'):
			num, error = num.notted()
		
		if error:
			return res.failure(error)
		else:
			return res.success(num.set_pos(node.pos_start, node.pos_end))

	def visit_VarAssignNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_tok.value
		value = res.register(self.visit(node.value_node, context))
		if res.error: return res
		
		context.symbol_table.set(var_name, value)
		return res.success(value)

	def visit_VarAccessNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_tok.value
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
		


##############################################################################################
# RUN
##############################################################################################


global_symbol_table = SymbolTable()
global_symbol_table.set("True", Number(1))
global_symbol_table.set("False", Number(0))
global_symbol_table.set("Null", Number(0))

def run(fn, text):
	lexer = Lexer(fn, text)
	tokens, error = lexer.make_tokens()
	if error: return None, error


	parser = Parser(tokens)
	ast = parser.parse()
	if ast.error: return None, ast.error

	interpreter = Interpreter()
	context = Context("<Program>")
	context.symbol_table = global_symbol_table
	result = interpreter.visit(ast.node, context)

	return result.value, result.error