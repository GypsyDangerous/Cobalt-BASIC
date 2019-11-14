from Errors import *
from Global_variables import *
from Nodes import *
from Token import Token

##############################################################################################
# PARSE RESULT
##############################################################################################


class ParseResult:
	def __init__(self, node=None):
		self.error = None
		self.node = node
		self.advance_count = 0
	
	def register_advancement(self):
		self.advance_count+=1
	
	def register_devancement(self):
		self.advance_count = max(self.advance_count-1, 0)

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
	
	def devance(self):
		self.token_index = max(self.token_index-1, 0)
		self.current_token = self.tokens[self.token_index]
		return self.current_token
	
###############################################################################################

	def parse(self, gst=None):
		'''
		general parse method that calls all the other necessary methods
		based on the grammar rules and the current text to parse
		'''
		res = self.expr(gst)
		if not res.error and self.current_token.type != TT_EOF:
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start, 
				self.current_token.pos_end, 
				"Expected '+', '-', '*', '/', '**', '==', '!=', '<', '>', <=', '>=', 'AND' or 'OR'"
			))
		return res

###############################################################################################

	def expr(self, gst=None):
		res = ParseResult()

		if self.current_token.matches(TT_KEYWORD, ("let", "var")):
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
				expr = res.register(ParseResult(NumberNode(Token(TT_INT, 0, pos_start = self.current_token.pos_start, pos_end = self.current_token.pos_end))))
			else:
				res.register_advancement()
				self.advance()
				expr = res.register(self.expr()) 

			if res.error: return res
			return res.success(VarAssignNode(var_name, expr))
		
		elif self.current_token.type == TT_IDENTIFER:
			var_name = self.current_token

			res.register_advancement()
			self.advance()

			if self.current_token.type != TT_EQ:
				res.register_devancement()
				self.devance()
			else:
				res.register_advancement()
				self.advance()
				expr = res.register(self.expr()) 
				if res.error: return res
				return res.success(VarReAssignNode(var_name, expr))



		node = res.register(self.bin_op(self.comp_expr, ((TT_KEYWORD, "and"), (TT_KEYWORD, "or"))))

		if res.error: 
			return res.failure(InvalidSyntaxError(
				self.current_token.pos_start,
				self.current_token.pos_end,
				"Expected 'let', int, float, identifier, '+', '-', or '('"
			))
		return res.success(node)
	
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

	def arith_expr(self):
		return self.bin_op(self.term, (TT_PLUS, TT_MINUS))	
	
###############################################################################################

	def term(self):
		return self.bin_op(self.factor, (TT_MUL, TT_DIV, TT_MOD, TT_FDIV))
	
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

	def power(self):
		return self.bin_op(self.call, (TT_POW, ), self.factor)
	
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

		if token.type == TT_STR:
			res.register_advancement()
			self.advance()
			return res.success(StringNode(token))

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

		elif token.type == TT_LBRACKET:
			list_expr = res.register(self.list_expr())
			if res.error: return res
			return res.success(list_expr)

		return res.failure(InvalidSyntaxError(
			self.current_token.pos_start,
			self.current_token.pos_end,
			"Expected 'let', 'if', 'for', 'while', 'def' int, float, identifier, '+', '-', '[', or '('"
		))
	

	def list_expr(self):
		res = ParseResult()
		element_nodes = []
		pos_start = self.current_token.pos_start.copy()
		
		if self.current_token.type != TT_LBRACKET:
			return res.failure(InvalidSyntaxError(
					self.current_token.pos_start,
					self.current_token.pos_end,
					"Expected '['"
				))

		res.register_advancement()
		self.advance()
		if self.current_token.type == TT_RBRACKET:
			res.register_advancement()
			self.advance()
		else:
			element_nodes.append(res.register(self.expr()))
			if res.error:
				return res.failure(InvalidSyntaxError(
					self.current_token.pos_start,
					self.current_token.pos_end,
					"Expected ']', 'let', 'if', 'for', 'while', 'def' int, float, identifier, '+', '-', or '('"
				))
			
			while self.current_token.type == TT_COMMA:
				res.register_advancement()
				self.advance()

				element_nodes.append(res.register(self.expr()))
				if res.error: return res

			if self.current_token.type != TT_RBRACKET:
				return res.failure(InvalidSyntaxError(
					self.current_token.pos_start,
					self.current_token.pos_end,
					"Expected ']' or ','"
				))

			res.register_advancement()
			self.advance()

		return res.success(ListNode(
			element_nodes,
			pos_start,
			self.current_token.pos_end.copy()
		))



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

###############################################################################################

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

###############################################################################################
	
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
