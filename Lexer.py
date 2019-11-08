from Position import *
from Token import *
from Global_variables import *
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
