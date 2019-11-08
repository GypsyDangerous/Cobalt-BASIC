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

	def matches(self, type_, value):
		if not type(value) in [list, tuple]:
			return self.type == type_ and self.value == value
		else:
			return self.type == type_ and self.value in value

	def __str__(self):
		return f"{self.type}: {self.value}" if self.value else f"{self.type}"

	def __repr__(self):
		return str(self)
