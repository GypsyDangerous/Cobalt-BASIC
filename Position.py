from Global_variables import *
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
