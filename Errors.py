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
