from Global_variables import *
from Token import *
from Errors import *

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
		self.symbols[name] = value

	def remove(self, name):
		del self.symbols[name]

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
		return Number(int(type(self).__name__ == type(other).__name__)), None

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
		other = other or self
		return RunTimeError(
			self.pos_start, other.pos_end,
			f"Illegal operation between types: '{type(self).__name__}' and '{type(other).__name__}'",
			self.context or other.context
		)

class NoneType(Value):
	def __init__(self):
		super().__init__()

	def copy(self):
		return NoneType().set_pos(self.pos_start, self.pos_end).set_context(self.context)
	
	def __str__(self):
		return "None"
	
	def __repr__(self):
		return str(self)




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
				return None, RunTimeError(
          self.pos_start, 
          self.pos_end, 
          "Division By Zero", 
          self.context)
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
				return None, RunTimeError(
        self.pos_start, 
        self.pos_end, 
        "Division By Zero", 
        self.context)
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

	def __eq__(self, other):
		try:
			return self.value == other.value
		except:
			return False

Number.true = Number(1)
Number.false = Number(0)
Number.null = Number(0)

class BaseFunction(Value):
	def __init__(self, name):
		super().__init__()
		self.name = name or "<anonymous>"

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
				self.pos_start,
         self.pos_end,
				f"{len(args) - len(self.arg_names)} too many args passed into '{self.name}'",
				self.context
			))
		
		if len(args) < len(self.arg_names):
			return res.failure(RunTimeError(
				self.pos_start,
         self.pos_end,
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
		return f"<function {self.name} at {hex(id(self))}>"

	def __repr__(self):
		return str(self)

class String(Value):
	def __init__(self, value):
		super().__init__()
		self.value = value

	def added_to(self, other):
		if isinstance(other, String):
			return String(self.value+other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def multed_by(self, other):
		if isinstance(other, Number):
			return String(self.value*other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)
	
	def is_true(self):
		return len(self.value) > 0

	def copy(self):
		return String(self.value).set_pos(self.pos_start, self.pos_end).set_context(self.context)

	def get_comparison_eq(self, other):
		if isinstance(other, String):
			return Number(int(self.value == other.value)), None
		else:
			return Number(0), None

	def __str__(self):
		return f'"{self.value}"'
	
	def __repr__(self):
		return str(self)

class List(Value):
	def __init__(self, elements):
		super().__init__()
		self.elements = elements[:]

	def added_to(self, other):
		new_list = self.copy()
		new_list.elements.append(other)
		return new_list, None

	def subbed_by(self, other):
		if isinstance(other, Number):
			new_list = self.copy()
			try:
				new_list.elements.pop(other.value)
			except:
				return None, RunTimeError(
					other.pos_start,
					other.pos_end,
					"Invalid Index",
					self.context
				)
			return new_list, None
		else:
			return None, Value.illegal_operation(self, other)

	def dived_by(self, other):
		if isinstance(other, Number):
			try:
				return self.elements[other.value], None
			except:
				return None, RunTimeError(
					other.pos_start,
					other.pos_end,
					"Invalid Index",
					self.context
				)
		else:
			return None, Value.illegal_operation(self, other)


	def multed_by(self, other):
		if isinstance(other, List):
			return List(self.elements+other.elements).set_pos(self.pos_start, self.pos_end).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)
	
	def is_true(self):
		return len(self.elements) > 0

	def get_comparison_eq(self, other):
		if isinstance(other, List):
			return Number(int(self.elements == other.elements)), None
		else:
			return Number(0), None

	def __eq__(self, other):
		try:
			return self.elements == other.elements
		except:
			return False

	def copy(self):
		return List(self.elements).set_pos(self.pos_start, self.pos_end).set_context(self.context)
		
	def __str__(self):
		return f'[{", ".join([str(x) for x in self.elements])}]'
	
	def __repr__(self):
		return str(self)


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
			return res.success(num.set_pos(node.pos_start, node.pos_end))\

	def visit_NoneNode(self, node, context):
		print(context)
		return RTResult().success(NoneType().set_pos(node.pos_start, node.pos_end).set_context(context))


	def visit_VarAssignNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_token.value
		if var_name in CONSTANTS:
			return res.failure(
				InvalidSyntaxError(
					node.pos_start,
					node.pos_end,
					"Can't assign to keyword",
				)
			)
		value = res.register(self.visit(node.value_node, context))
		if res.error: return res
		
		context.symbol_table.set(var_name, value)
		return res.success(value)

	def visit_VarReAssignNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_token.value

		if var_name in CONSTANTS:
			return res.failure(
				InvalidSyntaxError(
					node.pos_start,
					node.pos_end,
					"Can't assign to keyword",
				)
			)

		current = context.symbol_table.get(var_name)

		if current == None:
			return res.failure(RunTimeError(
				node.pos_start,
				node.pos_end,
				f"'{var_name}' is not defined",
				context
			))

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
		
		value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
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
		elements = []

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
			elements.append(result)
			if res.error: return res

		return res.success(
			List(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
		)



	def visit_WhileNode(self, node, context):
		res = RTResult()
		elements = []
		while True:
			condition = res.register(self.visit(node.condition_node, context))
			if res.error: return res

			if not condition.is_true(): break

			result = res.register(self.visit(node.body_node, context))
			elements.append(result)
			if res.error: return res

		return result.success(
			List(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
		)

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

	def visit_StringNode(self, node, context):
		return RTResult().success(
			String(node.token.value).set_context(context).set_pos(node.pos_start, node.pos_end)
			)

	def visit_ListNode(self, node, context):
		res = RTResult()
		elements = []

		for element_node in node.element_nodes:
			elements.append(res.register(self.visit(element_node, context)))
			if res.error: return res

		return res.success(
			List(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
		)