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


# String Node
###############################################################################################

class StringNode:
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


class VarReAssignNode(VarAssignNode):
    def __init__(self, name, value):
        super().__init__(name, value)


class NoneNode:
    def __init__(self, pos_start, pos_end):
        self.pos_start = pos_start
        self.pos_end = pos_end

# If conditional Node
###############################################################################################


class IfNode:
    def __init__(self, cases, else_case):
        self.cases = cases
        self.else_case = else_case

        self.pos_start = self.cases[0][0].pos_start
        self.pos_end = (else_case or self.cases[-1])[0].pos_end

# For Loop Node
###############################################################################################


class ForNode:
    def __init__(self, var_name_token, start_value_node, end_value_node, step_value_node, body_node, should_return_none):
        self.var_name_token = var_name_token
        self.start_value_node = start_value_node
        self.end_value_node = end_value_node
        self.step_value_node = step_value_node
        self.body_node = body_node
        self.should_return_none = should_return_none

        self.pos_start = self.var_name_token.pos_start
        self.pos_end = self.body_node.pos_end


# While Loop Node
###############################################################################################

class WhileNode:
    def __init__(self, condition_node, body_node, should_return_none):
        self.condition_node = condition_node
        self.body_node = body_node
        self.should_return_none = should_return_none

        self.pos_start = condition_node.pos_start
        self.pos_end = body_node.pos_end


# Function Definition Node
###############################################################################################

class FuncDefNode:
    def __init__(self, var_name_token, arg_name_tokens, body_node, should_return_none, has_star_args):
        self.var_name_token = var_name_token
        self.arg_name_tokens = arg_name_tokens
        self.body_node = body_node
        self.should_return_none = should_return_none
        self.has_star_args = has_star_args

        if self.var_name_token:
            self.pos_start = self.var_name_token.pos_start
        elif len(arg_name_tokens) > 0:
            self.pos_start = self.arg_name_tokens[0].pos_start
        else:
            self.pos_start = self.body_node.pos_start

        self.pos_end = self.body_node.pos_end

# Function Call Node
###############################################################################################


class CallNode:
    def __init__(self, node_to_call, arg_nodes):
        self.node_to_call = node_to_call
        self.arg_nodes = arg_nodes

        self.pos_start = node_to_call.pos_start

        if len(self.arg_nodes) > 0:
            self.pos_end = self.arg_nodes[-1].pos_end
        else:
            self.pos_end = self.node_to_call.pos_end


class ListNode:
    def __init__(self, element_nodes, pos_end, pos_start):
        self.element_nodes = element_nodes

        self.pos_start = pos_start
        self.pos_end = pos_end
