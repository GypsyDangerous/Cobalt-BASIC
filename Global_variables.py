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
	"def",
	"var",
	"lambda",
	"end.",
	"return",
	"continue",
	"break"
]

CONSTANTS = [
	"True",
	"False",
	"None",
	"PI",
	"TWO_PI",
	"HALF_PI",
	"Null",
	"Infinity",
	"print",
	"input",
	"clear",
	"int",
	"float",
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
TT_STR = "STR"
TT_LBRACKET = "LBRACKET"
TT_RBRACKET = "RBRACKET"
TT_DOT = "DOT"
TT_NEWLINE = "NEWLINE"
