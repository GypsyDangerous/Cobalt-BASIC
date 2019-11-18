# This is basic documentation to help you get started using Cobalt BASIC

## Variable declaration:
variables can be declared with either the 'let' or 'var' keyword, despite there begin two keywords they
both act the same, they declare a variable that acts the same as a python variable would act.
Once a variable has been declared it can be reassigned within its scope by saying `var_name = new value`.
variables in Cobalt act the same as variables in python would so there type can be changed.

## Function Definition:
Functions can be defined in six ways,
1. single line named `def func_name(args) -> func`
2. single line anonymous `(args) -> func`
3. single line anonymous again ` def (args) -> func`
4. multiline named `def func_name(args) \n multi line func \n end.`
5. multiline anonymous `def (args) \n multi line func \n end.`
6. multiline anonymous again `def (args) \n multi line func \n end.`
Functions can have default arguments, but putting a default argument before a non-default argument may result in bugs.
default arguments act in a way that is similar to javascript, they are given they're default value if nothing is passed into them and they must be given in the correct inorder to get the correct outcome. Unlike python you can not use kwargs to define some default arguments out of order.
Functions can also have `*args`, `*args` must come at the end of the arguments list and are treated in a similar way to python but since the language doesn't have tuples the args are packed into a list.

## Objects
Since Cobalt is such a simple language it has very few native objects and doesn't currently support the creation of custom objects

The Native Objects in Cobalt are:
 - **Number** - any number either or float
 - **String** - a string of 1 or more characters
 - **List** - an array of 1 or more objects
 - **NoneType** - same as python NoneType


## Constants
Cobalt has a quite a few built in constants, they are:
 - **True** - boolean True (Number 1)
 - **False** - boolean False (Number 0)
 - **None** - NoneType
 - **Null** - NoneType
 - **PI** - Number `python math.pi`
 - **TWO_PI** - Number PI*2
 - **HALF_PI** - Number PI/2
 - **Infinity** - Number `python math.inf`




## end. keyword
The `end.` keyword is required for ending any multiline statement like functions, conditionals, and loops