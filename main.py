import basic
import argparse

print("_"*80)
print()
print('''
Welcome to "Cobalt BASIC", This is a simple programming language created in python.
It has very similar syntax to python and javascript because they are my two favorite languages, and the syntax is easy to understand.
The mechanics of the language are very similar to the BASIC programmming language, hence the name.
Variables are declared with let or var then the variable name, using let is no different from using var,
I only added two variable keywords as a challenge to myself and I still don't know how to treat them 
differently especcially in python, where there are 'lets' or 'vars' or 'consts'. I hope to add in the future the ability to create variables 
without keywords like let and var. The language supports for and while loops, lists, strings and numbers. I hope
to add more in the future.
Enjoy!
''')

parser = argparse.ArgumentParser()
parser.add_argument('filename', nargs="*")
args = parser.parse_args()
try:
    fn = f"{str(args.filename[0])}"
except:
    fn = None

def print_result(result, error=None):
    if type(result) == tuple:
        print_result(result[0], result[1])
    else:
        if error:
            print(error)
        elif result and repr(result.elements[0]) != "None":
            if len(result.elements) == 1:
                print(repr(result.elements[0]))
            else:
                print(repr(result))

if fn:
    print_result(basic.run(__name__, f'run("{fn}")'))
while not fn:
    text = input("Cobalt > ")
    result, error = basic.run(__name__, text)
    print_result(result, error)
