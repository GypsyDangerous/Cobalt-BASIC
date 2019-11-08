import basic

print('''
Welcome to "Cobalt BASIC", This is a simple programming language created in python.
It has very similar syntax to python and javascript because they are my two favorite languages.
The mechanics of the language are very similar to the BASIC programmming language, hence the name.
Variables are declared with let or var then the variable name, using let is no different from using var,
I only added two variable keywords as a challenge to myself and I still don't know how to treat them 
differently especcially in python, where there are 'lets' or 'vars' or 'consts'. I hope to add in the future the ability to create variables 
without keywords like let and var. The language supports for and while loops, lists, strings and numbers. I hope
to add more in the future.
Enjoy!
''')

while True:
    text = input("basic > ")
    result, error = basic.run(__name__, text)
    if error:
        print(error)
    elif result:
        print(result)
