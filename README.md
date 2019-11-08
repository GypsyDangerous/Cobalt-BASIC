# Cobalt BASIC
Cobalt BASIC is a simple programming language with syntax inspired by python and javascript with an aim for readability and understandablility. It is the fastest langauge ever because it is interpreted by python, but it will get the job done for simple tasks
## Files
 ### main.py 
 - Contains the shell for running programs in my programming language
 - imports basic.py
### basic.py 
- Contains all the code the actual programming language, including the `lexer`, `parser`, and `interpreter`
- imports string_with_arrows.py, Token.py, Nodes.py, Lexer.py, Errors.py, and Parser.py
### string_with_arrows.py
- Contains a single method called `string_with_arrows`, for inserting `^` characters where an error appears in my programming language
### Token.py 
- Contains the Token class and all token type constants used in the language

        

## Installation
Currently this project can be run from any python environmment with no special installations, all you need to do is
1. clone this repo
2. cd into the directory for the repo
3. run `main.py` from your preffered python executor

## Todo list
- [x] add functions
- [ ] add strings
- [ ] add lists
- [ ] add built in functions
- [ ] add multi-line code compatability
- [ ] add return statement
- [ ] add iterative for loops
- [ ] add default arguments to functions
- [ ] add args and kwargs to functions
- [ ] improve anonymous functions

## Credits
The base of this code was written while following this [tutorial series](https://www.youtube.com/playlist?list=PLZQftyCk7_SdoVexSmwy_tBgs7P0b97yD) on youtube. I have added some of my own features and syntax. I plan to go beyond this tutorial series and add even more features as I learn more about this topic.

### saved tutorials
- freecodecamp [article](https://www.freecodecamp.org/news/the-programming-language-pipeline-91d3f449c919/)
- hackernoon [article](https://hackernoon.com/lets-build-a-programming-language-2612349105c6)
