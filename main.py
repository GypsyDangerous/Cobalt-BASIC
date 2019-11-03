import basic

while True:
    text = input("basic > ")
    result, error = basic.run(__name__, text)
    if error:
        print(error)
    else:
        print(result)