# Python Statement Syntax Errors
# These are syntax errors that prevent the script from running

# Variable declarations
a = 5
b = 10
c = 3.14
string_one = "Hello"
flag = True
result = 0

# ERROR: Cannot assign to literal
5 = a

# ERROR: Cannot assign to expression
(a + b) = 10

# ERROR: Invalid syntax - missing closing parenthesis
result = (a + b

# ERROR: Invalid syntax - missing closing bracket
my_list = [1, 2, 3

# ERROR: Invalid syntax - missing closing brace
my_dict = {"key": "value"

# ERROR: Invalid syntax - missing closing quote
bad_string = "Hello World

# ERROR: Using assignment operator in comparison context (syntax error)
if a = b:
    print("This won't work")

# ERROR: Invalid indentation (IndentationError)
result = a + b
 result = result * 2

# ERROR: Invalid character in variable name
re$ult = a + b

# ERROR: Variable name starting with number
2nd_value = 10

# ERROR: Using reserved keyword as variable name
def = 5

# ERROR: Invalid operator combination
result = a ++ b

# ERROR: Wrong use of colon outside control structures
result = a + b:

# ERROR: Invalid f-string syntax - missing closing brace
formatted = f"Value is {a"

# ERROR: Invalid escape sequence in string
bad_escape = "Hello\q World"

# ERROR: Mixing spaces and tabs (can cause IndentationError)
def test_function():
    x = 1
	y = 2  # This line uses tab instead of spaces

# ERROR: Invalid syntax in tuple unpacking
a, = (1, 2, 3)  # Too many values to unpack

# ERROR: Invalid augmented assignment target
(a + b) += 5

# ERROR: Using walrus operator incorrectly (Python 3.8+)
result = a := b + 5  # Should be (a := b + 5)

# ERROR: Invalid dictionary comprehension syntax
bad_dict = {for x in range(5): x}

# ERROR: Missing comma in function arguments
print("Hello" "World" without_comma)

# ERROR: Invalid use of ellipsis outside type hints
result = a + ... + b

# ERROR: Invalid syntax with lambda
bad_lambda = lambda x, y z: x + y + z

# ERROR: Invalid use of yield outside function
yield_result = yield 5

# ERROR: Invalid syntax in list comprehension
bad_list = [x for x in range(5) x > 2]

# ERROR: Invalid backslash at end of line
result = a + \

# ERROR: Invalid use of nonlocal outside function
nonlocal a

# ERROR: Invalid syntax with decorators
@invalid_decorator_syntax()
result = 5

print("End of error examples")
