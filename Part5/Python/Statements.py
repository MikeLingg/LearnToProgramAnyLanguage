# Python Statement Syntax Errors
# Only errors NOT already covered in the existing Python code

# Variable declarations
a = 5
b = 10
c = 3.14
string_one = "Hello"
flag = True
result = 0

# ERROR: Missing closing parenthesis
result = (a + b

# ERROR: Missing closing bracket
my_list = [1, 2, 3

# ERROR: Missing closing quote
bad_string = "Hello World

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

# ERROR: Invalid f-string syntax - missing closing brace
formatted = f"Value is {a"

# ERROR: Invalid escape sequence in string
bad_escape = "Hello\q World"

# ERROR: Invalid augmented assignment target
(a + b) += 5

# ERROR: Missing comma in function arguments
print("Hello" "World" without_comma)

# ERROR: Invalid syntax with lambda
bad_lambda = lambda x, y z: x + y + z

# ERROR: Invalid syntax in list comprehension
bad_list = [x for x in range(5) x > 2]

# ERROR: Invalid backslash at end of line
result = a + \

# ERROR: Invalid use of nonlocal outside function
nonlocal a

print("End of error examples")
