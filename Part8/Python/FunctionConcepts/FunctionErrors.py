# Python Function Errors - All errors active for execution

# Error: Missing colon after function definition
def print_hello()
    print("Hello")

# Error: Inconsistent indentation in function body
def greet():
    print("Hello")
      print("Welcome")

# Error: Missing parentheses in function definition
def calculate_sum:
    return 5 + 10

# Error: Using 'call' keyword (not needed in Python)
def my_function():
    print("My function")

call my_function()

# Error: Forgetting parentheses when calling function
def say_hi():
    print("Hi")

say_hi

# Error: Wrong number of arguments
def add_numbers(a, b):
    return a + b

result = add_numbers(5)

# Error: Missing return keyword, just writing value
def get_total(x, y):
    x + y

# Error: Trying to use parameter names with colon (keyword before equals)
def pizza_cost(size, toppings):
    return 10.0

total = pizza_cost(size: "large", toppings: 3)

# Error: Default parameter before non-default parameter
def order_pizza(size="medium", toppings, delivery=False):
    return 15.0

# Error: Missing comma between parameters in definition
def calculate_price(base price, tax):
    return base + tax

# Error: Using 'return' outside of function
x = 10
return x

# Error: Trying to declare return type before function name
def int get_number():
    return 42

# Error: Using braces instead of indentation
def my_func() {
    print("Hello")
}

# Error: Using semicolon instead of colon
def display();
    print("Display")

# Error: Forgetting to indent function body
def do_something():
print("Not indented")

# Error: Using 'end' keyword to close function
def calculate():
    x = 5
    return x
end

# Error: Passing positional argument after keyword argument
def make_order(size, toppings, delivery):
    return 20.0

price = make_order(size="large", 3, delivery=True)

# Error: Using wrong syntax for default parameter
def greet_user(name = default "Guest"):
    print(f"Hello {name}")

# Error: Redefining parameter with type declaration
def process(value):
    int value = 10
    return value

# Error: Using 'begin' keyword
def compute() begin:
    x = 5

# Error: Using 'function' keyword
function display():
    print("Display")

# Error: Using 'void' or 'none' return type
void print_message():
    print("Message")

none do_task():
    print("Task")

# Error: Using 'procedure' keyword
procedure run():
    print("Running")

# Error: Parameter type annotations in wrong position (type before name)
def set_value(int count, str name):
    print(count, name)

# Error: Using 'with parameters' syntax
def execute():
    print("Execute")

call execute with parameters()

# Error: Using := instead of = for default parameters
def create_order(size := "small"):
    print(size)

# Error: Using semicolon between parameters
def set_pizza(size; count):
    print(size, count)

# Error: Multiple colons in function definition
def my_function()::
    print("Multiple colons")

# Error: Missing parentheses but having colon
def show:
    print("Show")

# Error: Using 'def' with type before function name
def str get_name():
    return "John"

# Error: Nested function definition with incorrect indentation
def outer():
def inner():
    print("Inner")

# Error: Using arrow for return type (mixing with other languages)
def get_price() -> float:
    return 10.5

# Wait, that one is actually valid Python 3.5+, replace with invalid syntax
def get_price() => float:
    return 10.5

# Error: Using 'lambda' incorrectly as function definition
lambda my_function():
    print("Lambda")
