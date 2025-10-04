// Rust Function Errors - All errors active for compilation

// Error: Missing return type indicator (->)
fn get_number() i32 {
    42
}

// Error: Using 'void' instead of no return type
fn void print_hello() {
    println!("Hello");
}

// Error: Using 'none' for no return type
fn none say_hi() {
    println!("Hi");
}

// Error: Parameter type before name
fn add_numbers(i32 a, i32 b) -> i32 {
    a + b
}

// Error: Missing parameter type
fn calculate_total(size, toppings: i32) -> f64 {
    15.99
}

// Error: Using semicolon between parameters
fn set_pizza(size: String; count: i32) {
    println!("{} {}", size, count);
}

// Error: Missing comma between parameters
fn set_values(count: i32 price: f64) {
    println!("{} {}", count, price);
}

// Error: Using 'call' keyword
fn my_function() {
    println!("Test");
}

fn test() {
    call my_function();
}

// Error: Default parameters (not supported in standard Rust)
fn order_pizza(size: String = "medium", toppings: i32) {
    println!("Order placed");
}

// Error: Named parameters in call (not supported in Rust)
fn make_pizza(size: String, count: i32) {
    println!("{} {}", size, count);
}

fn place_order() {
    make_pizza(size = "large", count = 3);
}

// Error: Function overloading (not supported in Rust)
fn calculate(a: i32, b: i32) -> i32 {
    a + b
}

fn calculate(a: f64, b: f64) -> f64 {
    a + b
}

// Error: Missing semicolon after statement
fn process_data() {
    let x = 5
    let y = 10
}

// Error: Using 'return' incorrectly - semicolon after return value makes it void
fn get_value() -> i32 {
    return 42;
}

// Error: Missing return statement or expression
fn get_price() -> f64 {
    let price = 15.99;
}

// Error: Using 'begin' and 'end' keywords
fn compute() begin
    let x = 5;
end

// Error: Using 'function' keyword
function display() {
    println!("Display");
}

// Error: Using 'procedure' keyword
procedure run() {
    println!("Running");
}

// Error: Missing parentheses in function definition
fn show {
    println!("Show");
}

// Error: Missing parentheses in function call
fn greet() {
    println!("Greet");
}

fn call_greet() {
    greet;
}

// Error: Using 'with parameters' syntax
fn execute() {
    println!("Execute");
}

fn caller() {
    call execute with parameters();
}

// Error: Wrong number of arguments
fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn call_add() {
    let result = add(5);
}

// Error: Return type mismatch
fn get_pi() -> i32 {
    3.14
}

// Error: Using 'def' keyword
def print_message() {
    println!("Message");
}

// Error: Type in function call argument
fn get_total(price: f64, tax: f64) -> f64 {
    price + tax
}

fn purchase() {
    let total = get_total(f64 10.0, f64 2.0);
}

// Error: Using := for assignment or default
fn create_order(size: String := "small") {
    println!("{}", size);
}

// Error: Missing return type with arrow but no type specified
fn calculate_sum(a: i32, b: i32) -> {
    a + b
}

// Error: Semicolon after function signature
fn do_something(); {
    println!("Something");
}

// Error: Using 'return' outside function
let global_value = 10;
return global_value;

// Error: Implicit return with semicolon (makes it unit type, not i32)
fn factorial(n: i32) -> i32 {
    if n <= 1 {
        1;
    } else {
        n * factorial(n - 1);
    }
}

// Error: Multiple return values without tuple
fn divmod(a: i32, b: i32) -> i32, i32 {
    (a / b, a % b)
}

fn main() {
    println!("Main");
}
