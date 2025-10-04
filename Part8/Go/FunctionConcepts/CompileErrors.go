// Go Function Errors - All errors active for compilation

package main

import "fmt"

// Error: Opening brace on next line (Go requires it on same line)
func printHello()
{
    fmt.Println("Hello")
}

// Error: Missing parameter type
func calculateTotal(size, toppings int, delivery) float64 {
    return 15.99
}

// Error: Return type before function name
float64 func getPrice() {
    return 10.50
}

// Error: Using 'void' instead of no return type
func void sayHi() {
    fmt.Println("Hi")
}

// Error: Using 'none' for no return type
func none display() {
    fmt.Println("Display")
}

// Error: Missing return statement in function with return type
func getNumber() int {
    x := 5
}

// Error: Returning value from function with no return type
func processData() {
    return 42
}

// Error: Return type mismatch
func getValue() int {
    return 3.14
}

// Error: Using 'call' keyword
func myFunction() {
    fmt.Println("Test")
}

func test() {
    call myFunction()
}

// Error: Using semicolon between parameters
func setPizza(size string; count int) {
    fmt.Printf("%s %d\n", size, count)
}

// Error: Parameter type before name
func addNumbers(int a, int b) int {
    return a + b
}

// Error: Default parameters (not supported in Go)
func orderPizza(size string = "medium", toppings int) {
    fmt.Println("Order placed")
}

// Error: Named parameters in call (not supported in Go)
func makePizza(size string, count int) {
    fmt.Printf("%s %d\n", size, count)
}

func placeOrder() {
    makePizza(size = "large", count = 3)
}

// Error: Function overloading (not supported in Go)
func calculate(a int, b int) int {
    return a + b
}

func calculate(a float64, b float64) float64 {
    return a + b
}

// Error: Using 'begin' and 'end' keywords
func compute() begin
    x := 5
end

// Error: Missing comma between parameters
func setValues(count int price float64) {
    fmt.Printf("%d %.2f\n", count, price)
}

// Error: Semicolon after function signature
func doSomething(); {
    fmt.Println("Something")
}

// Error: Using 'function' keyword
function display() {
    fmt.Println("Display")
}

// Error: Using 'procedure' keyword
procedure run() {
    fmt.Println("Running")
}

// Error: Parentheses required even with no parameters (missing them)
func show {
    fmt.Println("Show")
}

// Error: Using 'with parameters' syntax
func execute() {
    fmt.Println("Execute")
}

func caller() {
    call execute with parameters()
}

// Error: Missing parentheses in function call
func greet() {
    fmt.Println("Greet")
}

func callGreet() {
    greet
}

// Error: Wrong number of arguments
func add(a int, b int) int {
    return a + b
}

func callAdd() {
    result := add(5)
}

// Error: Using := for parameter default (trying default params wrong way)
func createOrder(size string := "small") {
    fmt.Println(size)
}

// Error: Type in function call argument
func getTotal(price float64, tax float64) float64 {
    return price + tax
}

func purchase() {
    total := getTotal(float64 10.0, float64 2.0)
}

// Error: Multiple return values without parentheses
func divmod(a int, b int) int, int {
    return a / b, a % b
}

// Error: Return statement outside function
var globalValue = 10
return globalValue

// Error: Using 'def' keyword
def printMessage() {
    fmt.Println("Message")
}

// Error: Nested function definition (not standard Go)
func outer() {
    func inner() {
        fmt.Println("Inner")
    }
}

func main() {
    fmt.Println("Main")
}
