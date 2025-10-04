// C++ Function Errors - All errors active for compilation

#include <iostream>
#include <string>
using namespace std;

// Error: Missing return type
printHello() {
    cout << "Hello" << endl;
}

// Error: Missing semicolon after function definition
void sayHi() {
    cout << "Hi" << endl;
}

void greet() {
    cout << "Greetings" << endl;
}

// Error: Function defined inside another function (not allowed in standard C++)
void outerFunction() {
    void innerFunction() {
        cout << "Inner" << endl;
    }
}

// Error: Missing parameter type
float calculateTotal(size, int toppings) {
    return 15.99;
}

// Error: Return type doesn't match returned value
int getValue() {
    return 3.14;
}

// Error: Missing return statement in non-void function
int getNumber() {
    int x = 5;
}

// Error: Returning value from void function
void processData() {
    return 42;
}

// Error: Using 'call' keyword before function name
void myFunction() {
    cout << "Test" << endl;
}

void test() {
    call myFunction();
}

// Error: Function overloading with same signature (only return type differs)
int calculate(int a, int b) {
    return a + b;
}

double calculate(int a, int b) {
    return a + b;
}

// Error: Missing parentheses in function call
void display() {
    cout << "Display" << endl;
}

void run() {
    display;
}

// Error: Default parameter not at end
void orderPizza(string size = "medium", int toppings, bool delivery = false) {
    cout << "Order placed" << endl;
}

// Error: Redefining default parameter in implementation
void makeOrder(string size, int toppings = 2);

void makeOrder(string size, int toppings = 3) {
    cout << "Making order" << endl;
}

// Error: Missing semicolon after function prototype
void prototype()

// Error: Parameter name used in function call (C++ doesn't support named parameters like this)
void setPizza(string size, int count) {
    cout << size << " " << count << endl;
}

void order() {
    setPizza(size = "large", count = 3);
}

// Error: Using 'begin' and 'end' keywords
void calculate() begin
    int x = 5;
end

// Error: Using 'none' or 'procedure' instead of void
none printMessage() {
    cout << "Message" << endl;
}

procedure doSomething() {
    cout << "Something" << endl;
}

// Error: Missing comma between parameters
int addNumbers(int a int b) {
    return a + b;
}

// Error: Using semicolon instead of comma between parameters
float getPrice(float base; float tax) {
    return base + tax;
}

// Error: Semicolon after function header before opening brace
void processValue(); {
    int x = 10;
}

// Error: Parameter type comes after parameter name
void setValue(count int, price float) {
    cout << count << " " << price << endl;
}

// Error: Using 'default' keyword for default parameters
void createPizza(string size default "small") {
    cout << size << endl;
}

// Error: Using 'with parameters' syntax
void myFunc() {
    cout << "Function" << endl;
}

void caller() {
    call myFunc with parameters();
}

// Error: Missing parameter name in function definition
int multiply(int, int b) {
    return 5 * b;
}

// Error: Wrong number of arguments
int add(int a, int b) {
    return a + b;
}

void callAdd() {
    int result = add(5);
}

// Error: Using 'function' keyword
function void display() {
    cout << "Display" << endl;
}

// Error: Return statement outside function
int globalValue = 10;
return globalValue;

// Error: Type specified in function call arguments
float computeTotal(float price, float tax) {
    return price + tax;
}

void purchase() {
    float total = computeTotal(float 10.0, float 2.0);
}

int main() {
    cout << "Main" << endl;
    return 0;
}
