// C Function Errors - All errors active for compilation

#include <stdio.h>
#include <stdbool.h>

// Error: Missing return type (implicit int in old C, error in modern C)
printHello() {
    printf("Hello\n");
}

// Error: Missing semicolon after function definition
void sayHi() {
    printf("Hi\n");
}

void greet() {
    printf("Greetings\n");
}

// Error: Missing parameter type in definition
float calculateTotal(size, int toppings) {
    return 15.99;
}

// Error: Return type mismatch - returning float from int function
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
    printf("Test\n");
}

void test() {
    call myFunction();
}

// Error: Missing parentheses in function call
void display() {
    printf("Display\n");
}

void run() {
    display;
}

// Error: Default parameters (not supported in C)
void orderPizza(char* size = "medium", int toppings, bool delivery = false) {
    printf("Order placed\n");
}

// Error: Named parameters in function call (not supported in C)
void setPizza(char* size, int count) {
    printf("%s %d\n", size, count);
}

void order() {
    setPizza(size = "large", count = 3);
}

// Error: Function overloading (not supported in C)
int calculate(int a, int b) {
    return a + b;
}

float calculate(float a, float b) {
    return a + b;
}

// Error: Nested function definition (not standard C)
void outerFunction() {
    void innerFunction() {
        printf("Inner\n");
    }
    innerFunction();
}

// Error: Missing semicolon after function prototype
void prototype()

// Error: Using 'begin' and 'end' keywords
void compute() begin
    int x = 5;
end

// Error: Using 'procedure' instead of 'void'
procedure doSomething() {
    printf("Something\n");
}

// Error: Semicolon after function header before opening brace
void processValue(); {
    int x = 10;
}

// Error: Missing parameter name in function definition
int addNumbers(int, int b) {
    return 5 + b;
}

// Error: Using 'none' return type
none printMessage() {
    printf("Message\n");
}

// Error: Comma instead of semicolon after function prototype
void myProto(),

// Error: Wrong number of arguments in function call
int add(int a, int b) {
    return a + b;
}

void callAdd() {
    int result = add(5);
}

// Error: Type in parameter name position for function call
float getPrice(float base, float tax) {
    return base + tax;
}

void purchase() {
    float total = getPrice(float 10.0, float 2.0);
}

// Error: Missing comma between parameters in definition
void setValues(int count float price) {
    printf("Count: %d, Price: %.2f\n", count, price);
}

// Error: Using 'return' outside of function
int globalValue = 10;
return globalValue;

int main() {
    printf("Main\n");
    return 0;
}
