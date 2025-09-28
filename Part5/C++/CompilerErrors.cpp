#include <iostream>
#include <string>
#include <cstring>

int main()
{
    // Variable declarations
    int a = 5;
    int b = 10;
    float c = 3.14f;
    std::string stringOne = "Hello";
    bool flag = true;
    int result;
    
    // ERROR: Missing semicolon after expression statement
    result = a + b
    
    // ERROR: Missing std:: namespace qualifier
    string badString = "Error";
    
    // ERROR: Using C-style string comparison on std::string objects
    result = strcmp(stringOne, "Hello");
    
    // ERROR: Const correctness violation
    const int constValue = 100;
    int* badPtr = &constValue;
    
    // ERROR: Reference must be initialized
    int& uninitRef;
    
    // ERROR: Auto without initializer
    auto badAuto;
    
    // ERROR: Using . operator with pointer instead of ->
    std::string* strPtr = &stringOne;
    result = strPtr.length();
    
    // ERROR: Using assignment in boolean context
    flag = (a = 5);
    
    // ERROR: Template syntax without template parameters
    std::vector vec;
    
    // ERROR: Missing semicolon after struct definition
    struct Point { int x, y; }
    
    // ERROR: Scope resolution operator used incorrectly
    result = ::a;
    
    // ERROR: Stream operator ambiguity
    std::cout << (a >> 2) >> " shifted";
    
    // ERROR: Constructor initialization syntax in wrong context
    result(10, 20);
    
    // ERROR: Using this in non-member context
    result = this->a;
    
    return 0;
}
