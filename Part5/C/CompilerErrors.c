#include <stdio.h>
#include <stdbool.h>
#include <string.h>

int main()
{
    // Variable declarations
    int a = 5;
    int b = 10;
    float c = 3.14f;
    char stringOne[] = "Hello";
    bool flag = true;
    int result;
    
    // ERROR: Missing semicolon after expression statement
    result = a + b
    
    // ERROR: Using assignment operator instead of comparison
    flag = (a = b);
    
    // ERROR: Trying to modify string literal
    char *str = "Hello";
    str[0] = 'h';
    
    // ERROR: Array assignment (arrays cannot be assigned directly)
    char arr1[10] = "test";
    char arr2[10];
    arr2 = arr1;
    
    // ERROR: Missing parentheses in function call
    printf "Hello World\n";
    
    // ERROR: Wrong parameter types for strcmp
    result = strcmp(a, b);
    
    // ERROR: Missing address-of operator for scanf
    scanf("%d", result);
    
    // ERROR: Wrong format specifier
    printf("%d\n", c);
    
    // ERROR: Bitwise operator used instead of logical operator in assignment
    flag = (a & b);
    
    // ERROR: Using void function result in assignment
    result = printf("Hello");
    
    // ERROR: Modifying const variable
    const int constant = 100;
    constant = 200;
    
    // ERROR: Division by literal zero
    result = a / 0;
    
    // ERROR: Array index with floating point
    int arr[10];
    arr[c] = 5;
    
    // ERROR: Wrong use of ternary operator
    printf("%d\n", (a > b) ? printf("Greater") : printf("Lesser"));
    
    return 0;
}
