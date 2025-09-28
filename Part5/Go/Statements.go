package main

import "fmt"

func main() {
    // Variable declarations
    a := 5
    b := 10
    c := 3.14
    flag := true
    stringOne := "Hello"
    result := 0
    
    // ERROR: Unused variable (Go compiler strictly enforces this)
    unusedVar := 42
    
    // ERROR: Type mismatch in assignment
    result = c
    
    // ERROR: Missing variable declaration with :=
    newVar = 10
    
    // ERROR: Redeclaring variable with :=
    a := 20
    
    // ERROR: Mixing types in arithmetic operations
    result = a + c
    
    // ERROR: Wrong format verb in Printf
    fmt.Printf("%s\n", a)
    
    // ERROR: Using == to compare slices
    slice1 := []int{1, 2, 3}
    slice2 := []int{1, 2, 3}
    flag = (slice1 == slice2)
    
    // ERROR: Type assertion on non-interface type
    result = a.(int)
    
    // ERROR: Taking address of expression
    ptr := &(a + b)
    
    // ERROR: Dereferencing non-pointer
    result = *a
    
    // ERROR: Using append on non-slice type
    result = append(a, 10)
    
    // ERROR: Assignment to constant
    const constValue = 10
    constValue = 20
    
    // ERROR: Wrong assignment operator for comparison
    flag = (a = b)
    
    // ERROR: Missing semicolon (in some contexts)
    result = a + b
    
    fmt.Println(result, flag, stringOne)
}
