fn main() {
    // Variable declarations
    let a = 5;
    let b = 10;
    let c = 3.14;
    let string_one = "Hello";
    let flag = true;
    
    // ERROR: Missing semicolon after expression statement
    let result = a + b
    
    // ERROR: Trying to mutate immutable variable
    a = 20;
    
    // ERROR: Type mismatch - cannot add integer and float
    let result = a + c;
    
    // ERROR: Cannot assign different types
    let result: i32 = c;
    
    // ERROR: Using uninitialized variable
    let uninitialized: i32;
    let result = uninitialized + 5;
    
    // ERROR: Wrong format specifier in println!
    println!("{:x}", string_one);
    
    // ERROR: Cannot compare different types directly
    let comparison = (a == c);
    
    // ERROR: Trying to use moved value
    let s1 = String::from("hello");
    let s2 = s1;
    println!("{}", s1);
    
    // ERROR: Cannot borrow as mutable
    let immutable_ref = &a;
    let mutable_ref = &mut a;
    
    // ERROR: Missing type annotation
    let result = Vec::new();
    
    // ERROR: Integer overflow in debug mode
    let result: u8 = 255 + 1;
    
    // ERROR: Cannot index with wrong type
    let arr = [1, 2, 3, 4, 5];
    let index = 2.0;
    let value = arr[index];
    
    // ERROR: Using & without proper context
    let reference = &;
    
    // ERROR: Wrong lifetime annotation
    let result: &'static str = &string_one;
    
    // ERROR: Cannot use undefined macro
    undefined_macro!("Hello");
    
    // ERROR: Wrong number of arguments to println!
    println!("{} {}", a);
    
    // ERROR: Cannot convert automatically
    let result: f32 = a;
    
    // ERROR: Pattern matching assignment without destructuring
    let (x, y) = 5;
    
    // ERROR: Using dot operator on primitive incorrectly
    let length = a.len();
}
