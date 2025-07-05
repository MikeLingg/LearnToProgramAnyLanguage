fn main()
{
    // Don't forget to declare your variables as appropriate to the language, some languages will fail to compile with this program
    let valid_name: char = 'a';
    let wrong_case: char = 'a';
    let wr_ong_letter: char = 'a';
    
    // This will cause compile error - undefined variable
    println!("{}", invalid_name);
    // This will cause compile error - undefined variable
    println!("{}", validname);
    // This will cause compile error - undefined variable
    println!("{}", wrongcase);
    // This will cause compile error - undefined variable
    println!("{}", wr0ng_letter);
 
    // Don't start your variables with numbers or use hyphens
    // This will cause compile error - invalid identifier
    let 2_name_invalid: i32 = 5;
    // This will cause compile error - invalid identifier
    let invalid-name: char = 'a';
    
    println!("{}", 2_name_invalid);
    println!("{}", invalid-name);
    
    // Also avoid using keywords already reserved by the programming language
    // This will cause compile error - reserved keyword
    let fn: i32 = 1;
    // This will cause compile error - reserved keyword
    let struct: i32 = 2;
    
    println!("{}", fn);
    println!("{}", struct);
}
