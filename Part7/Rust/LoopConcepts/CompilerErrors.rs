use std::io;

fn main()
{
    // ERROR: Missing 'in' keyword in for loop
    for loop_index 0..10
    {
        println!("{}", loop_index);
    }

    // ERROR: Using assignment = instead of 'in'
    for loop_index = 0..10
    {
        println!("{}", loop_index);
    }

    // ERROR: Using colon instead of 'in'
    for loop_index: 0..10
    {
        println!("{}", loop_index);
    }

    // ERROR: Missing range operator '..'
    for loop_index in 0 10
    {
        println!("{}", loop_index);
    }

    // ERROR: Using 'to' instead of '..'
    for loop_index in 0 to 10
    {
        println!("{}", loop_index);
    }

    // ERROR: Using single dot instead of '..'
    for loop_index in 0.10
    {
        println!("{}", loop_index);
    }

    // ERROR: Using three dots instead of two
    for loop_index in 0...10
    {
        println!("{}", loop_index);
    }

    // ERROR: Using parentheses around range (C-style)
    for (loop_index in 0..10)
    {
        println!("{}", loop_index);
    }

    // ERROR: Missing opening brace
    for loop_index in 0..10
        println!("{}", loop_index);
    }

    // ERROR: Missing closing brace
    for loop_index in 0..10
    {
        println!("{}", loop_index);

    // ERROR: Using semicolons like C for loop
    for (loop_index = 0; loop_index < 10; loop_index++)
    {
        println!("{}", loop_index);
    }

    // ERROR: Using 'while' for range iteration
    while loop_index in 0..10
    {
        println!("{}", loop_index);
    }

    // ERROR: Missing condition in while loop
    let mut counter = 0;
    while
    {
        counter = counter + 1;
        if counter > 10 { break; }
    }

    // ERROR: Using parentheses around while condition
    while (counter < 10)
    {
        println!("{}", counter);
        counter = counter + 1;
    }

    // ERROR: Using assignment = instead of comparison in while
    while counter = 10
    {
        println!("{}", counter);
        counter = counter + 1;
    }

    // ERROR: Using 'do' keyword (not in Rust)
    let mut num = 0;
    do
    {
        println!("{}", num);
        num = num + 1;
    }
    while num < 10;

    // ERROR: Using 'foreach' instead of 'for'
    let arr = [1, 2, 3, 4, 5];
    foreach item in arr
    {
        println!("{}", item);
    }

    // ERROR: Using 'of' instead of 'in'
    for item of arr
    {
        println!("{}", item);
    }

    // ERROR: Missing 'mut' when loop variable needs to be mutable
    for loop_index in 0..10
    {
        loop_index = loop_index + 1;
        println!("{}", loop_index);
    }

    // ERROR: Using '=' instead of '..' in range
    for loop_index in 0=10
    {
        println!("{}", loop_index);
    }

    // ERROR: Using comma instead of '..'
    for loop_index in 0,10
    {
        println!("{}", loop_index);
    }

    // ERROR: Missing 'let' or 'let mut' in loop keyword
    loop
    {
        i = i + 1;
        if i > 10 { break; }
    }

    // ERROR: Using 'break' with non-existent label
    for i in 0..10
    {
        if i == 5
        {
            break 'outer;
        }
    }

    // ERROR: Using 'continue' with non-existent label
    for i in 0..10
    {
        if i % 2 == 0
        {
            continue 'outer;
        }
    }

    // ERROR: Missing colon after label
    'outer for i in 0..10
    {
        println!("{}", i);
    }

    // ERROR: Using 'end' or 'end for'
    for i in 0..10
    {
        println!("{}", i);
    }
    end for;

    // ERROR: Using increment operator ++
    let mut i = 0;
    while i < 10
    {
        println!("{}", i);
        i++;
    }

    // ERROR: Using += with wrong spacing
    let mut i = 0;
    while i < 10
    {
        println!("{}", i);
        i+ =1;
    }

    // ERROR: Using decrement operator --
    let mut i = 10;
    while i > 0
    {
        println!("{}", i);
        i--;
    }

    // ERROR: Using 'then' keyword
    while i < 10 then
    {
        println!("{}", i);
        i = i + 1;
    }

    // ERROR: Missing 'loop' keyword for infinite loop
    {
        println!("Forever");
        break;
    }

    // ERROR: Using square brackets instead of curly braces
    for i in 0..10 [
        println!("{}", i);
    ]

    // ERROR: Using 'step' keyword (wrong syntax)
    for i in 0..10 step 2
    {
        println!("{}", i);
    }

    // ERROR: Using 'by' for step
    for i in 0..10 by 2
    {
        println!("{}", i);
    }

    // ERROR: Wrong step_by syntax - missing parentheses
    for i in 0..10.step_by(2)
    {
        println!("{}", i);
    }

    // ERROR: Wrong step_by syntax - no range parentheses
    for i in 0..10.step_by(2)
    {
        println!("{}", i);
    }

    // ERROR: Using 'downto' (Pascal style)
    for i in 10 downto 0
    {
        println!("{}", i);
    }

    // ERROR: Missing '.rev()' for reverse iteration
    for i in 10..0
    {
        println!("{}", i);
    }

    // ERROR: Using 'until' instead of 'while'
    let mut i = 0;
    until i >= 10
    {
        println!("{}", i);
        i = i + 1;
    }

    // ERROR: Using 'repeat' (other languages)
    let mut i = 0;
    repeat
    {
        println!("{}", i);
        i = i + 1;
    }
    until i >= 10;

    // ERROR: Missing variable name in for loop
    for in 0..10
    {
        println!("iteration");
    }

    // ERROR: Using type annotation incorrectly
    for i: i32 in 0..10
    {
        println!("{}", i);
    }

    // ERROR: Using 'var' or 'let' in for loop
    for let i in 0..10
    {
        println!("{}", i);
    }

    // ERROR: Using 'mut' incorrectly in for loop
    for mut i in 0..10
    {
        i = i + 1;
        println!("{}", i);
    }

    // ERROR: Using inclusive range with wrong syntax
    for i in 0..=10..
    {
        println!("{}", i);
    }

    // ERROR: Missing equals in inclusive range
    for i in 0.=10
    {
        println!("{}", i);
    }

    // ERROR: Using wrong inclusive range operator
    for i in 0..<10
    {
        println!("{}", i);
    }

    // ERROR: Using 'as' for type conversion in range
    for i in 0 as i32..10
    {
        println!("{}", i);
    }

    // ERROR: Missing semicolon after break
    loop
    {
        break
        println!("After break");
    }

    // ERROR: Using && or || without proper syntax in while
    let mut i = 0;
    let mut j = 0;
    while i < 10 && j < 10
    {
        println!("{} {}", i, j);
        i = i + 1;
    }

    // ERROR: Using 'and' instead of '&&'
    while i < 10 and j < 10
    {
        println!("{} {}", i, j);
        i = i + 1;
    }

    // ERROR: Using 'or' instead of '||'
    while i < 10 or j < 10
    {
        println!("{} {}", i, j);
        i = i + 1;
    }

    // ERROR: Missing brace after range with method
    for i in (0..10).rev()
        println!("{}", i);

    // ERROR: Using 'next' instead of 'continue'
    for i in 0..10
    {
        if i % 2 == 0
        {
            next;
        }
        println!("{}", i);
    }

    // ERROR: Using 'exit' instead of 'break'
    for i in 0..10
    {
        if i == 5
        {
            exit;
        }
    }

    // ERROR: Wrong label syntax with extra colon
    'outer: for i in 0..10
    {
        for j in 0..10
        {
            break 'outer:;
        }
    }

    // ERROR: Using array as range bound
    let arr = [1, 2, 3, 4];
    for i in 0..arr
    {
        println!("{}", i);
    }
}
