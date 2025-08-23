fn main()
{
    // Use nested conditions to test a palindrome up to a set length
    let test_string = "radar";
    let mut is_a_palindrome = false;
    let mut left_char_index = 0;
    let mut right_char_index = test_string.len() - 1;
    
    let chars: Vec<char> = test_string.chars().collect();
    
    if chars[left_char_index] == chars[right_char_index]
    {
        left_char_index = left_char_index + 1;
        right_char_index = right_char_index - 1;
        if chars[left_char_index] == chars[right_char_index]
        {
            left_char_index = left_char_index + 1;
            right_char_index = right_char_index - 1;
            if chars[left_char_index] == chars[right_char_index]
            {
                is_a_palindrome = true;
            }
        }
    }
    
    println!( "Input string is a palindrome: {}", is_a_palindrome );
    
    // Guarded conditions version with length protections
    let test_string = "radar";
    let mut not_a_palindrome = false;
    let mut left_char_index = 0;
    let mut right_char_index = test_string.len() - 1;
    
    let chars: Vec<char> = test_string.chars().collect();
    
    if left_char_index < test_string.len()
    {
        if chars[left_char_index] != chars[right_char_index]
        {
            not_a_palindrome = true;
        }
    }
    
    if left_char_index < test_string.len()
    {
        if not_a_palindrome != true
        {
            left_char_index = left_char_index + 1;
            right_char_index = right_char_index - 1;
            if chars[left_char_index] != chars[right_char_index]
            {
                not_a_palindrome = true;
            }
        }
    }
    
    if left_char_index < test_string.len()
    {
        if not_a_palindrome != true
        {
            left_char_index = left_char_index + 1;
            right_char_index = right_char_index - 1;
            if chars[left_char_index] != chars[right_char_index]
            {
                not_a_palindrome = true;
            }
        }
    }
    
    println!( "Input string is a palindrome: {}", !not_a_palindrome );
}
