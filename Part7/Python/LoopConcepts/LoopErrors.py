def main():
    # ERROR: Missing colon after for statement
    for i in range(10)
        print(i)

    # ERROR: Missing colon after while statement
    i = 0
    while i < 10
        print(i)
        i = i + 1

    # ERROR: Using parentheses around condition (valid but often indicates C-style thinking)
    # Combined with missing colon
    while (i < 10)
        print(i)
        i += 1

    # ERROR: Missing indentation in loop body
    for i in range(10):
    print(i)

    # ERROR: Inconsistent indentation (mixing tabs and spaces)
    for i in range(10):
        print(i)
    	print(i)  # This line has a tab instead of spaces

    # ERROR: Using curly braces instead of colon and indentation
    for i in range(10) {
        print(i)
    }

    # ERROR: Using semicolons like C for loop
    for (i = 0; i < 10; i++):
        print(i)

    # ERROR: Using 'do' keyword (not in Python)
    i = 0
    do:
        print(i)
        i = i + 1
    while i < 10

    # ERROR: Missing 'in' keyword in for loop
    for i range(10):
        print(i)

    # ERROR: Using assignment = instead of 'in'
    for i = range(10):
        print(i)

    # ERROR: Using 'to' instead of 'in'
    for i to range(10):
        print(i)

    # ERROR: Missing 'range()' - trying to use bare numbers
    for i in 0..10:
        print(i)

    # ERROR: Using '..' for range (from other languages)
    for i in 0 .. 10:
        print(i)

    # ERROR: Using 'foreach' instead of 'for'
    arr = [1, 2, 3, 4, 5]
    foreach item in arr:
        print(item)

    # ERROR: Using 'of' instead of 'in'
    for item of arr:
        print(item)

    # ERROR: Using increment operator ++
    i = 0
    while i < 10:
        print(i)
        i++

    # ERROR: Using decrement operator --
    i = 10
    while i > 0:
        print(i)
        i--

    # ERROR: Using += with wrong syntax
    i = 0
    while i < 10:
        print(i)
        i+ =1

    # ERROR: Using 'then' keyword after condition
    i = 0
    while i < 10 then:
        print(i)
        i = i + 1

    # ERROR: Using 'end' or 'end for'
    for i in range(10):
        print(i)
    end for

    # ERROR: Using 'end while'
    i = 0
    while i < 10:
        print(i)
        i = i + 1
    end while

    # ERROR: Using 'next' instead of 'continue'
    for i in range(10):
        if i % 2 == 0:
            next
        print(i)

    # ERROR: Using 'exit' instead of 'break'
    for i in range(10):
        if i == 5:
            exit
        print(i)

    # ERROR: Missing parentheses in range()
    for i in range 10:
        print(i)

    # ERROR: Using semicolon at end of statement
    for i in range(10);
        print(i)

    # ERROR: Using 'until' instead of 'while'
    i = 0
    until i >= 10:
        print(i)
        i = i + 1

    # ERROR: Using 'repeat' (from other languages)
    i = 0
    repeat:
        print(i)
        i = i + 1
    until i >= 10

    # ERROR: Using 'loop' keyword
    loop i in range(10):
        print(i)

    # ERROR: Missing condition in while
    while:
        print("Hello")
        break

    # ERROR: Using 'step' keyword (VB/BASIC style)
    for i in range(0, 10, step 2):
        print(i)

    # ERROR: Using 'by' for step
    for i in range(0, 10) by 2:
        print(i)

    # ERROR: Using 'downto' (Pascal style)
    for i in 10 downto 0:
        print(i)

    # ERROR: Using assignment = instead of comparison ==
    i = 0
    while i = 10:
        print(i)
        i = i + 1

    # ERROR: Using single = in if statement
    for i in range(10):
        if i = 5:
            break

    # ERROR: Missing colon after if statement
    for i in range(10):
        if i == 5
            break

    # ERROR: Using 'elseif' instead of 'elif'
    for i in range(10):
        if i < 5:
            print("Less")
        elseif i > 5:
            print("Greater")

    # ERROR: Using 'else if' as two words
    for i in range(10):
        if i < 5:
            print("Less")
        else if i > 5:
            print("Greater")

    # ERROR: Missing colon after elif
    for i in range(10):
        if i < 5:
            print("Less")
        elif i > 5
            print("Greater")

    # ERROR: Missing colon after else
    for i in range(10):
        if i < 5:
            print("Less")
        else
            print("Equal or greater")

    # ERROR: Using 'pass' incorrectly (missing colon before)
    for i in range(10)
        pass

    # ERROR: Using square brackets instead of parentheses in range
    for i in range[10]:
        print(i)

    # ERROR: Using curly braces for range
    for i in range{10}:
        print(i)

    # ERROR: Wrong number of dots in range syntax
    for i in range(0...10):
        print(i)

    # ERROR: Using && instead of 'and'
    i = 0
    j = 0
    while i < 10 && j < 10:
        print(i, j)
        i = i + 1

    # ERROR: Using || instead of 'or'
    while i < 10 || j < 10:
        print(i, j)
        i = i + 1

    # ERROR: Using ! instead of 'not'
    flag = True
    while !flag:
        print("Running")
        flag = False

    # ERROR: Using parentheses around for loop (C-style)
    for (i in range(10)):
        print(i)

    # ERROR: Missing indentation after colon
    for i in range(10):
print(i)

    # ERROR: Over-indentation
    for i in range(10):
            print(i)

    # ERROR: Using 'var' or 'let' for variable declaration
    for var i in range(10):
        print(i)

    # ERROR: Using type annotation incorrectly in for loop
    for i: int in range(10):
        print(i)

    # ERROR: Using 'as' incorrectly
    for i as int in range(10):
        print(i)

    # ERROR: Missing break/continue statement (just keyword alone)
    for i in range(10):
        if i == 5
            break

    # ERROR: Using label syntax (not supported in Python)
    outer:
    for i in range(10):
        for j in range(10):
            if j == 5:
                break outer

    # ERROR: Using goto (not in Python)
    for i in range(10):
        if i == 5:
            goto end
    end:

    # ERROR: Using brackets for code blocks
    for i in range(10):
    [
        print(i)
    ]

    # ERROR: Missing 'pass' in empty loop (will cause IndentationError)
    for i in range(10):

    print("After loop")

    # ERROR: Wrong indentation level for else in for loop
    for i in range(10):
        print(i)
      else:
        print("Done")

    # ERROR: Using 'default' instead of else
    for i in range(10):
        if i == 5:
            print("Five")
        default:
            print("Not five")

    # ERROR: Incomplete range with just one argument but comma
    for i in range(10,):
        print(i)

if __name__ == "__main__":
    main()
