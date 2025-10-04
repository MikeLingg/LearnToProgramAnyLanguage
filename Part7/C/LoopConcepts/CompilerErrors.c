#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

int main()
{
    // ERROR: Missing parentheses around for loop parameters
    printf("Missing parentheses in for loop\n");
    for int i = 0; i < 10; i++
    {
        printf("%d\n", i);
    }

    // ERROR: Using assignment (=) instead of comparison (==) in condition
    int counter = 0;
    while (counter = 10)
    {
        printf("%d\n", counter);
        counter++;
    }

    // ERROR: Missing braces and semicolon creates wrong association
    for (int i = 0; i < 5; i++)
        printf("Loop iteration\n");
        printf("This should be in loop\n");

    // ERROR: Using single = instead of == for comparison
    int value = 5;
    while (value = 0)
    {
        printf("This won't print\n");
        value--;
    }

    // ERROR: Missing increment in for loop
    for (int i = 0; i < 10)
    {
        printf("%d\n", i);
    }

    // ERROR: Missing condition in for loop
    for (int i = 0; ; i++)
    {
        printf("%d\n", i);
    }

    // ERROR: Using wrong increment operator (-- instead of ++)
    for (int i = 0; i < 10; i--)
    {
        printf("%d\n", i);
    }

    // ERROR: Missing condition expression entirely
    int count = 0;
    while ()
    {
        count++;
        if (count > 10) break;
    }

    // ERROR: Using 'do' without 'while' at the end
    int num = 0;
    do
    {
        printf("%d\n", num);
        num++;
    }

    // ERROR: Extra semicolon after while in do-while
    int x = 0;
    do
    {
        printf("%d\n", x);
        x++;
    }
    while (x < 5);
    ;

    // ERROR: Missing semicolon after do-while
    int y = 0;
    do
    {
        printf("%d\n", y);
        y++;
    }
    while (y < 5)

    // ERROR: Using curly braces in for loop parameter section
    for {int i = 0; i < 10; i++}
    {
        printf("%d\n", i);
    }

    // ERROR: Undeclared variable (missing type declaration)
    for (i = 0; i < 10; i++)
    {
        printf("%d\n", i);
    }

    // ERROR: Missing opening brace
    for (int i = 0; i < 10; i++)
        printf("%d\n", i);
    }

    // ERROR: Missing closing brace
    for (int i = 0; i < 10; i++)
    {
        printf("%d\n", i);

    // ERROR: Using := instead of = for assignment
    int idx := 0;
    while (idx < 10)
    {
        printf("%d\n", idx);
        idx++;
    }

    // ERROR: Missing parentheses around while condition
    int k = 0;
    while k < 10
    {
        printf("%d\n", k);
        k++;
    }

    // ERROR: Using 'then' keyword (from other languages)
    int m = 0;
    while (m < 10) then
    {
        printf("%d\n", m);
        m++;
    }

    // ERROR: Using ++ twice in increment
    for (int i = 0; i < 10; ++i++)
    {
        printf("%d\n", i);
    }

    // ERROR: Missing opening parenthesis in while
    int n = 0;
    while n < 10)
    {
        printf("%d\n", n);
        n++;
    }

    // ERROR: Missing closing parenthesis in while
    int p = 0;
    while (p < 10
    {
        printf("%d\n", p);
        p++;
    }

    // ERROR: Using 'end' or 'end for' (from other languages)
    for (int i = 0; i < 10; i++)
    {
        printf("%d\n", i);
    }
    end for;

    // ERROR: Trying to use range-based for loop with 'in' keyword
    int arr[] = {1, 2, 3, 4, 5};
    for (int element in arr)
    {
        printf("%d\n", element);
    }

    // ERROR: Using 'foreach' keyword (not valid in C)
    foreach (int element in arr)
    {
        printf("%d\n", element);
    }

    // ERROR: Multiple semicolons in for loop
    for (int i = 0;; i < 10;; i++)
    {
        printf("%d\n", i);
    }

    // ERROR: Using equals (==) instead of assignment (=) in initialization
    for (int i == 0; i < 10; i++)
    {
        printf("%d\n", i);
    }

    // ERROR: Missing semicolon after initialization
    for (int i = 0 i < 10; i++)
    {
        printf("%d\n", i);
    }

    // ERROR: Missing semicolon after condition
    for (int i = 0; i < 10 i++)
    {
        printf("%d\n", i);
    }

    // ERROR: Using 'continue' with label (not supported in C)
    for (int i = 0; i < 10; i++)
    {
        if (i % 2 == 0)
            continue outer;
        printf("%d\n", i);
    }

    // ERROR: Using 'break' with label (not supported in C)
    outer:
    for (int i = 0; i < 10; i++)
    {
        for (int j = 0; j < 10; j++)
        {
            if (j == 5)
                break outer;
        }
    }

    // ERROR: Declaring variable inside while condition
    while (int z = getchar())
    {
        printf("%c", z);
    }

    // ERROR: Using += with incorrect spacing
    for (int i = 0; i < 10; i+ =1)
    {
        printf("%d\n", i);
    }

    // ERROR: Trying to declare multiple different types in one for loop
    for (int i = 0, float j = 0.0; i < 10; i++, j++)
    {
        printf("%d %f\n", i, j);
    }

    // ERROR: Missing comma between multiple variable declarations
    for (int i = 0 j = 0; i < 10; i++, j++)
    {
        printf("%d %d\n", i, j);
    }

    // ERROR: Using 'loop' keyword (from Ada)
    loop (int i = 0; i < 10; i++)
    {
        printf("%d\n", i);
    }

    // ERROR: Using colon instead of semicolon
    for (int i = 0: i < 10: i++)
    {
        printf("%d\n", i);
    }

    // ERROR: Square brackets instead of parentheses
    for [int i = 0; i < 10; i++]
    {
        printf("%d\n", i);
    }

    // ERROR: Using float in loop comparison (implicit conversion warning/error)
    for (int i = 0; i < 10.5; i++)
    {
        printf("%d\n", i);
    }

    // ERROR: Using -> instead of ++ for increment
    for (int i = 0; i < 10; i->)
    {
        printf("%d\n", i);
    }

    // ERROR: Missing 'do' keyword in do-while
    int val = 0;
    {
        printf("%d\n", val);
        val++;
    }
    while (val < 5);

    // ERROR: Using 'repeat' and 'until' (from Pascal)
    int r = 0;
    repeat
    {
        printf("%d\n", r);
        r++;
    }
    until (r >= 10);

    // ERROR: Nested for without proper bracing causing declaration in wrong scope
    for (int i = 0; i < 3; i++)
    for (int j = 0; j < 3; j++)
    int product = i * j;
    printf("%d\n", product);

    // ERROR: Using double equals in increment
    for (int i = 0; i < 10; i == i + 1)
    {
        printf("%d\n", i);
    }

    // ERROR: Conflicting increment/decrement operators
    for (int i = 0; i < 10; ++i--)
    {
        printf("%d\n", i);
    }

    // ERROR: Using 'as' keyword for type (from other languages)
    for (i as int = 0; i < 10; i++)
    {
        printf("%d\n", i);
    }

    // ERROR: Using 'elsif' or 'elif' in loop (wrong context/language)
    for (int i = 0; i < 10; i++)
    elsif (i % 2 == 0)
    {
        printf("Even\n");
    }

    // ERROR: Missing while keyword, just condition
    int w = 0;
    (w < 10)
    {
        printf("%d\n", w);
        w++;
    }

    return 0;
}
