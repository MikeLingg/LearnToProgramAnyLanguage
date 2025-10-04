def main():
    score = 85
    value = 5

    # ERROR 1: Missing colon after if
    if score >= 90
        print("You got an A")

    # ERROR 2: Missing colon after elif
    if score >= 90:
        print("A")
    elif score >= 80
        print("B")

    # ERROR 3: Missing colon after else
    if score >= 90:
        print("A")
    else
        print("Not A")

    # ERROR 4: Using 'else if' instead of 'elif' (two words)
    if score >= 90:
        print("A")
    else if score >= 80:
        print("B")

    # ERROR 5: Using 'elseif' (one word) instead of 'elif'
    if score >= 90:
        print("A")
    elseif score >= 80:
        print("B")

    # ERROR 6: Using 'elsif' instead of 'elif'
    if score >= 90:
        print("A")
    elsif score >= 80:
        print("B")

    # ERROR 7: Inconsistent indentation
    if score >= 90:
        print("A")
       print("Great job")  # Wrong indentation level

    # ERROR 8: elif without preceding if
    elif score >= 80:
        print("B")

    # ERROR 9: else without preceding if
    else:
        print("Failed")

    # ERROR 10: Missing colon after match (Python 3.10+)
    match value
        case 1:
            print("One")
        case _:
            print("Other")

    # ERROR 11: Missing colon after case
    match value:
        case 1
            print("One")
        case _:
            print("Other")

    # ERROR 12: Using 'switch' instead of 'match'
    switch value:
        case 1:
            print("One")
        case _:
            print("Other")

    # ERROR 13: Using 'default' instead of 'case _'
    match value:
        case 1:
            print("One")
        default:
            print("Other")

    # ERROR 14: Using braces instead of indentation
    if score >= 90 {
        print("A")
    }

    # ERROR 15: Using 'then' keyword
    if score >= 90 then:
        print("A")

    # ERROR 16: Missing indentation after if
    if score >= 90:
    print("A")

    # ERROR 17: Using semicolon to separate statements on same line without proper syntax
    if score >= 90: print("A"); print("Great")  # This actually works in Python, remove this one

    # ERROR 18: Mixing tabs and spaces (inconsistent indentation)
    if score >= 90:
        print("A")  # spaces
	print("Great")  # tab

    # ERROR 19: case outside of match
    case 1:
        print("One")

    # ERROR 20: Using break in match (Python doesn't need/use break in match)
    match value:
        case 1:
            print("One")
            break
        case _:
            print("Other")

if __name__ == "__main__":
    main()
