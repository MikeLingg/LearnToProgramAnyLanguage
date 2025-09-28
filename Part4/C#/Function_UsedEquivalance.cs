using System;

class Program
{
    static void Main()
    {
        string userInput;

        Console.WriteLine("Type 1 and press enter.");
        userInput = Console.ReadLine();

        // Using == (comparison) instead of = (assignment)
        int enteredInteger == int.Parse(userInput);

        Console.WriteLine("The user entered the integer {0}", enteredInteger);
    }
}
