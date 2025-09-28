using System;

class Program
{
    static void Main()
    {
        string userInput;
        
        Console.WriteLine("Type 1 and press enter.");
        userInput = Console.ReadLine();
        
        // Function name has wrong capitalization
        int enteredInteger = int.parse(userInput);
        
        Console.WriteLine("The user entered the integer {0}", enteredInteger);
    }
}
