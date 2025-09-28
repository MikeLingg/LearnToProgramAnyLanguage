using System;

class Program
{
    static void Main()
    {
        string userInput;
        int enteredInteger;
        
        Console.WriteLine("Type 1 and press enter.");
        userInput = Console.ReadLine();
        
        // Function called but return value ignored
        int.Parse(userInput);
        
        // enteredInteger was never assigned a value
        Console.WriteLine("The user entered the integer {0}", enteredInteger);
    }
}
