using System;

class Program
{
    static void Main()
    {
        string userInput;
        int enteredInteger;
        
        Console.WriteLine("Type 1 and press enter.");
        userInput = Console.ReadLine();
        
        // Trying to assign to function name instead of calling function
        int.Parse = enteredInteger(userInput);
        
        Console.WriteLine("The user entered the integer {0}", enteredInteger);
    }
}
