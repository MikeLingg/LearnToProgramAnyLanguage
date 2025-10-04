// C# Function Errors - All errors active for compilation

using System;

class CSharpFunctionErrors
{
    // Error: Missing return type
    PrintHello()
    {
        Console.WriteLine("Hello");
    }

    // Error: Missing semicolon after statement in method
    void SayHi()
    {
        Console.WriteLine("Hi")
    }

    // Error: Using 'function' keyword instead of method declaration
    function void Greet()
    {
        Console.WriteLine("Greetings");
    }

    // Error: Missing return statement in non-void method
    int GetNumber()
    {
        int x = 5;
    }

    // Error: Returning value from void method
    void ProcessData()
    {
        return 42;
    }

    // Error: Return type doesn't match returned value
    int GetValue()
    {
        return 3.14;
    }

    // Error: Using 'call' keyword before method name
    void MyMethod()
    {
        Console.WriteLine("Test");
    }

    void Test()
    {
        call MyMethod();
    }

    // Error: Missing parentheses in method call
    void Display()
    {
        Console.WriteLine("Display");
    }

    void Run()
    {
        Display;
    }

    // Error: Using 'none' instead of 'void'
    none PrintMessage()
    {
        Console.WriteLine("Message");
    }

    // Error: Parameter type after parameter name
    void SetValue(count int, price float)
    {
        Console.WriteLine($"{count} {price}");
    }

    // Error: Missing parameter type
    float CalculateTotal(size, int toppings)
    {
        return 15.99f;
    }

    // Error: Using 'default' keyword incorrectly for default parameter
    void OrderPizza(string size default "medium", int toppings)
    {
        Console.WriteLine("Order placed");
    }

    // Error: Default parameter before required parameter
    void MakePizza(string size = "large", int toppings, bool delivery = false)
    {
        Console.WriteLine("Making pizza");
    }

    // Error: Using := for named arguments
    void SetPizza(string size, int count)
    {
        Console.WriteLine($"{size} {count}");
    }

    void Order()
    {
        SetPizza(size := "large", count := 3);
    }

    // Error: Using semicolon instead of comma between parameters
    void Configure(int width; int height; bool fullscreen)
    {
        Console.WriteLine("Configured");
    }

    // Error: Missing comma between parameters in definition
    int AddNumbers(int a int b)
    {
        return a + b;
    }

    // Error: Using 'begin' and 'end' instead of braces
    void Calculate() begin
        int x = 5;
    end

    // Error: Semicolon after method header before opening brace
    void ProcessValue(); 
    {
        int x = 10;
    }

    // Error: Wrong number of arguments in method call
    int Add(int a, int b)
    {
        return a + b;
    }

    void CallAdd()
    {
        int result = Add(5);
    }

    // Error: Using 'with parameters' syntax
    void DoSomething()
    {
        Console.WriteLine("Done");
    }

    void Execute()
    {
        call DoSomething with parameters ();
    }

    // Error: Parameter name in method call without colon
    void CreateOrder(string product, int quantity)
    {
        Console.WriteLine($"{product}: {quantity}");
    }

    void PlaceOrder()
    {
        CreateOrder(product = "Pizza", quantity = 2);
    }

    // Error: Missing parentheses in method declaration
    void ShowMessage
    {
        Console.WriteLine("Message");
    }

    // Error: Using 'procedure' instead of 'void'
    procedure DisplayInfo()
    {
        Console.WriteLine("Info");
    }

    // Error: Using 'return' outside of method
    int globalValue = 10;
    return globalValue;

    static void Main(string[] args)
    {
        Console.WriteLine("Main");
    }
}
