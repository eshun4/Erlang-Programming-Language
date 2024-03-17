# Erlang-Programming-Language
This repository contains the code implementation for Data Structures and different Algorithmic concepts in Erlang Language.

Erlang is a general-purpose language designed for building highly concurrent and fault-tolerant systems.

It was developed at Ericsson in the 1980s.

Erlang is known for its exceptional concurrency capabilities and is favored for building highly reliable and distributed systems.

### MAIN FEATURE
- Concurrency: This is the ability of a system to execute multiple tasks simultaneously, making thus an efficient use of resources. This allows developers to build systems that can handle large number of concurrent tasks.

-Fault Tolerance: This is the ability of a sytem to continue and providing service even in the process of hardware and software failures. The erlang design includes isolated process, supervision prints and the abiity to dynamically restart failed components ensuring system resilience.

- Soft Real-Time: This is where response sytems need to be predictable but not necessarily instantaneous.

- Hot Code Upgrades: This mean we can upgrade parts of our system without shutting it down.

- Functional Programming: This means it reads computaion as the evaluation of mathematical functions.

- Pattern Matching: This is a technique used to confirm data structures and identify specific patterns in them. Erlang has a feature that helps us manipulate these data structures efficiently.

APPLICATIONS
- Telecommunications: Backend Infrastructre of telephone systems, Voice-Over API systems and mobile network elements. The concurrency is crucial in this domain.

- Messaging and Chat Applications: WhatsApp was originall built with Erlang on the backend.

- Online Gaming: It is suitable to build the server infrasture for backend systems.

- Financial Services: It has been used in financial trading systems. It is used in finacncial systems where low-latency and fault-tolerance are critical.

- IoT (Internet of Things) - Lighweight processes and built-in distribution make it suitable for managing IoT devices in a scalable and fault-tolerable manner.

- Healthcare: For realtime communication between patient devices.

- Transportation and Logistics: Tracking of vehicles is one where erlang's concurrency is valuable.

- E-commerce: Many platforms use erlang to handle realtime inventories, order processing and customer support chat. 

- Social Media: It is used on platforms like this for their real-time features like streaming.

#Erlang Compilation

Erlang files are compiled into bytecode, which is executed by the Erlang Virtual Machine (BEAM). Here's an example Erlang file called `hello.erl`

`
% The purpose of this file is to just show how we compile in erlang
% When we create an erlang file with an erl extension we are essentially creating a module
-module(hello).

% We export functions below
% The list contains the name of the function followed by a backslash. The number of input parameters follows the backslash
-export([world/0]).

world() -> 
    io:format("Hello, World!~n").

% The io module is a library in erlang for input and output operations
`
##Explanation
Let's break down each part:
`-module(hello).`

- The -module(hello) directive declares that this file is a module named hello.
- Erlang modules are encapsulated units of code. The filename should match the module name (with the .erl extension).

`-export([world/0]).`

- The -export([world/0]). directive specifies that the world/0 function is accessible from outside the module.
- world/0 means it's a function named world with 0 arguments.

`world() -> 
    io:format("Hello, World!~n").`

- This defines the world/0 function.
- When called, it prints "Hello, World!" to the console followed by a new line (~n).
- `io:format/1` is a function from the io Erlang module used for formatted output.

## Compiling Erlang Code

To compile this Erlang file, you need to use the Erlang compiler called erlc. Here's how you can compile hello.erl:

Navigate to the directory containing hello.erl in your terminal or command prompt.

Compile the file:
Use the erlc command followed by the filename:
`erlc hello.erl`

This command will create a bytecode file named hello.beam.

## Running the Erlang Shell:
After compilation, you can start the Erlang shell (erl command) in the same directory:
`erl`
This will open the Erlang shell where you can interact with the compiled module.

## Using the Module:
Inside the Erlang shell, you can load and use the hello module:
`1> hello:world().
Hello, World!
ok`

`hello:world().` calls the world/0 function from the hello module.

You'll see "Hello, World!" printed to the console.

## Summary

- Erlang files are modules.
- `-module(ModuleName)` declares the module.
-`-export([FunctionName/Arity])` exports functions for use outside the module.
- Function definitions follow the format - `function_name(Parameters) -> Body..`
- `erlc` compiles Erlang files into bytecode.
- `erl` starts the Erlang shell.
- Use `Module:Function().` syntax to call functions from modules in the shell.

# Numbers in Erlang

- Integers: These are whole numbers without a decimal point. Theycan be positive, negative, or zero. For example:
42, -17, or 0

- Floats: These are numbers with a decimal point or in scientific notation. They are used to represent real numbers, including fractions. For example:
3.14159 or 2.0e-3 (which is equivalent to 0.002).

Erlang provides a wide range of arithmetic operations for working with numbers, including addition, subtraction, multiplication, and
division.

Erlang provides a wide range of arithmetic operations for working with numbers, including addition, subtraction, multiplication, and
division.

# Immutable Variables
- We need to store our results somewhere → variables.
- Variables in functional programming cannot change.
- The symbol = is used for pattern matching, not traditional equality.
- Variables are case sensitive: MyVariable and Myvariable are different variables.
- Variables in Erlang must start with an uppercase letter or an underscore _
- After the first character, variable names can consist of letters (uppercase or lowercase), digits, and underscores.

# Atoms
- Atoms are literals → constants whose only value is their own name.
- In other words, what we see is what we get.
- An atom should be enclosed in single quotes (') if it does not begin
with a lowercase letter or if it contains any characters other than
alphanumeric characters, an underscore (_), or an at sign (@).

# Tuple
- A tuple is a way to group together a fixed number of elements.
- It is written in the form {Element1, Element2, ..., ElementN}
- Tuples can also be useful when working with single values.
- A tuple that contains an atom with one element following it is called a tagged tuple.
- If we want to represent a point (x,y) in a Cartesian graph.
- Suppose that we want to store the temperature in different units.

# Lists
- Lists are the bread and butter of functional programming languages.
- They can contain anything: numbers, atoms, tuples, other lists, etc. The basic notation is [Element1, Element2 , … , Elementn].
- A list can contain elements of different type.
To glue lists together, we use the ++ operator.
Lists
- To remove elements from a list, we use the -- operator. - Both ++ and -- are right-associative
- The first element of a list is named the head.
Head and Tail
- The rest of the list is named the tail.
In Erlang we can separate the head from the tail using the notation:
`[Head|Tail]` - The | we used is called the cons operator (constructor).

# List Comprehensions
- They serve to build or modify lists → rich and beautiful way.
- They are based on the mathematical idea of set notation.
- Set notation describes how to build a set by specifying properties its members must satisfy.
- Like set notation, list comprehensions are about building sets from other sets.
- For example, given the set {2n : n ∈ L} where L is the list [1,2,3,4] we could read:
`for all n values in [1,2,3,4], give me n*2`
- The set built from this would contain the elements 2,4,6,8 
- We can also add constraints to a list comprehension by using operations that return Boolean values.
- The recipe for list comprehensions in Erlang is as follows:
`NewList = [Expression || Pattern <- List, Condition1, Condition2, ...ConditionN]`
- Useful when we want to apply a function to each element of a list, forcing it to respect constraints.
- Consider we own a restaurant. A customer enters, sees our menu, and asks if he could have the prices of all the items costing between $3 and $10, with taxes (say 8 percent) counted in afterward.
- This is our menu:
[{steak, 6.99}, {coke, 2.95}, {rice, 3.20}, {fish, 13.95}, {water, 0.00}].
- Suppose we have this weather information:
`Weather = [{toronto, rain}, {montreal, storms}, {london, fog}, {paris, sun}, {boston, fog}, {vancouver, snow}]`
- How can we have the list of cities where weather is fog?
- The | we used is called the cons operator (constructor).
- If we want to represent a point (x,y) in a Cartesian graph.
- Variables are case sensitive: MyVariable and Myvariable are different variables.
- Variables in Erlang must start with an uppercase letter or an underscore _ Notions about Variables
- After the first character, variable names can consist of letters (uppercase or lowercase), digits, and underscores.























