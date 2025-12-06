# Cofy
Cofy is a modern functional, statically-typed and embeddable programming language.
1. functional because it allows you to escape the horrors of debugging globally mutable state.
2. statically-typed because it allows you to escape the horrors of calling methods on a null value.
3. and embeddable because you can put it inside other applications for easy scripting (lua style).

This document serves as an introduction to the language, below you will find examples of Cofy programs.

## Hello world

```rust
fn main = println("Hello world!");
```
All Cofy programs need a `main` function, it is required to either return a `()` (which is like `None` in Python or `undefined` in Javascript) or type that implements `Monad` (monads will be explained in the next examples)
Here `main` returns an `IO<()>`, which the interpreter deduced from the return type of `println`. In cases where the interpreter gets confused about the type of a function you need to declare a function prototype (like in C or C++):

```rust
// in the first parenthesis youd declare the parameters this function accepts 
// if `main` required any arguments
fn main () => IO<()>; 
fn main = println("Hello world!");
```
## Input/Output
Most programs you write will have to work with user input. However as you may know all operations working with the operating system require you to have at least *some* degree of mutability. So how does IO work in functional programming languages?
We use something called monads, they come from abstract algebra but you can imagine them more as like `Promise` in JavaScript or futures in Python/Rust. A monad starts with some value, and then operations that return a monad of the same type can be chained together, to then be reduced into a single action.
The main example of a monad in Cofy is `IO<T>`, which can be interpreted as a delayed Input/Output action that returns `T` when awaited.

The following code shows you how to write fizz buzz in Cofy.
```rust
// the program can now access the module `io`
import lib.io;

// first we need a function that converts a number into a fizzbuzz string
fn _fizzbuzz_str (int) => string;
// n is a parameter of type `int`
fn _fizzbuzz_str n =
	if n % 15 then 	
		"FizzBuzz"
	else if n % 3 then
		"Fizz"
	else if n % 5 then
		"Buzz"
	else 
		stringify(n); 
	// stringify is a function that converts a number to a string

// fizzbuzz takes a left and a right and returns a single IO action that writes
// goes from `left` to `right` and prints the fizzbuzz of each number to stdout
fn fizzbuzz (int, int) => IO<()>;
fn fizzbuzz left right = 
	// range works pretty much the same as in Python
	// however in cofy it returns an object of type [int]
	// which we call an iterator
	// 
	// for expressions work like `map` in JS/Python, they 
	// convert each member of an iterator into something
	for i in range(left, right) do 
		println(_fizzbuzz_str(i)) 
	// this `for` expression converts a [int] to a [IO<()>]
	// however we still need to reduce it to a single IO action
	// we use `io.group_null` for this, it converts an array of `IO<()>` into 
	// a single `IO<()>`
	=> io.group_null($);
	// as you can see we used a `=>` which is called a pipe
	// it takes the result of the previous expression and puts it inside 
	// the next expression as the constant `$` 

// so now we need to read `left` and `right` from the user
// `read_bounds` will return an action that reads 2 numbers from the user
fn read_bounds () => IO<(int, int)>;
// a `with T` expression makes the function await every value that returns a monad T
// and puts the last expression of the function as this T
fn read_bounds = with IO => 
	// here we use a `let` statement
	// that lets you bind values to names
	// these values are also immutable
	let 
		left = input("input the left bound: "), // input works the same as in python
		right = input("input the right bound: ")
	in 
		(left, right); // this is a tuple!
	// if a let statement is followed by an `in` the values declared in the 
	// `let` statement will only be available inside the `in` expression
	
fn main = with IO
	// here `left` and `right` will exist for the whole function
	=> let (left, right) = read_bounds()
	=> fizzbuzz(left, right);
```
