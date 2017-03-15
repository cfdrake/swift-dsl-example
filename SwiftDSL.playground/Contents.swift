/*:
# Implementing a Small DSL in Swift

By Colin Drake

I was working on an iOS project earlier today when I ended up running into an annoying bug in my Swift Core Data stack.  My seemingly correct `NSFetchRequest` code was returning `nil` for all inputs, and I couldn't figure out why.

After inspecting and thinking through totally different parts of the codebase, I had an ah-hah moment! I went back to my fetch request code and sure enough I saw something resembling the following:

    let predicate = NSPredicate(format: "%@ = %@", key, value)

Those acquainted with Core Data will probably notice my error pretty quickly: I accidentally formatted `key` as a value type! The [format string](https://developer.apple.com/library/ios/documentation/Cocoa/Conceptual/Predicates/Articles/pSyntax.html) should actually be `%K = %@`, that way Core Data will know that `key` is in fact, a substitutable key. Otherwise, it will simply perform string comparison between the two `String` values.

I lamented that there was no error checking in format strings and quickly moved on. However, I had the idea in the back of my mind that a small, focused, problem-driven language, or [Domain Specific Language](https://en.wikipedia.org/wiki/Domain-specific_language), could easily be built in Swift to aid in basic validation/cleanliness of `NSPredicate` construction.

## Overview
I figured a limited proof of concept DSL for `NSPredicate` construction could be made pretty quickly for "fun" 😉 As a baseline, all I wanted to support was:

- Referencing keypaths and simple `String`-convertible constant values
- Checking keys against values with comparison operators
- Combining expressions with logical operators, such as `AND` and `OR`

My goal syntax was something resembling the following:

    let predicate = NSPredicate("foo" == true && !("bar" == 4))

Luckily for me, this experiment turned out to be relatively straightforward thanks to many of Swift's powerful language features. Let's dive straight into the implementation!

## Keys and Values
We'll start with the most simple object to represent: a keypath. A keypath is simply a string that represents a (potentially nested) property, for example `amountDeposited` or `parent.firstName`. To encode this in our DSL, we'll simply represent a `KeyPath` as a `String`:
*/

import Foundation

typealias KeyPath = String

/*:
The second most simple object type to represent is a constant value. Again, in our simplified DSL, we'll only be supporting values that can be directly represented as strings. Thus, we simply want `ValueType` to be those objects that are `CustomStringConvertible`:
*/

typealias ValueType = CustomStringConvertible

/*:
A more full-featured DSL would not be encoding values into strings, but this representation will suit our small experiment 👍🏼

## Operators
Now we can represent the names and values of things we want to query. However, we need a way to relate the two: this is where operators come into play.

We'll support a small subset of comparison and logical operators for querying data. These can be easily represented by Swift's ever-helpful `RawRepresentable`-backed `enum` type:
*/

enum Comparison: String {
    case EqualTo = "="
    case GreaterThan = ">"
    case LessThan = "<"
    case And = "&&"
    case Or = "||"
}

/*:
Given that we'll eventually want to convert these `Comparison` values into a `String` representation, we'll adhere to `CustomStringConvertible` ourselves. Because we've already defined a nice string for the `rawValue`, we'll simply return that for our implementation.
*/

extension Comparison: CustomStringConvertible {
    var description: String { return rawValue }
}

/*:
**Tip:** We could have easily made calls to `rawValue` in our code later for `String` versions of our operators, but I consider implementing `CustomStringConvertible` a cleaner mechanism. For one, it gives you a free initializer: `let str = String(Comparison.EqualTo)`. But more importantly, it keeps the "storage mechanism" of `Comparison` abstracted out.

## Expressions
The `Expression` type will represent a value or compound comparison of values that we can convert to an `NSPredicate`. In our DSL's simplified model, there are four types of values:

- A property
- A constant
- A negated expression
- A compound expression connecting two subexpressions

These concepts can be directly translated into Swift via yet another `enum`. However, this time we'll be creating a [recursive enumeration](https://developer.apple.com/library/prerelease/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Enumerations.html#//apple_ref/doc/uid/TP40014097-CH12-ID145):
*/

indirect enum Expression {
    case Property(KeyPath)
    case Constant(ValueType)
    case Negated(Expression)
    case Binary(Expression, Comparison, Expression)
}

/*:
The complex cases to explain here are `Negated` and `Binary`.

An `Expression` of type `Negated` is simply a subexpression with an additional tag attached saying that it should be negated. Likewise, `Binary` is composed of two subexpressions related by a `Comparison` operator. In both of these cases, the subexpressions could be anything from a simple `Property` to another `Binary` relation.

Feel free to read up on [Algebraic Data Types](https://en.wikipedia.org/wiki/Algebraic_data_type) if you want to learn more about structuring data like this!

At this point, we have enough ground-level types to _represent_ what we want to eventually generate. Next, let's take a look at how we can build up an `NSPredicate`.

## NSPredicate Creation
Our DSL implementation will create `NSPredicates` by concatenating `String` representations of our query together. Keypaths, constants, and operators are already strings (or string convertible) but how do we stringify an `Expression`?

You might have guessed it: Recursion again!

By defining how to convert the simple cases of `Expression` to a `String` (the `Property` and `Constant` cases), we can implement the more complicated cases on top. Let's take a look at the code:
*/

extension Expression: CustomStringConvertible {
    var description: String {
        switch self {
        case let .Property(key): return key
        case let .Constant(value): return String(describing: value)
        case let .Negated(expression): return "!(\(expression))"
        case let .Binary(lhs, op, rhs): return "(\(lhs)) \(op) (\(rhs))"
        }
    }
}

/*:
Now that we can fully convert an `Expression`, our simplified _representation_ of an `NSPredicate`, to a `String`, we simply need a clean way to pass it to `NSPredicate` itself. To do this, we can define a convenience initializer:
*/

extension NSPredicate {
    convenience init(_ expression: Expression) {
        self.init(format: String(describing: expression))
    }
}

/*:
After all this work, let's stop, take a look, and verify that we've got something that works:
*/

let expr1 = Expression.Property("amount")
let expr2 = Expression.Constant(300)
let expr3 = Expression.Binary(expr1, Comparison.EqualTo, expr2)
let pred = NSPredicate(expr3)  // amount == 300

/*:
Cool, but it's still a lot of typing 😒 Now for the fun part...

## Syntactic Sugar
The main problem with the current state of our DSL is that we have no clean way to create and combine expressions.

Swift's [literal convertible protocols](http://nshipster.com/swift-literal-convertible/) will aid in being able to construct values and keypaths easily, while [operator overloading](http://nshipster.com/swift-operators/#overloading) will provide the basis of a sugary combination syntax.

Literal protocols will allow us to type a value such as `3` and have it automatically converted to `Expression.Constant(3)`, for example. Likewise, it'd be great if strings were automatically interpreted as keypaths. We support this behavior by adhering to a number of different protocols and defining specialized initializers.

I've implemented `Bool`, `String`, and `Integer` conversion in the snippet below - it's pretty much plug 'n' chug mapping values into `Constant`, etc. cases:
*/

extension Expression: ExpressibleByBooleanLiteral, ExpressibleByIntegerLiteral, ExpressibleByStringLiteral {
    typealias ExtendedGraphemeClusterLiteralType = StringLiteralType
    typealias UnicodeScalarLiteralType = StringLiteralType
    
    init(booleanLiteral: Bool) {
        self = .Constant(booleanLiteral)
    }
    
    init(integerLiteral: Int) {
        self = .Constant(integerLiteral)
    }
    
    init(unicodeScalarLiteral value: UnicodeScalarLiteralType) {
        self = .Property(value)
    }
    
    init(extendedGraphemeClusterLiteral value: ExtendedGraphemeClusterLiteralType) {
        self = .Property(value)
    }
    
    init(stringLiteral value: StringLiteralType) {
        self = .Property(value)
    }
}

/*:
Next, we'll use (abuse?) some operators for combining expressions. The most obvious way of implementing a simple, readable DSL for predicates is by overloading the relevant comparison and logical operators to work on `Expression` objects as well.
*/

func ==(lhs: Expression, rhs: Expression) -> Expression {
    return Expression.Binary(lhs, Comparison.EqualTo, rhs)
}

func >(lhs: Expression, rhs: Expression) -> Expression {
    return Expression.Binary(lhs, Comparison.GreaterThan, rhs)
}

func <(lhs: Expression, rhs: Expression) -> Expression {
    return Expression.Binary(lhs, Comparison.LessThan, rhs)
}

func &&(lhs: Expression, rhs: Expression) -> Expression {
    return Expression.Binary(lhs, Comparison.And, rhs)
}

func ||(lhs: Expression, rhs: Expression) -> Expression {
    return Expression.Binary(lhs, Comparison.Or, rhs)
}

prefix func !(expr: Expression) -> Expression {
    return Expression.Negated(expr)
}

/*:
Implementing the syntactic sugar for this DSL ends up being little more than tying an operator with the matching `Expression` case.

As a note, it's always worth thinking whether the above is a good idea, or if you should introduce your own operators instead. The second case should almost always be the preferred solution, but if the semantics of an existing operator fits 100% with your data structures (think a `Vector` type that could work with `+`, etc) then it might be okay to overload.

Now that we've got some of the philosophy out of the way, let's give it a spin:
*/

let bourbonExpr: Expression = ("proof" > 100 && "name" == "Wild Turkey")
let deletedExpr: Expression = ("deleted" == true)

/*:
## Final Example
As a working final example, we'll define a `Person` structure, and filter an `NSArray` of people using an `NSPredicate` constructed in our DSL:
*/

class Person: NSObject {
    let tweets: Int
    let age: Int
    
    init(tweets: Int, age: Int) {
        self.tweets = tweets
        self.age = age
    }
}

// Setup some contrived, inaccurate data ;)
let people: NSArray = [Person(tweets: 250, age: 18),
    Person(tweets: 80, age: 22)]

let tweetsPred = NSPredicate("tweets" > 200 && "age" == 18)
let filtered = people.filtered(using: tweetsPred)

/*:
Looks neat, plus you get some _minor_ validation wins done by the compiler. Note, however, it is still entirely possible to construct an invalid `NSPredicate` with the existing DSL - it's more demo/thought experiment than production-ready library!

Practically speaking, a similar, type-checked, more fleshed out DSL could perhaps end up being useful for Core Data development, where you are limited to the string parsing predicate API. For other applications, however, [block predicates](http://nshipster.com/nspredicate/#block-predicates) or `SequenceType`'s `filter` function will potentially solve the problem better.
*/
