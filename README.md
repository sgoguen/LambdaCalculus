# Lambda Calculus in F#

This is a fork of [Brian Bern's](https://github.com/brianberns/) [Lambda Calculus implementation in F#](https://github.com/brianberns/LambdaCalculus) with a few enhancements.

## Enhancements

* I wanted to create a function that effectively maps each nutural number to a unique lambda function so that it creates a bijection between all natural numbers and all lambda functions.
* This effectively would let you address lambda functions by their index.
* Added a DeBruijn expression model, which is used as an intermediate step to transform Natual numbers to Lambda functions.

## Todo

1. Fix lambdaToInt - This function converts a DeBruijn indexed function back to a nat
2. Add lambdaToDeBruijn conversion

## Motivation

In the [Unison programming language](https://www.unison-lang.org/), the [big idea](https://www.unison-lang.org/learn/the-big-idea/) is:

```
Each Unison definition is identified by a hash of its syntax tree.

Put another way, Unison code is content-addressed.
```

What Unison does that's important is it does not include the ***names*** in the hash of the function.  Actually, they go further, they separate names of the variables and functions from the function and inject it.

## The Big Idea Here

Rather than identifying a function by the hash of its syntax tree, what if we identified a function by its unique natural number?

* Unlike a hash, using natural number to address is always reversable.
* Addresses are also continguous in a way that smaller addresses represent smaller functions.
* The addresses aren't just content addressable, the addresses are timeless addresses to a complete infinite database that requires no storage.
* Timeless addressing allows one to create a distributed global database where computers can prove things about functions.
* Timeless addressing allows one to create a distributed global database where computers can prove things about functions or their composition.

## References

* Matthew P. Szudzik - [The Rosenberg-Strong Pairing Function (2019)](https://arxiv.org/abs/1706.04129)