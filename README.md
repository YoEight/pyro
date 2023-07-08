<h1 align="center">Pyro</h1>
<p align="center">
  <i>· π-calculus programming language ·</i>  
</p>

<p align="center">
    <img src="https://github.com/YoEight/pyro/assets/144545/7cebd746-09de-495c-a381-6d20e1b5c4ae" />      
</p>

## Key features

* Functional oriented (mutation is not allow for now)
* Strong nominal type system with inference for non top-level declarations.
* Type checking can be disabled, leading type errors or missing variables to raise exceptions at runtime.

## Syntax

Current `Pyro` syntax is very close to the Pict programming language however, this might change in mid-long term. A syntax closer to OCAML might be implemented in the future. See [Pict Tutorial] for exhaustive examples and more.

## Example

```
run
    (def for [min: Integer max: Integer f:![Integer ^[]] done: ^[]] =
        (def loop x:Integer =
            if (<= x max) then
                (new c : ^[]
                ( f ! [x c]
                | c?[] = loop!(+ x 1)))
            else
                done ! []
        loop ! min )
    (new done : ^[]
     ( for! [1 4
            \[x c] = (print ! x | c ! [])
            done]
     | done?[] = print ! "Done!")))
```

When executed, that program should produce the following output:

```
1
2
3
4
"Done!"
```

## Getting Started

This repository contains an embeddable, a standalone runtime and a REPL. The codebase is entirely based on the Rust programming language. Version 1.70+ has been used to build the project but earlier versions of the compiler can work too.

* `pyro`: Standalone interpreter
* `pyro-core`: Common types but also contains the lexer, parser, inferencer and the type checker.
* `pyro-runtime`: Embeddable interpreter.
* `pyro-repl`: CLI-based REPL.

You can build the whole codebase by running the following command:
```
$ cargo build
```

## About

π-calculus is a theoretical model for concurrent computation that was developed by Robin Milner around the late 20th century.
It's a mathematical framework used to describe and analyze the interactive behaviors of concurrent systems, where multiple computations are executing simultaneously and can interact with each other.

In `Pyro`, computations are modeled as processes that communicate by passing messages through channels. The core features of `Pyro` are the ability to dynamically create new communication channels and to treat channels as first-class values that can be sent as part of messages. This allows `Pyro` to express dynamic network topologies, where the interconnections between components can change over time, which is a key aspect of distributed computing environments.

The central idea is that these processes can not only send and receive information but also alter the network of communication. For instance, a process might create a new channel and send its name to other processes, which can then use this channel for future communication. This provides a powerful mechanism for expressing complex communication patterns.

## Inspiration

`Pyro` draws significant inspiration from the Pict programming language, one of the earliest implementations of the π-calculus theory in the form of a practical programming language. You can find more about the Pict programming language on the [Pict Homepage]. You can also find a [Pict presentation slides] and the [Pict tutorial] I used to implement `Pyro`.

[Pict Homepage]: https://www.cis.upenn.edu/~bcpierce/papers/pict/Html/Pict.html
[Pict presentation slides]: https://www-sop.inria.fr/mimosa/Pascal.Zimmer/mobility/pict.pdf
[Pict tutorial]: https://www.cs.rpi.edu/academics/courses/spring04/dci/picttutorial.pdf
