# pyro

<div align="center">


  A π-calculus programming language and interpreter


</div>

[![asciicast](https://asciinema.org/a/4XCoctDj8Ugk65w4sVzgWrCAf.svg)](https://asciinema.org/a/4XCoctDj8Ugk65w4sVzgWrCAf)

## About

π-calculus is a theoretical model for concurrent computation that was developed by Robin Milner around the late 20th century.
It's a mathematical framework used to describe and analyze the interactive behaviors of concurrent systems, where multiple computations are executing simultaneously and can interact with each other.

In `Pyro`, computations are modeled as processes that communicate by passing messages through channels. The core features of `Pyro` are the ability to dynamically create new communication channels and to treat channels as first-class values that can be sent as part of messages. This allows `Pyro` to express dynamic network topologies, where the interconnections between components can change over time, which is a key aspect of distributed computing environments.

The central idea is that these processes can not only send and receive information but also alter the network of communication. For instance, a process might create a new channel and send its name to other processes, which can then use this channel for future communication. This provides a powerful mechanism for expressing complex communication patterns.
