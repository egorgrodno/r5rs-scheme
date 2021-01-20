# R5RS Scheme implementation

Scheme Interpreter. Language specification can be found [here].

## Usage

To start a REPL simply run:

```schell
r5rs-scheme
```
#### Available options:

| Option             | Description      |
| ------------------ | ---------------- |
| *none*             | Interactive mode |
| `--eval,-e SCRIPT` | Evaluate script  |
| `--help,-h`        | Show help text   |
| `--version,-v`     | Show version     |


## Build & Install

Install appropriate GHC version (you only need to do this step once)

```shell
stack setup
```

Then you can build and add `r5rs-scheme` executable to PATH with

```shell
stack build --copy-bins
```

Alternatively, you can build locally and run it with

```shell
stack build
stack exec r5rs-scheme
```

Check out [Stack] documentation for other commands and options.

[here]: https://schemers.org/Documents/Standards/R5RS/HTML/r5rs.html
[Stack]: https://docs.haskellstack.org/en/stable/README/
