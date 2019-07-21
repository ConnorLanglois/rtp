# RTP : Resolution Theorem Prover

A resolution theorem prover capable of proving most logical statements specified in first-order logic.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

```
Lisp
Quicklisp
```

### Installing

Clone the repo.

### Running

1. Add `src/lib/agent` as `agent` to `~/quicklisp/local-projects`

2. Add `src` as `rtp` to `~/quicklisp/local-projects`

3. Run `sbcl --noinform --noprint --disable-debugger --eval '(ql:quickload "rtp" :silent t)' --eval '(in-package :rtp)' --eval '(run-simulator)' --eval '(cl-user::quit)'`

4. Input filename (e.g. `marcus`, `robot`, etc.) of file in `bases`

5. Input theorem to prove (e.g. `(grows John Carrots)`)

## Authors

* **Connor Langlois** - [ConnorLanglois](https://github.com/ConnorLanglois)

## License

This project is not licensed.
