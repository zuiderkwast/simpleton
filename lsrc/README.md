lsrc: A Lesser compiler
=======================

This is an implementation of the Lesser language.  It is written in Erlang and
orginized as an Erlang/OTP application.

Build the compiler with `rebar compile`.  (You may need to get Rebar, if it's
not included in this repo.)

Compile a Lesser program with `lsrc`.  The `escript` (Erlang script)
interpreter (included in Erlang/OTP) is required to run `lsrc`.

Directory structure:

* `src/` Compiler source code, Erlang
* `priv/` Other files that belong to the compiler
* `runtime/` The C runtime, independent from the compiler

See ROADMAP.md for the status and progress of each feature.

