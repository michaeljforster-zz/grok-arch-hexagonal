# grok-arch-hexagonal

Grokking Alistair Cockburn's [Hexagonal Architecture][1]

grok-arch-hexagonal includes the Alistair Cockburn's FIT example and
Pat Maddox's [hexarch2][2] example.

grok-arch-hexagonal is not available for download via
quicklisp. Instead, clone the repository, tell ASDF where to find the
system definition, and load the system with quicklisp:

```lisp
(ql:quicklisp "grok-arch-hexagonal")
```

Following that, the FIT example can be driven either by the test

```lisp
CL-USER> (grok-arch-hexagonal-fit-example:run-test)
```

or by the UI

```lisp
CL-USER> (grok-arch-hexagonal-fit-example:run-ui)
```

The hexarch2 example can be driven by the test

```lisp
CL-USER> (grok-arch-hexagonal-hexarch2:run-test)
```

### License

grok-arch-hexagonal is distributed under the MIT license. See LICENSE.

[1]: http://alistair.cockburn.us/Hexagonal+architecture
[2]: https://github.com/patmaddox/hexarch2
