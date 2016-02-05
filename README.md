# grok-arch-hexagonal

Grokking Alistair Cockburn's [Hexagonal Architecture][1]

grok-arch-hexagonal is not available for download via
quicklisp. Instead, clone the repository, tell ASDF where to find the
system definition, and load the system with quicklisp:

```lisp
(ql:quicklisp "grok-arch-hexagonal")
```

Following that, the application can be driven either by the test

```lisp
CL-USER> (run-test)
```

or by the UI

```lisp
CL-USER> (run-ui)
```

### License

grok-arch-hexagonal is distributed under the MIT license. See LICENSE.




[1]: http://alistair.cockburn.us/Hexagonal+architecture