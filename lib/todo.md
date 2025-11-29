parser:
1. traits (done)
2. types (done)
3. traits in type parameters
4. lambda functions (done)
5. impl blocks (done)
6. export blocks (done)
7. constructors (done)
8. element access (done)
9. alias (done)
10. data (done)
11. import blocks (done)

code quality:
1. make `Ast` a `WithPos<AstRepr>`
2. replace all `current().ok_or_else(|| eof_error)` with `current_or_eof()` and etc
3. add `const` where possible
