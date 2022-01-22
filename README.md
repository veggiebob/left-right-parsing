### Demo

Run `cargo run`.

Try inputting some examples and observe the AST output:

`add_one [x: nat] => x + 1`

```
factorial [x: nat] => if x=0 1 else go x
        where {
            go [y: nat] => y * (go [y-1])
        }
```

Read below for more information.

#### Notation

| thing being described   | notation              |
|-------------------------|-----------------------|
| expression              | `<expr>`              |
| statement               | `<stmt>`              |
| literal text            | `literal text`        |
| identifier              | `<var>`               |
| continuation of pattern | `...`                 |
| optional section        | `[[<other symbols>]]` |
| any amount or no space  | ` `                   |
| at least one space      | `_`                   |


### Example Structures

#### Statements

Notice flexibility in spacing! The only space required in statements is right after the keyword `let`.
Additionally, any amount and any kind of space is allowed! Tabs, newlines, or any length of space can be added anywhere that there is a space as per the notation.

`let_<var> = <expr>`  

```
<var> [arg1 : type1, arg2 : type2, ..., argn : typen] => <expr> 
[[
    where {
        <stmt>
        <stmt>
        ...
    }
]]
```

#### Expressions

##### Infix Expressions

For any operator defined as any string `<op>`, an infix may be parsed as `<expr> <op> <expr>`
For example, `3 * 4`, or `3 operator! 4` or `1operator!2`, although that last one may be a bit confusing to the user.

##### Strings

Escaped quotes are not allowed, just for simplicity's sake. It's something I can add later easily.
Anything starting with `"` is considered the start of a string, and the string lasts all the way until the next `"` (if there is one).

##### Numbers

Parsing only for whole numbers, including 0-leading numbers.

##### Parenthesis

Do exactly what you'd expect.
`(<expr>)` is parsed exactly how `<expr>` is parsed.

##### If-else

```
if <expr> <expr> else <expr>
```
Note that, once again, all spaces are optional. So, it is perfectly reasonable to have an if-else chain without the need for "special" syntax.

For example, `if x x elseif y y else z` could be decomposed as
`if x x else (if y y else z)` which, written in a more traditional manner, might look like
```
if x {
    x
} else if y {
    y
} else {
    z
}
```
in another language. In fact, nearly all keywords are merely to provide context to the *user*, not to the program.

##### Lists

List parsers are allowed to be constructed with any separator `<sep>` defined as a single `char`. They are defined like usual:
```
[<expr1> <sep> <expr2> <sep>  ... <sep> <exprn>]
```
would be parsed as a list containing `expr1, expr2, ..., exprn`
Lists can also be empty `[]`.

Consider a list parsed using `' '` as a separator:

`["hello world" 3 + 4 x]`, given the fact that `+` is not an expression on its own, would be parsed in 3 elements:
`"hello world"`, `3 + 4`, and `x`

#### Notes

Note that these are only the parsing expressions implemented, in the structure of Statement-level and Expression-level
parsing. This makes the most sense to me in terms of representation of an AST you'd see
in a production programming language. However, the parsers implemented are in no way bound by any rules
other than the fact that **tokenizing is discouraged** and everything should be read **exclusively
left-to-right.**

Also note that because ambiguity is a **feature**, some parsed statements or expressions may have multiple solutions. 
This is expected in many cases, because there is no goal for an end in ambiguity. Specificity can always be added
with parentheses if necessary, but the flexibility in syntax is never taken away.
These multiple solutions are shown as a hashset containing every unique solution.

This is especially obvious in the factorial example, since there is an infix operator with
the infix string being `" "` (that being a space). No surprise, this causes lots of ambiguities!
However, it also allows for the "function calling" syntax as seen in the example without any other syntax specification.

If it were part of a real programming language, these different possibilities as results would be paired with some 
sort of semantic evaluation program that rejects some "variables" or "identifiers" that might have been parsed
that aren't actually variables or valid identifiers. Those would be ruled out at a later stage. 