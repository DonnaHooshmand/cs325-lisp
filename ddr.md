# Deductive retrieval

https://courses.cs.northwestern.edu/325/readings/deductive-retrieval.php

DDR is a generalisation of simple data retrieval.

A deductive retriever adds the following operations:

- storing rules in the database
- using those rules to infer answers to queries, in addition to the assertions explicitly stored

DDR has three major processes:

- unification -- generalized pattern matching
- backward chaining -- linking rules together to form a conclusion
- trying all possibilities until one or all answers are found

## Pattern Matching

A pattern can be any form, e.g. `(in tiger savannah)`, but usually a pattern will have avariables, e.g. `(in ?x savannah)`. A form without variables is a _constant form_.

A form matches a pattern all corresponding elements of the form and pattern can be matched. Constant elements match each other if they are equal. Variable elements can match anything, but if multiple occurrences of the same variable must match the same thing.

The result of a match is a _list_ of binding lists. A binding list is a list of `(variable value)` pairs, recording each pattern variable and the corresponding form of element.

For example, the result of matching `(in ?x savannah)` with `(in tiger savannah)` is `(((?x tiger)))`.

With simple patterns, the result of a match will be either `NIL` or a list of one binding list. It is possible to extend patterns in such a way that multiple bindings can occur, i.e., there is more than one way the pattern can match the form.

In particular, matching `(in tiger savannah)` with `(in tiger savannah)` will produce `(nil)`, while matching `(in tiger savannah)` with `(in tiger swamp`A will produce `nil`.

### Unification

Unification introduces one additional feature to normal pattern matching. It allows a pattern to match a pattern. This raises a number of complexities. For example, a variable can bind to another variable, or even to itself. Fortunately, these issues have been worked out long ago, and the code for implementing unification, while subtle and subject to pitfalls, is not all that complicated. It can be found in the `unifly.lisp` file.

Pattern matching against patterns is important for backward chaining.

## Assertions and rules

The knowledge base for our deductive retriever contains:

- simple assertions, e.g. `(in tiger savannah)`
- backward chaining rules, e.g. `(<- (near ?x grass (in ?x savannah))`
- forward chaining rules, e.g. `(-> (predator-of ?x ?y) (prey-of ?y ?x))`

Assertions and rules are stored using `(tell form)`, e.g.,

```lisp
(tell '(at tiger savannah))
(tell '(<- (near ?x grass) (at ?x savannah)))
(tell '(-> (predator-of ?x ?y) (prey-of ?y ?x)))
```

### Logical terms

The argument of predicates, such as `near` or `at`, are called terms. Terms in logic can be

- Simple constants, e.g. `tiger`
- Variable names, e.g., `?x`
- Functional terms, e.g.,`(father-of john)`

Functional terms may look like assertions but they most definitely are not. An assertion is like a sentence, e.g., `(at tiger savannah)` is "The tiger is in the savannah." An assertion can be true or false.

A functional term is like a noun phrase, e.g., `(father-of john)` is "the father of John." A functional term refers to something, but is neither true nor false. It's like the difference between "the doc" and "that is a dog". The former is a reference and the latter is an assertion.

Functional terms in logic are not like functions in Lisp. They are not executable. They don't return values. They are simply ways to construct a representation of something, e.g., `(date 2015 10 13)` for "the date October 13, 2015."

Functional terms turn out to give deductive retrieval super powers.

## Answering Queries

A query is asked by typing `(ask query)`. The query can be any form.

If the query is a constant form, then we're effectively asking a yes or no question. For example, to ask "Is the tiger near grass?" we type:

```
> (ask '(at tiger savannah))
((AT TIGER SAVANNAH))
```

The result is a list of answers to the query found in the KB. There's just one answer to this query.

### Backward Chaining

Now consider a query very similar to the previous one:

```
> (ask '(near tiger grass))
((NEAR TIGER GRASS))
```

The query is still a "yes/no" question, and the answer still says "yes" but `(near tiger grass)` was not explicitly stored in the knowledge base. It was inferred using the backward chaining rule "something is near grass if it is in a savannah."

In this case, only one rule was needed to derive the answers. In general, though, many rules may be chained together.

For example, let's add a rule that says that if you're near something, you can bite it:

```
> (tell '(<- (can-bite ?x ?y) (near ?x ?y)))
```

Now let's ask if the tiger can bite grass:

```
> (ask '(can-bite tiger grass))
((CAN_BITE TIGER GRASS))
```
