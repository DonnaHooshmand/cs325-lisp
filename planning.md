## The Planning Problem

Here's a simpmlified statement of the _planning problem_.

- You have a current state of the world.
- You have a desired state of the world.
- Find a sequence of actions that, if executed, should change the world from its current state into the desire state.

A _planner_ is a program that can solve planning problems.

Military planning is an obvious, and long-studies, example of planning. It's particularly challenging because it mixes not only planning, which is hard enough, but game playing against an uncontrollable opponent.

## Deductive Rules for Planning

Using just our simple backward chainer, we can define a general planning framework, that involves a few basic concepts, and two core deductive rules.

There are three core predicates that we need to define rules for.

- `(PLAN plan start-state goal-state)`: This asserts that executing _plan_ would lead from start-state to goal-state
- `(RESULTS action current-state result-state)`: This asserts that doing action in _current_state_ leads to result-state.
- `(STEP action current-state goal-state)`: This asserts that _action_ is a good step in a plan to get from current-state to goal-state.

Actions and states are things you need to design when creating a planner. Logically, they will be functional terms. You can define them any way you want but there are some design constraints.

### States

States are functional terms. They should be designed so that there is just one way to describe any given state, backward chaining will have to explore all of them, even though the answer will be the same in every case.

For example. in `Blocks World`, we could represent a stack of blocks with the term `(and (on a b) (on b c) (on c table))`. This is a bad representation, because there are six different ways to describe the same state. If we represent the stack of blocks with the term `(cons a (cns b (cons c nil)))` -- remember, `cons` here is NOT the Lisp function -- then there is only one way to describe that state.

### Actions

Actions are also functional terms. They are relatively simple to design. They can be simple names, like `JUMP`, if the actor is implicitly understood to be the robot, or they can beterms specifying parameters, e.g., `(PUT-ON x y)`.

### Results

For each action, you have to define one or more rules that say how that action changes the state of the world. These rules use the predicate `RESULTS` to make assertions of the form `(results action current-state result-state)` to describe what happens. In simple domains, these rules are normally pretty simple.

### Step selection

Action result rules are not enough. The planner can't just try every action and see what happens. That is very inefficient, and can easily lead to endless loops.

`STEP` rules are used to pick what action to do in a given state of the world. This is where the intelligence of your planner reside. These at the least must avoid infinite loops, while still finding solutions, if there are any.

### Plan construction

Finally there are `PLAN` rules. These say how to use the `STEP` and `RESULTS` rules to find plans. Surprisingly, just two general rules can do basic planning. You normally don't need to define any more `PLAN` rules.

The first rule is trivial. It says "the empty plan works if the current state is the desired state." To represent the empty plan, we'll use the symbol `NIL`. We could have picked `DONE` or something else.

```
(<- (plan nil ?goal ?goal))
```

The second rule is where most of the work occurs. It needs to say "a plan from current state to the desired state is one that has an action for current state, followed by a plan that gets from the result of that action to the desired state."

To represent a sequence of actions, we'll use Lisp list notation. `(CONS action1 (CONS action2 ... NIL))` represents the plan "first do _action1_ then do _action2_ then ..."

```
(<- (plan (cons ?action ?actions) ?current ?goal)
    (step ?action ?current ?goal)
    (results ?action ?current ?result)
    (plan ?actions ?result ?goal))
```

This may take a bit of study. In a typical planning query, `?current` and `?goal` will be constant state descriptions, but the first argument, the plan, will be an unbound variable. The goal of the deductive planner is to prove the query by constructing a plan that makes it true, if possible.
