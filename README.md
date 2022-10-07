# Turing Machine Programming Language (TMPL)

# Usage

To build:
```
make build
```

To run `filename.tm`:
```
./turing filename.tm
```

# Syntax
```
[state] [pattern1] [action1] [next_state1];
        [pattern2] [action2] [next_state2];
        ...
```

Where:
- **[state]** - the name of the state (string of alphanumeric characters)
- **[pattern\*]** - character to match with the head of the tape
- **[action\*]** - list of actions (separate by commas) where actions can be:
	- **L** - move tape head to the left
	- **R** - move tape head to the right
	- **H** - tape head doesn't move (placeholder if you don't want to do anything)
	- **Pc** - prints the character 'c' at the head position on the tape
- **[next_state\*]** - the name of the next state 

Comments can be made on separate lines or at the end of a line with `//` (similar with C/C++/Java etc.).

You can separate cases ([pattern] [action] [next_state] pairs) with newlines or `;` character.

Alphabet: You can use any ASCII character for your alphabet with the exceptions of:
- `*` (wildcard) - if used as a pattern it will match any character

**NOTE**: `#` is the empty symbol, any tape symbol that is not specified is by default `#` (i.e. the tape extends to infinity with `#` symbols in both directions).

The starting state is considered the `start` state, if the state is not defined the program will not run.

The ending state is considered `accept`, the program will end after transitioning to the `accept` state.

# TODO
- [ ] add input method for tape instead of hardcoding
- [ ] add function arguments to states
- [ ] add the posibility of using flags
- [ ] unit tests
- [ ] add 2d tape
- [ ] delete trailing '#' from tape ends

# Ideas
- add `Y`/`N`/`H` accepting states instead of just `accept`
- maximum number of steps (to avoid infinite loops)
