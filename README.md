# Turing Machine Programming Language (TMPL)

# Usage

To build:
```
make build
```

To run `filename.tm`:
```
./turing filename.tm tape.t
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

Alphabet: You can use any ASCII character for your alphabet with the exceptions of:
- `*` (wildcard) - if used as a pattern it will match any character
- `<`, `>` - are used to mark the head in tape files (you can use them, but you will not be able to have them on the starting tape)

**NOTE**: `#` is the empty symbol, any tape symbol that is not specified is by default `#` (i.e. the tape extends to infinity with `#` symbols in both directions).

The starting state is considered the `start` state, if the state is not defined the program will not run.

If you want another starting state you can use just add a redundant start state as follows:
```
start * H [starting_state]
```

The ending state are considered `Y`, `N` and `H`:
- `Y` will accept the input
- `N` will reject the input
- `H` will halt and output the tape contents

# Tape
You can start a tape file with as many `//` comment lines as you want.

The first line without a comment will be considered the tape, with the head being placed between `<` and `>` characters.

Every character is a separate cell of the tape.

**NOTE**: If you want an empty tape, you will still need to provide a empty tape file with the string `<#>`.

# TODO
- [x] add input method for tape instead of hardcoding
- [x] change `accept` state to `Y`/`N`/`H` states for accept/reject/halt
- [ ] add function arguments to states
- [ ] add the posibility of using flags
- [ ] unit tests for interpreter/preprocessor
- [ ] add 2d tape
- [ ] delete trailing '#' from tape ends
- [ ] add more specific error system
