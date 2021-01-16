# Intcode reference

Taken from intcode problems and organized for easy reference

# Instruction structure
```
ABCDE
 1002

DE - two-digit opcode,      02 == opcode 2
 C - mode of 1st parameter,  0 == position mode
 B - mode of 2nd parameter,  1 == immediate mode
 A - mode of 3rd parameter,  0 == position mode,
                                  omitted due to being a leading zero
```

# Opcodes

- Opcode 1 (add): param3 <- param1 + param2
- Opcode 2 (multiply): param3 <- param1 * param2
- Opcode 3 (input): param1 <- input
- Opcode 4 (output): outputs param1
- Opcode 5 (jump-if-true): if param1 != 0 then goto param2
- Opcode 6 (jump-if-false): if param1 == 0 then goto param2
- Opcode 7 (less than): if param1 < param2 then param3 = 1 else param3 = 0
- Opcode 8 (equals): if param1 == param2 then param3 = 1 else param3 = 0
- Opcode 9 (adjust relative base): relative base += param1


# Parameter modes:
- Mode 0 (position mode): the parameter is interpreted as a position - if the
  parameter is 50, its value is the value stored at address 50 in memory.
- Mode 1 (immediate mode): a parameter is interpreted as a value - if the
  parameter is 50, its value is simply 50.
- Mode 2 (relative mode): behave very similarly to parameters in position mode:
  the parameter is interpreted as a position. Like position mode, parameters in
  relative mode can be read from or written to. The important difference is that
  relative mode parameters don't count from address 0. Instead, they count from
  a value called the relative base. The relative base starts at 0.

