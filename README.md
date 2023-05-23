# Compiler Construction - Assignment 2

We have built a Compiler for the `Mini-C` language as specified in `assignment.pdf`.

## Run Locally

There are **5** parts to this assignment, the steps to run each part are given below:

### Part 1: Lexical Analysis

1. Build the `lex.yy.c` file in the `bin` folder:

```bash
mkdir -p bin
flex -o bin/lex.yy.c src/lexer/part_1.l
```

2. Compile the `lex.yy.c` file into executable `lexer` in the `bin` folder:

```bash
gcc bin/lex.yy.c -o bin/lexer
```

3. Run the `lexer` executable:

```bash
./bin/lexer
```

### Part 2: Syntax Analysis

1. Build the `lex.yy.c` file in the `bin` folder:

```bash
mkdir -p bin
flex -o bin/lex.yy.c src/lexer/part_2.l
```

2. Build the `y.tab.h` and `y.tab.c` files in the `bin` folder:

```bash
bison -d -o bin/y.tab.c src/parser/part_2.y
```

3. Compile the `parser` executable in the `bin` folder:

```bash
gcc bin/y.tab.c bin/lex.yy.c -o bin/parser
```

4. Run the `parser` executable:

```bash
./bin/parser
```

### Part 3: Semantic  Analysis

1. Use `make` to build the Mini-C-Compiler `mcc` binary in the `bin` folder:

```bash
make
```

2. Run the `mcc` executable from the `bin` folder:

```bash
./bin/mcc
```

> If the code is semantically correct, the syntax tree should be output by the `mcc` binary, in this format: `(tu (ed (dec (ts int)(i_decl (decl IDENTIFIER x)(ae (loe (lae (eqe (re (adde (me (ue (pofe primary_expr (pme 3))))))))))))))`

3. To print the AST, copy this entire string and provide it as input for the `scripts/pretty_print.py` python file:

```bash
python3 scripts/pretty_print.py
```

### Part 4: Three Address Code Generation

1. Set the following variable to true in the `src/main.cpp` file:

```cpp
constexpr bool genThreeAddressCode = true;
```

2. Use `make` to build the Mini-C-Compiler `mcc` binary in the `bin` folder:

```bash
make
```

3. Run the `mcc` executable from the `bin` folder:

```bash
./bin/mcc
```

### Part 5: Execution of the program

1. Set the following variable to true in the `src/main.cpp` file:

```cpp
constexpr bool executeCode = true;
```

2. Use `make` to build the Mini-C-Compiler `mcc` binary in the `bin` folder:

```bash
make
```

3. Run the `mcc` executable from the `bin` folder:

```bash
./bin/mcc
```

## Authors

- [Tushar Chenan (2020A7PS0253H)](https://www.github.com/rocka0)
- [Kavyanjali Agnihotri (2020A7PS0185H)](https://www.github.com/kavyagnihotri)
