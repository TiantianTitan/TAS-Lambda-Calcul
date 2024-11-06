# Compiler and flags
OCAMLC = ocamlc
OCAMLFLAGS = -g
INCLUDES = -I src

# Source files
SRC = src/expr.ml src/types.ml src/test_typing.ml
OBJ = $(SRC:.ml=.cmo)

# Executable names
TEST_EXEC = test_typing

# Default target
all: $(TEST_EXEC)

# Test executable for type inference testing
$(TEST_EXEC): $(OBJ)
	$(OCAMLC) $(OCAMLFLAGS) $(INCLUDES) unix.cma -o $(TEST_EXEC) $(OBJ)

# Compile each .ml file into .cmo and .cmi files
src/%.cmo: src/%.ml
	$(OCAMLC) $(OCAMLFLAGS) $(INCLUDES) -c $<

# Run the tests
test: $(TEST_EXEC)
	@echo "Starting test_typing..."
	./$(TEST_EXEC)


# Clean up generated files
clean:
	rm -f src/*.cmo src/*.cmi $(TEST_EXEC)
