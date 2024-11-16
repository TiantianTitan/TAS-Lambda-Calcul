# Paths
OCAMLC = ocamlc
SRC = ./src
TEST = ./test
EXEC = ./exec

# Modules
AST_CMO = $(EXEC)/lambda_ast.cmo
EVAL_CMO = $(EXEC)/lambda_eval.cmo
TYPE_CMO = $(EXEC)/lambda_type.cmo

# Test Files
TEST_EVAL = $(TEST)/test_lambda_eval.ml
TEST_TYPE = $(TEST)/test_lambda_type.ml

# Executables
MAIN_EXE = $(EXEC)/main
TEST_EVAL_EXE = $(EXEC)/test_lambda_eval
TEST_TYPE_EXE = $(EXEC)/test_lambda_type

# Default target
all: $(MAIN_EXE)

# Main executable
$(MAIN_EXE): $(AST_CMO) $(EVAL_CMO) $(TYPE_CMO) $(SRC)/main.ml
	$(OCAMLC) -I $(EXEC) $^ -o $@

# Compile lambda_ast
$(AST_CMO): $(SRC)/lambda_ast.ml
	$(OCAMLC) -o $@ -c $<

# Compile lambda_eval
$(EVAL_CMO): $(SRC)/lambda_eval.ml $(AST_CMO)
	$(OCAMLC) -I $(EXEC) -o $@ -c $<

# Compile lambda_type
$(TYPE_CMO): $(SRC)/lambda_type.ml $(AST_CMO)
	$(OCAMLC) -I $(EXEC) -o $@ -c $<

# Compile and Link Test for Lambda_eval
$(TEST_EVAL_EXE): $(TEST_EVAL) $(AST_CMO) $(EVAL_CMO)
	$(OCAMLC) -I $(EXEC) $(AST_CMO) $(EVAL_CMO) $< -o $@

# Compile and Link Test for Lambda_type
$(TEST_TYPE_EXE): $(TEST_TYPE) $(AST_CMO) $(TYPE_CMO) $(EVAL_CMO)
	$(OCAMLC) -I $(EXEC) $(AST_CMO) $(TYPE_CMO) $(EVAL_CMO) $< -o $@

# Run tests
test: $(TEST_EVAL_EXE) $(TEST_TYPE_EXE)
	./$(TEST_EVAL_EXE)
	./$(TEST_TYPE_EXE)

# Clean up
clean:
	rm -f $(EXEC)/*.cmo $(EXEC)/*.cmi $(EXEC)/*.o $(EXEC)/*.cmx $(EXEC)/*
