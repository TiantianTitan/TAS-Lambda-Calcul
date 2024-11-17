# Variables pour les chemins et les fichiers
OCAMLC = ocamlc
SRC = ./src
TEST = ./test
EXEC = ./exec

# Modules compilés
AST_CMO = $(EXEC)/lambda_ast.cmo
EVAL_CMO = $(EXEC)/lambda_eval.cmo
TYPE_CMO = $(EXEC)/lambda_type.cmo

# Fichiers de test
TEST_EVAL = $(TEST)/test_lambda_eval.ml
TEST_TYPE = $(TEST)/test_lambda_type.ml

# Exécutables
MAIN_EXE = $(EXEC)/main
TEST_EVAL_EXE = $(EXEC)/test_lambda_eval
TEST_TYPE_EXE = $(EXEC)/test_lambda_type

# Cible par défaut
all: $(MAIN_EXE)

# Compilation de l'exécutable principal
$(MAIN_EXE): $(AST_CMO) $(EVAL_CMO) $(TYPE_CMO) $(SRC)/main.ml
	$(OCAMLC) -I $(EXEC) str.cma $^ -o $@

# Compilation du module lambda_ast
$(AST_CMO): $(SRC)/lambda_ast.ml
	$(OCAMLC) -o $@ -c $<

# Compilation du module lambda_eval
$(EVAL_CMO): $(SRC)/lambda_eval.ml $(AST_CMO)
	$(OCAMLC) -I $(EXEC) -o $@ -c $<

# Compilation du module lambda_type
$(TYPE_CMO): $(SRC)/lambda_type.ml $(AST_CMO)
	$(OCAMLC) -I $(EXEC) -o $@ -c $<

# Compilation et lien pour le test de lambda_eval
$(TEST_EVAL_EXE): $(TEST_EVAL) $(AST_CMO) $(EVAL_CMO)
	$(OCAMLC) -I $(EXEC) $(AST_CMO) $(EVAL_CMO) $< -o $@

# Compilation et lien pour le test de lambda_type
$(TEST_TYPE_EXE): $(TEST_TYPE) $(AST_CMO) $(TYPE_CMO) $(EVAL_CMO)
	$(OCAMLC) -I $(EXEC) $(AST_CMO) $(TYPE_CMO) $(EVAL_CMO) $< -o $@

# Cible pour exécuter les tests
test: $(TEST_EVAL_EXE) $(TEST_TYPE_EXE)
	./$(TEST_EVAL_EXE)
	./$(TEST_TYPE_EXE)

# Nettoyage des fichiers générés
clean:
	rm -f $(EXEC)/*.cmo $(EXEC)/*.cmi $(EXEC)/*.o $(EXEC)/*.cmx $(EXEC)/*
	rm -f $(MAIN_EXE)

# Exécution de REPL
repl:
	./$(MAIN_EXE)
