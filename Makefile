# ============================================================================
# MAKEFILE PARA L2 - Linguagem com construções imperativas
# ============================================================================

# Compilador OCaml
OCAMLC = ocamlc
OCAMLOPT = ocamlopt

# Arquivos fonte
MAIN_SRC = Main.ml
TEST_SRC = tests.ml

# Diretórios
BUILD_DIR = build
BIN_DIR = $(BUILD_DIR)/bin
OBJ_DIR = $(BUILD_DIR)/obj

# Executáveis
MAIN_EXE = $(BIN_DIR)/main
TEST_EXE = $(BIN_DIR)/tests

# Arquivos intermediários
OBJS = $(OBJ_DIR)/*.cmi $(OBJ_DIR)/*.cmo $(OBJ_DIR)/*.cmx $(OBJ_DIR)/*.o

# Cores para output
BLUE = \033[34m
GREEN = \033[32m
RED = \033[31m
YELLOW = \033[33m
NC = \033[0m

# ============================================================================
# SETUP
# ============================================================================

.PHONY: setup-dirs
setup-dirs:
	@echo "$(BLUE)Criando diretórios de build...$(NC)"
	@mkdir -p $(BUILD_DIR) $(BIN_DIR) $(OBJ_DIR)
	@echo "$(GREEN)Diretórios criados: $(BUILD_DIR), $(BIN_DIR), $(OBJ_DIR)$(NC)"

# ============================================================================
# TARGETS PRINCIPAIS
# ============================================================================

.PHONY: all test clean help build run-tests quick-test

# Target padrão
all: setup-dirs build test

# Compilar apenas o main
build: setup-dirs $(MAIN_EXE)

# Executar todos os testes
test: setup-dirs $(TEST_EXE)
	@echo "$(BLUE)Executando todos os testes...$(NC)"
	@$(TEST_EXE)
	@echo "$(GREEN)Testes concluídos!$(NC)"

# Teste rápido (compila e executa em uma linha)
quick-test: clean test

# Executar testes com output detalhado
verbose-test: setup-dirs $(TEST_EXE)
	@echo "$(BLUE)Executando testes com output detalhado...$(NC)"
	@$(TEST_EXE) || echo "$(RED)Alguns testes falharam$(NC)"

# ============================================================================
# COMPILAÇÃO
# ============================================================================

$(MAIN_EXE): $(MAIN_SRC)
	@echo "$(BLUE)Compilando $(MAIN_SRC)...$(NC)"
	$(OCAMLC) -I $(OBJ_DIR) -o $@ $<
	@mv *.cmi *.cmo $(OBJ_DIR)/ 2>/dev/null || true
	@echo "$(GREEN)$(MAIN_SRC) compilado com sucesso!$(NC)"

$(TEST_EXE): $(MAIN_SRC) $(TEST_SRC)
	@echo "$(BLUE)Compilando sistema de testes...$(NC)"
	$(OCAMLC) -I $(OBJ_DIR) -o $@ $^
	@mv *.cmi *.cmo $(OBJ_DIR)/ 2>/dev/null || true
	@echo "$(GREEN)Sistema de testes compilado com sucesso!$(NC)"

# ============================================================================
# INSTALAÇÃO DE DEPENDÊNCIAS
# ============================================================================

install-deps:
	@echo "$(BLUE)Verificando e instalando dependências...$(NC)"
	@if command -v apt >/dev/null 2>&1; then \
		echo "$(YELLOW)Detectado sistema Debian/Ubuntu$(NC)"; \
		echo "Execute: sudo apt update && sudo apt install ocaml"; \
	elif command -v dnf >/dev/null 2>&1; then \
		echo "$(YELLOW)Detectado sistema Fedora$(NC)"; \
		echo "Execute: sudo dnf install ocaml"; \
	elif command -v brew >/dev/null 2>&1; then \
		echo "$(YELLOW)Detectado macOS com Homebrew$(NC)"; \
		echo "Execute: brew install ocaml"; \
	else \
		echo "$(YELLOW)Sistema não detectado automaticamente$(NC)"; \
		echo "Instale OCaml manualmente ou use OPAM:"; \
		echo "  1. Instale OPAM do site oficial"; \
		echo "  2. opam init"; \
		echo "  3. eval \$$(opam env)"; \
		echo "  4. opam install ocaml"; \
	fi

# ============================================================================
# TESTES ESPECÍFICOS
# ============================================================================

# Testar apenas operações básicas
test-basic: setup-dirs $(TEST_EXE)
	@echo "$(YELLOW)Executando testes básicos...$(NC)"
	@$(TEST_EXE) | grep -E "(Soma|Multiplicação|Comparação)" -A 1 || true

# Testar apenas referências
test-refs: setup-dirs $(TEST_EXE)
	@echo "$(YELLOW)Executando testes de referências...$(NC)"
	@$(TEST_EXE) | grep -E "(Referência|Atribuição)" -A 1 || true

# Testar apenas I/O
test-io: setup-dirs $(TEST_EXE)
	@echo "$(YELLOW)Executando testes de I/O...$(NC)"
	@$(TEST_EXE) | grep -E "(Read|Print)" -A 1 || true

# Testar apenas controle de fluxo
test-control: setup-dirs $(TEST_EXE)
	@echo "$(YELLOW)Executando testes de controle de fluxo...$(NC)"
	@$(TEST_EXE) | grep -E "(If|While|Fatorial)" -A 1 || true

# ============================================================================
# DEBUGGING E DESENVOLVIMENTO
# ============================================================================

# Compilar com informações de debug
debug: setup-dirs
	@echo "$(BLUE)Compilando com informações de debug...$(NC)"
	$(OCAMLC) -g -I $(OBJ_DIR) -o $(BIN_DIR)/main $(MAIN_SRC)
	@mv *.cmi *.cmo $(OBJ_DIR)/ 2>/dev/null || true
	@echo "$(GREEN)Debug build compilado com sucesso!$(NC)"

# Verificar sintaxe sem compilar
check-syntax: $(MAIN_SRC) $(TEST_SRC)
	@echo "$(BLUE)Verificando sintaxe...$(NC)"
	$(OCAMLC) -i $(MAIN_SRC) > /dev/null && echo "$(GREEN)Main.ml: OK$(NC)" || echo "$(RED)Main.ml: ERRO$(NC)"
	$(OCAMLC) -I . -i $(TEST_SRC) > /dev/null && echo "$(GREEN)tests.ml: OK$(NC)" || echo "$(RED)tests.ml: ERRO$(NC)"

# Mostrar interface do módulo
interface: $(MAIN_SRC)
	@echo "$(BLUE)Interface do módulo Main:$(NC)"
	$(OCAMLC) -i $<

# ============================================================================
# LIMPEZA
# ============================================================================

# Limpeza básica
clean:
	@echo "$(YELLOW)Limpando arquivos compilados...$(NC)"
	rm -rf $(BUILD_DIR)
	rm -f *.cmi *.cmo *.cmx *.o  # Remove qualquer arquivo solto
	@echo "$(GREEN)Limpeza concluída!$(NC)"

# Limpeza completa (inclui backups e temporários)
clean-all: clean
	@echo "$(YELLOW)Limpeza completa...$(NC)"
	rm -f *~ *.bak *.tmp
	@echo "$(GREEN)Limpeza completa concluída!$(NC)"

# Mostrar estrutura de diretórios
show-structure:
	@echo "$(BLUE)Estrutura do projeto:$(NC)"
	@tree . 2>/dev/null || find . -type d | sed 's|[^/]*/|  |g'

# ============================================================================
# UTILITÁRIOS
# ============================================================================

# Mostrar estatísticas do código
stats:
	@echo "$(BLUE)Estatísticas do código:$(NC)"
	@echo "Linhas em Main.ml: $$(wc -l < $(MAIN_SRC))"
	@echo "Linhas em tests.ml: $$(wc -l < $(TEST_SRC))"
	@echo "Total de linhas: $$(cat $(MAIN_SRC) $(TEST_SRC) | wc -l)"
	@if [ -d "$(BUILD_DIR)" ]; then \
		echo "Tamanho da pasta build: $$(du -sh $(BUILD_DIR) | cut -f1)"; \
	fi

# Verificar se há TODOs ou FIXMEs
todo:
	@echo "$(YELLOW)Verificando TODOs e FIXMEs:$(NC)"
	@grep -n -E "(TODO|FIXME|XXX)" $(MAIN_SRC) $(TEST_SRC) || echo "$(GREEN)Nenhum TODO encontrado!$(NC)"

# Mostrar ajuda
help:
	@echo "$(BLUE)Sistema de Build para L2$(NC)"
	@echo ""
	@echo "Targets disponíveis:"
	@echo "  $(GREEN)build$(NC)          - Compila apenas o main"
	@echo "  $(GREEN)test$(NC)           - Executa todos os testes"
	@echo "  $(GREEN)quick-test$(NC)     - Limpa, compila e testa"
	@echo "  $(GREEN)verbose-test$(NC)   - Testes com output detalhado"
	@echo ""
	@echo "Testes específicos:"
	@echo "  $(GREEN)test-basic$(NC)     - Testa operações básicas"
	@echo "  $(GREEN)test-refs$(NC)      - Testa referências"
	@echo "  $(GREEN)test-io$(NC)        - Testa entrada/saída"
	@echo "  $(GREEN)test-control$(NC)   - Testa controle de fluxo"
	@echo ""
	@echo "Debugging:"
	@echo "  $(GREEN)debug$(NC)          - Compila com informações de debug"
	@echo "  $(GREEN)check-syntax$(NC)   - Verifica sintaxe sem compilar"
	@echo "  $(GREEN)interface$(NC)      - Mostra interface do módulo"
	@echo ""
	@echo "Utilitários:"
	@echo "  $(GREEN)clean$(NC)          - Remove arquivos compilados"
	@echo "  $(GREEN)clean-all$(NC)      - Limpeza completa"
	@echo "  $(GREEN)stats$(NC)          - Mostra estatísticas do código"
	@echo "  $(GREEN)todo$(NC)           - Procura TODOs no código"
	@echo "  $(GREEN)show-structure$(NC) - Mostra estrutura de diretórios"
	@echo "  $(GREEN)help$(NC)           - Mostra esta ajuda"
	@echo "  $(GREEN)install-deps$(NC)   - Mostra como instalar OCaml"
	@echo ""
	@echo "$(YELLOW)Estrutura do projeto:$(NC)"
	@echo "  $(BUILD_DIR)/bin/     - Executáveis"
	@echo "  $(BUILD_DIR)/obj/     - Arquivos objeto (.cmi, .cmo)"
	@echo ""
	@echo "Exemplos:"
	@echo "  make test"
	@echo "  make debug"
	@echo "  make show-structure"

# ============================================================================
# INTEGRAÇÃO CONTÍNUA
# ============================================================================

# Target para CI/CD
ci: setup-dirs clean check-syntax test
	@echo "$(GREEN)Pipeline de CI passou com sucesso!$(NC)"

# Verificação completa antes de commit
pre-commit: setup-dirs clean-all check-syntax todo test stats
	@echo "$(GREEN)Verificação pré-commit concluída!$(NC)"