#!/bin/bash

# ============================================================================
# SCRIPT DE TESTES AUTOMATIZADO PARA L2
# ============================================================================

# Cores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Função para logging
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Função para verificar se arquivo existe
check_file() {
    if [ ! -f "$1" ]; then
        log_error "Arquivo $1 não encontrado!"
        exit 1
    fi
}

# Função principal
main() {
    echo "============================================================================"
    echo "                    SISTEMA DE TESTES PARA L2"
    echo "============================================================================"
    echo

    # Verificar se os arquivos necessários existem
    log_info "Verificando arquivos necessários..."
    check_file "Main.ml"
    check_file "tests.ml"
    
    # Compilar os arquivos
    log_info "Compilando Main.ml..."
    if ! ocamlc -o main Main.ml 2>/dev/null; then
        log_error "Falha na compilação de Main.ml"
        log_info "Tentando mostrar erros de compilação:"
        ocamlc -o main Main.ml
        exit 1
    fi
    log_success "Main.ml compilado com sucesso"

    log_info "Compilando tests.ml..."
    if ! ocamlc -I . -o tests Main.ml tests.ml 2>/dev/null; then
        log_error "Falha na compilação de tests.ml"
        log_info "Tentando mostrar erros de compilação:"
        ocamlc -I . -o tests Main.ml tests.ml
        exit 1
    fi
    log_success "tests.ml compilado com sucesso"

    # Executar testes
    echo
    log_info "Executando testes..."
    echo "============================================================================"
    
    if ./tests; then
        log_success "Todos os testes passaram! ✨"
        RESULT=0
    else
        log_error "Alguns testes falharam! 😞"
        RESULT=1
    fi

    # Cleanup
    log_info "Limpando arquivos temporários..."
    rm -f main tests *.cmi *.cmo
    
    echo "============================================================================"
    exit $RESULT
}

# Função para executar apenas um teste específico
run_single_test() {
    if [ -z "$1" ]; then
        log_error "Nome do teste não fornecido"
        echo "Uso: $0 single <nome_do_teste>"
        exit 1
    fi
    
    log_info "Executando teste específico: $1"
    # Aqui você poderia modificar tests.ml para aceitar argumentos
    # Por simplicidade, executamos todos e filtramos a saída
    main | grep -A 5 -B 5 "$1"
}

# Função para mostrar ajuda
show_help() {
    echo "Sistema de Testes para L2"
    echo
    echo "Uso:"
    echo "  $0                  - Executa todos os testes"
    echo "  $0 all             - Executa todos os testes"
    echo "  $0 single <nome>   - Executa teste específico"
    echo "  $0 help            - Mostra esta ajuda"
    echo "  $0 clean           - Limpa arquivos compilados"
    echo
    echo "Exemplos:"
    echo "  $0"
    echo "  $0 single \"Soma simples\""
    echo "  $0 clean"
}

# Função para limpeza
clean() {
    log_info "Limpando arquivos compilados..."
    rm -f main tests *.cmi *.cmo
    log_success "Limpeza concluída"
}

# Verificar argumentos de linha de comando
case "${1:-all}" in
    "all"|"")
        main
        ;;
    "single")
        run_single_test "$2"
        ;;
    "help"|"-h"|"--help")
        show_help
        ;;
    "clean")
        clean
        ;;
    *)
        log_error "Comando desconhecido: $1"
        show_help
        exit 1
        ;;
esac