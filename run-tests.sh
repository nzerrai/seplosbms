#!/bin/bash

# ============================================================================
# COBOL to Java Translator - Quick Start Test Execution
# ============================================================================
# Guide rapide pour exécuter la suite de tests complète

set -e

# Couleurs
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# ============================================================================
# MAIN MENU
# ============================================================================
show_menu() {
    clear
    echo -e "${BLUE}"
    echo "╔════════════════════════════════════════════════════════════════╗"
    echo "║  COBOL to Java Translator - Test Suite Execution Menu          ║"
    echo "╚════════════════════════════════════════════════════════════════╝"
    echo -e "${NC}"
    echo ""
    echo "Select test suite to run:"
    echo ""
    echo "  1) Run Semantic Tests (Symbol Table + Type Checking)"
    echo "  2) Run Performance Benchmarks (JMH)"
    echo "  3) Run Spring Batch Integration Tests"
    echo "  4) Run End-to-End Data Verification Tests"
    echo "  5) Run Code Generation Tests"
    echo ""
    echo "  10) Run ALL Tests"
    echo "  11) Run ALL Tests + Generate Coverage Report"
    echo ""
    echo "  20) Generate Test Documentation"
    echo "  21) Run Validation Script"
    echo ""
    echo "  0) Exit"
    echo ""
    echo -n "Choose option (0-21): "
}

# ============================================================================
# TEST EXECUTION FUNCTIONS
# ============================================================================

run_semantic_tests() {
    echo -e "\n${YELLOW}Running Semantic Tests...${NC}\n"
    echo "Tests included:"
    echo "  • SymbolTableTest (18 test methods)"
    echo "  • TypeCheckerTest (17 test methods)"
    echo ""
    
    cd "$PROJECT_ROOT"
    mvn test \
        -Dtest=SymbolTableTest,TypeCheckerTest \
        -DfailIfNoTests=false
}

run_performance_benchmarks() {
    echo -e "\n${YELLOW}Running Performance Benchmarks (JMH)...${NC}\n"
    echo "Benchmarks included:"
    echo "  • Parsing (small/medium/large)"
    echo "  • Symbol Table Building"
    echo "  • Code Generation"
    echo "  • Full Conversion Workflow"
    echo "  • Memory Profiling"
    echo ""
    echo "This may take several minutes (default: 17 benchmarks)"
    echo ""
    
    cd "$PROJECT_ROOT"
    mvn test \
        -Dtest=PerformanceBenchmark \
        -DfailIfNoTests=false
}

run_spring_batch_tests() {
    echo -e "\n${YELLOW}Running Spring Batch Integration Tests...${NC}\n"
    echo "Tests included:"
    echo "  • Simple Job Execution"
    echo "  • File Input Processing"
    echo "  • Data Validation"
    echo "  • Multi-Step Workflows"
    echo "  • End-to-End Conversion"
    echo "  • Multiple File Processing"
    echo ""
    
    cd "$PROJECT_ROOT"
    mvn test \
        -Dtest=SpringBatchIntegrationTest \
        -DfailIfNoTests=false
}

run_data_verification_tests() {
    echo -e "\n${YELLOW}Running End-to-End Data Verification Tests...${NC}\n"
    echo "Tests included:"
    echo "  • Type Conversions (Numeric, String, Binary)"
    echo "  • Advanced Types (COMP-3, REDEFINES)"
    echo "  • Complex Structures (Records, Tables)"
    echo "  • Operation Validation (MOVE, Round-trip)"
    echo "  • File Control Mapping"
    echo ""
    
    cd "$PROJECT_ROOT"
    mvn test \
        -Dtest=EndToEndDataVerificationTest \
        -DfailIfNoTests=false
}

run_code_generation_tests() {
    echo -e "\n${YELLOW}Running Code Generation Integration Tests...${NC}\n"
    echo "Tests included:"
    echo "  • Simple Processor Generation"
    echo "  • Control Flow Statements"
    echo "  • Iteration Constructs"
    echo "  • Computation Expressions"
    echo "  • Architecture Validation"
    echo ""
    
    cd "$PROJECT_ROOT"
    mvn test \
        -Dtest=ProcessorGenerationIntegrationTest \
        -DfailIfNoTests=false
}

run_all_tests() {
    echo -e "\n${YELLOW}Running ALL Tests...${NC}\n"
    echo "Total tests to execute: ~82 test methods"
    echo "Estimated time: 5-10 minutes (without benchmarks)"
    echo ""
    
    cd "$PROJECT_ROOT"
    mvn clean test \
        -Dtest=SymbolTableTest,TypeCheckerTest,PerformanceBenchmark,SpringBatchIntegrationTest,EndToEndDataVerificationTest,ProcessorGenerationIntegrationTest \
        -DfailIfNoTests=false
}

run_all_tests_with_coverage() {
    echo -e "\n${YELLOW}Running ALL Tests + Code Coverage...${NC}\n"
    echo "Total tests to execute: ~82 test methods"
    echo "Code coverage report will be generated"
    echo "Estimated time: 10-15 minutes"
    echo ""
    
    cd "$PROJECT_ROOT"
    mvn clean test \
        -Dtest=SymbolTableTest,TypeCheckerTest,PerformanceBenchmark,SpringBatchIntegrationTest,EndToEndDataVerificationTest,ProcessorGenerationIntegrationTest \
        -DfailIfNoTests=false
    
    echo -e "\n${GREEN}Generating Code Coverage Report...${NC}\n"
    mvn jacoco:report
    
    echo -e "\n${GREEN}✓ Coverage report generated${NC}"
    echo "  Location: target/site/jacoco/index.html"
}

generate_documentation() {
    echo -e "\n${YELLOW}Generating Test Documentation...${NC}\n"
    
    cd "$PROJECT_ROOT"
    
    echo "Creating documentation files..."
    
    # List test files
    find src/test/java/com/cobol/translator -name "*.java" | while read file; do
        echo "  ✓ $(basename $file)"
    done
    
    # List documentation
    echo ""
    echo "Documentation files:"
    [ -f "COMPLETE_TEST_SUITE.md" ] && echo "  ✓ COMPLETE_TEST_SUITE.md (488 lines)"
    [ -f "TEST_SUITE_SUMMARY.txt" ] && echo "  ✓ TEST_SUITE_SUMMARY.txt"
    [ -f "TEST_IMPLEMENTATION_COMPLETE.md" ] && echo "  ✓ TEST_IMPLEMENTATION_COMPLETE.md"
    
    echo -e "\n${GREEN}✓ Documentation ready${NC}"
}

run_validation_script() {
    echo -e "\n${YELLOW}Running Test Suite Validation...${NC}\n"
    
    cd "$PROJECT_ROOT"
    
    if [ -f "validate-test-suite.sh" ]; then
        bash validate-test-suite.sh
    else
        echo "Error: validate-test-suite.sh not found"
        exit 1
    fi
}

show_test_report() {
    echo -e "\n${BLUE}════════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}TEST EXECUTION SUMMARY${NC}"
    echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}\n"
    
    if [ -f "target/surefire-reports/index.html" ]; then
        echo "Test Results:"
        echo "  Location: target/surefire-reports/index.html"
        echo ""
    fi
    
    if [ -d "target/site/jacoco" ]; then
        echo "Code Coverage:"
        echo "  Location: target/site/jacoco/index.html"
        echo ""
    fi
    
    echo "Test Suite Information:"
    echo "  • Total test files: 6"
    echo "  • Total test methods: 82+"
    echo "  • Semantic tests: 35"
    echo "  • Performance benchmarks: 17"
    echo "  • Integration tests: 30"
    echo ""
    
    echo "Documentation:"
    echo "  • COMPLETE_TEST_SUITE.md"
    echo "  • TEST_SUITE_SUMMARY.txt"
    echo "  • TEST_IMPLEMENTATION_COMPLETE.md"
}

# ============================================================================
# MAIN LOOP
# ============================================================================

main() {
    while true; do
        show_menu
        read -r option
        
        case $option in
            1)
                run_semantic_tests
                ;;
            2)
                run_performance_benchmarks
                ;;
            3)
                run_spring_batch_tests
                ;;
            4)
                run_data_verification_tests
                ;;
            5)
                run_code_generation_tests
                ;;
            10)
                run_all_tests
                show_test_report
                ;;
            11)
                run_all_tests_with_coverage
                show_test_report
                ;;
            20)
                generate_documentation
                ;;
            21)
                run_validation_script
                ;;
            0)
                echo -e "\n${GREEN}Goodbye!${NC}\n"
                exit 0
                ;;
            *)
                echo -e "\n${YELLOW}Invalid option. Please try again.${NC}\n"
                sleep 2
                ;;
        esac
        
        if [ $? -eq 0 ]; then
            echo -e "\n${GREEN}✓ Execution completed successfully${NC}"
        else
            echo -e "\n${YELLOW}⚠ Execution completed with errors${NC}"
        fi
        
        echo ""
        echo "Press Enter to continue..."
        read
    done
}

# ============================================================================
# START EXECUTION
# ============================================================================

# Check if Maven is installed
if ! command -v mvn &> /dev/null; then
    echo -e "${YELLOW}Maven not found. Please install Maven 3.8+${NC}"
    exit 1
fi

# Check if project has pom.xml
if [ ! -f "$PROJECT_ROOT/pom.xml" ]; then
    echo -e "${YELLOW}pom.xml not found in $PROJECT_ROOT${NC}"
    exit 1
fi

# Run main menu
main
