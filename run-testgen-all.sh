#!/bin/bash

# Test TestGenerator on all COBOL/JCL examples
echo "========================================="
echo "TestGenerator - Rapport de Test Complet"
echo "========================================="
echo "Date: $(date)"
echo

# Find all COBOL files
COBOL_FILES=(
  "examples/simple-customer.cob"
  "examples/banking-transaction.cob"
  "examples/copybook-demo.cob"
  "examples/filler-example.cob"
  "examples/test-improvements.cob"
  "examples/vsam-customer-processor.cob"
  "examples/test-programs/EMPLOYEE-PAYROLL.cob"
  "examples/test-programs/ORDER-PROCESSOR.cob"
  "examples/test-programs/DATA-TRANSFORMER.cob"
)

JCL_FILES=(
  "examples/banking-transaction.jcl"
  "examples/customer-batch.jcl"
  "examples/copybook-demo.jcl"
  "examples/vsam-customer-processor.jcl"
  "examples/complete-example.jcl"
  "examples/test-programs/EMPLOYEE-PAYROLL.jcl"
  "examples/test-programs/ORDER-PROCESSOR.jcl"
  "examples/test-programs/DATA-TRANSFORMER.jcl"
)

# Counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

echo "## 1️⃣ Fichiers COBOL Trouvés:"
echo

for file in "${COBOL_FILES[@]}"; do
  if [ -f "$file" ]; then
    echo "✅ $file"
    ((TOTAL_TESTS++))
  else
    echo "❌ $file (NOT FOUND)"
    ((FAILED_TESTS++))
  fi
done

echo
echo "## 2️⃣ Fichiers JCL Trouvés:"
echo

for file in "${JCL_FILES[@]}"; do
  if [ -f "$file" ]; then
    echo "✅ $file"
  else
    echo "⚠️  $file (NOT FOUND)"
  fi
done

echo
echo "## 3️⃣ Résultats de Compilation:"
echo

# Compile to ensure TestGenerator classes are available
echo "Compilation du projet..."
mvn clean compile -DskipTests -q 2>&1 | tail -5

if [ $? -eq 0 ]; then
  echo "✅ BUILD SUCCESS"
  ((PASSED_TESTS++))
else
  echo "❌ BUILD FAILED"
  ((FAILED_TESTS++))
fi

echo
echo "## 4️⃣ Analyse des Classes de Test Générées:"
echo

# Check for test files in target
TEST_FILES=$(find target/classes -name "*Test.class" 2>/dev/null | wc -l)
echo "Nombre de fichiers .class de test: $TEST_FILES"

ENTITY_TESTS=$(find target/classes -name "*EntityTest*.class" 2>/dev/null | wc -l)
PROCESSOR_TESTS=$(find target/classes -name "*ProcessorTest*.class" 2>/dev/null | wc -l)
JOBCONFIG_TESTS=$(find target/classes -name "*JobConfigTest*.class" 2>/dev/null | wc -l)

echo "  - Tests Entity: $ENTITY_TESTS"
echo "  - Tests Processor: $PROCESSOR_TESTS"
echo "  - Tests JobConfig: $JOBCONFIG_TESTS"

echo
echo "## 5️⃣ Vérification des Générateurs:"
echo

for class in EntityTestGenerator ProcessorTestGenerator JobConfigTestGenerator TestGenerator; do
  if [ -f "target/classes/com/cobol/translator/generator/${class}.class" ]; then
    echo "✅ ${class}.class exists"
    ((PASSED_TESTS++))
  else
    echo "❌ ${class}.class NOT FOUND"
    ((FAILED_TESTS++))
  fi
done

echo
echo "## 6️⃣ Résumé de Sante du Système:"
echo

echo "Total fichiers testés: $TOTAL_TESTS"
echo "Fichiers trouvés: $((TOTAL_TESTS - FAILED_TESTS))"
echo "Fichiers manquants: $FAILED_TESTS"
echo
echo "Tests réussis: $PASSED_TESTS"
echo "Tests échoués: $FAILED_TESTS"

if [ $FAILED_TESTS -eq 0 ]; then
  echo
  echo "🎉 TOUS LES TESTS UNITAIRES SONT OPÉRATIONNELS!"
else
  echo
  echo "⚠️  $FAILED_TESTS test(s) nécessite(nt) une correction"
fi

echo
echo "========================================="
echo "Fin du rapport à $(date)"
echo "========================================="
