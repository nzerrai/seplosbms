#!/bin/bash

echo "=========================================="
echo "Test de génération automatique de tests"
echo "=========================================="
echo ""

cd /home/seplos/projets/cobol-to-java-translator

echo "1. Compilation du traducteur..."
mvn compile -DskipTests -q

if [ $? -ne 0 ]; then
    echo "❌ Erreur de compilation"
    exit 1
fi

echo "✅ Compilation réussie"
echo ""

echo "2. Test de traduction avec génération de tests..."
mvn exec:java -Dexec.mainClass="com.cobol.translator.CobolTranslator" \
    -Dexec.args="examples/simple-customer.cob" \
    -q 2>&1 | grep -E "(Generated|Test generation|entity test|processor test|job config test|Total test files)"

echo ""
echo "3. Vérification des fichiers générés..."
if [ -d "generated-projects" ]; then
    echo "📁 Fichiers de tests générés:"
    find generated-projects -name "*Test.java" -type f 2>/dev/null | head -10
    
    echo ""
    echo "📊 Statistiques:"
    echo "   - Tests d'entités: $(find generated-projects -name "*RecordTest.java" 2>/dev/null | wc -l)"
    echo "   - Tests de processors: $(find generated-projects -name "*ProcessorTest.java" 2>/dev/null | wc -l)"
    echo "   - Tests de job config: $(find generated-projects -name "*ConfigurationTest.java" 2>/dev/null | wc -l)"
    echo "   - Total: $(find generated-projects -name "*Test.java" 2>/dev/null | wc -l)"
fi

echo ""
echo "✅ Test de génération terminé"
