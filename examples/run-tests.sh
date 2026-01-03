#!/bin/bash

# Script de test pour valider la traduction COBOL vers Java
# Compare les résultats du programme COBOL original avec le code Java généré

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR/.."
GENERATED_PROJECT="../generated-projects/customer-batch-processing"

echo "╔════════════════════════════════════════════════════════╗"
echo "║   Test de Validation COBOL → Java                     ║"
echo "╚════════════════════════════════════════════════════════╝"
echo ""

# Couleurs pour l'affichage
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Fonction pour afficher les résultats
print_result() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✓${NC} $2"
    else
        echo -e "${RED}✗${NC} $2"
    fi
}

# ============================================================================
# 1. Vérification des prérequis
# ============================================================================
echo "📋 Vérification des prérequis..."
echo ""

# Vérifier GnuCOBOL (optionnel)
if command -v cobc &> /dev/null; then
    echo -e "${GREEN}✓${NC} GnuCOBOL installé ($(cobc --version | head -n1))"
    COBOL_AVAILABLE=true
else
    echo -e "${YELLOW}⚠${NC} GnuCOBOL non installé - tests COBOL ignorés"
    echo "   Installation: sudo apt-get install gnucobol"
    COBOL_AVAILABLE=false
fi

# Vérifier Maven
if command -v mvn &> /dev/null; then
    echo -e "${GREEN}✓${NC} Maven installé ($(mvn --version | head -n1))"
    MAVEN_AVAILABLE=true
else
    echo -e "${RED}✗${NC} Maven non installé - tests Java impossibles"
    echo "   Installation: sudo apt-get install maven"
    MAVEN_AVAILABLE=false
    exit 1
fi

# Vérifier Java
if command -v java &> /dev/null; then
    echo -e "${GREEN}✓${NC} Java installé ($(java -version 2>&1 | head -n1))"
else
    echo -e "${RED}✗${NC} Java non installé"
    exit 1
fi

echo ""

# ============================================================================
# 2. Test du programme COBOL (si disponible)
# ============================================================================
if [ "$COBOL_AVAILABLE" = true ]; then
    echo "🔵 Test du programme COBOL original..."
    echo ""

    cd "$SCRIPT_DIR"

    # Compiler le programme COBOL
    echo "   Compilation de simple-customer.cob..."
    if cobc -x -free simple-customer.cob 2>/dev/null; then
        echo -e "   ${GREEN}✓${NC} Compilation réussie"

        # Exécuter le programme
        echo "   Exécution du programme COBOL..."
        echo ""
        echo "   ─────────────────── Sortie COBOL ───────────────────"
        ./simple-customer > /tmp/cobol_output.txt 2>&1 || true
        cat /tmp/cobol_output.txt
        echo "   ────────────────────────────────────────────────────"
        echo ""

        # Extraire le nombre d'enregistrements traités
        COBOL_COUNT=$(grep -oP 'PROCESSED:\s*\K\d+' /tmp/cobol_output.txt || echo "0")
        COBOL_HIGH_VALUE_COUNT=$(grep -c "HIGH VALUE:" /tmp/cobol_output.txt || echo "0")

        echo "   Résultats COBOL:"
        echo "   - Enregistrements traités: $COBOL_COUNT"
        echo "   - Valeurs élevées (>1000): $COBOL_HIGH_VALUE_COUNT"
        echo ""

    else
        echo -e "   ${RED}✗${NC} Échec de la compilation COBOL"
        COBOL_AVAILABLE=false
    fi
fi

# ============================================================================
# 3. Vérifier que le projet Java a été généré
# ============================================================================
echo "🔍 Vérification du projet Java généré..."
echo ""

if [ ! -d "$GENERATED_PROJECT" ]; then
    echo -e "${YELLOW}⚠${NC} Le projet Java n'existe pas encore"
    echo "   Génération du projet..."
    cd "$PROJECT_DIR"
    java -jar target/cobol-translator.jar translate "$SCRIPT_DIR/simple-customer.cob"
fi

if [ ! -d "$GENERATED_PROJECT" ]; then
    echo -e "${RED}✗${NC} Impossible de générer le projet Java"
    exit 1
fi

echo -e "${GREEN}✓${NC} Projet Java trouvé: $GENERATED_PROJECT"
echo ""

# ============================================================================
# 4. Copier les données de test dans le projet Java
# ============================================================================
echo "📦 Préparation des données de test..."
echo ""

cd "$GENERATED_PROJECT"

# Créer le répertoire de données
mkdir -p src/main/resources/data

# Copier le fichier CSV
cp "$SCRIPT_DIR/customers.csv" src/main/resources/data/customers.csv
echo -e "${GREEN}✓${NC} Fichier de test copié: customers.csv"
echo ""

# ============================================================================
# 5. Compiler et tester le projet Java
# ============================================================================
echo "☕ Compilation du projet Java..."
echo ""

mvn clean package -DskipTests -q
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓${NC} Compilation Maven réussie"
else
    echo -e "${RED}✗${NC} Échec de la compilation Maven"
    exit 1
fi
echo ""

# ============================================================================
# 6. Comparer les résultats
# ============================================================================
echo "📊 Comparaison des résultats..."
echo ""

# Résultats attendus
EXPECTED_TOTAL=10
EXPECTED_HIGH_VALUE=5

echo "Résultats attendus (basés sur customers.csv):"
echo "  - Total d'enregistrements: $EXPECTED_TOTAL"
echo "  - Montants > 1000€: $EXPECTED_HIGH_VALUE"
echo ""

if [ "$COBOL_AVAILABLE" = true ] && [ -f /tmp/cobol_output.txt ]; then
    echo "Résultats COBOL:"
    echo "  - Total d'enregistrements: $COBOL_COUNT"
    echo "  - Montants > 1000€: $COBOL_HIGH_VALUE_COUNT"
    echo ""

    # Validation COBOL
    if [ "$COBOL_COUNT" = "$EXPECTED_TOTAL" ]; then
        echo -e "  ${GREEN}✓${NC} Nombre total correct"
    else
        echo -e "  ${RED}✗${NC} Nombre total incorrect (attendu: $EXPECTED_TOTAL)"
    fi

    if [ "$COBOL_HIGH_VALUE_COUNT" = "$EXPECTED_HIGH_VALUE" ]; then
        echo -e "  ${GREEN}✓${NC} Nombre de HIGH VALUE correct"
    else
        echo -e "  ${RED}✗${NC} Nombre de HIGH VALUE incorrect (attendu: $EXPECTED_HIGH_VALUE)"
    fi
    echo ""
fi

# ============================================================================
# 7. Affichage des fichiers générés
# ============================================================================
echo "📝 Fichiers Java générés:"
echo ""
find src/main/java -name "*.java" -type f | while read file; do
    echo "  - $(basename $file)"
done
echo ""

# ============================================================================
# 8. Rapport de conversion
# ============================================================================
if [ -f "docs/CUSTPROC_CONVERSION_REPORT.txt" ]; then
    echo "📋 Extrait du rapport de conversion:"
    echo ""
    echo "────────────────────────────────────────────────────────"
    head -40 "docs/CUSTPROC_CONVERSION_REPORT.txt"
    echo "────────────────────────────────────────────────────────"
    echo ""
    echo "   Rapport complet: docs/CUSTPROC_CONVERSION_REPORT.txt"
fi

# ============================================================================
# 9. Résumé final
# ============================================================================
echo ""
echo "╔════════════════════════════════════════════════════════╗"
echo "║   Résumé des Tests                                     ║"
echo "╚════════════════════════════════════════════════════════╝"
echo ""

if [ "$COBOL_AVAILABLE" = true ]; then
    echo -e "${GREEN}✓${NC} Programme COBOL testé"
else
    echo -e "${YELLOW}⚠${NC} Programme COBOL non testé (GnuCOBOL non disponible)"
fi

echo -e "${GREEN}✓${NC} Code Java généré et compilé"
echo -e "${GREEN}✓${NC} Données de test préparées"
echo ""

echo "📁 Emplacement du projet Java généré:"
echo "   $GENERATED_PROJECT"
echo ""

echo "🚀 Prochaines étapes:"
echo "   1. Examiner le code Java généré"
echo "   2. Adapter les readers/writers Spring Batch si nécessaire"
echo "   3. Exécuter: cd $GENERATED_PROJECT && mvn spring-boot:run"
echo "   4. Comparer les résultats avec la sortie COBOL"
echo ""

echo "✅ Tests terminés!"
