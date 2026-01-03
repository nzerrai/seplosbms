#!/bin/bash

# Démonstration rapide du traducteur COBOL to Java
# Ce script montre un exemple complet de A à Z

set -e

GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo ""
echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║                                                               ║"
echo "║      COBOL to Java Translator - Démonstration Rapide         ║"
echo "║                                                               ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""

# Fonction pour afficher les étapes
step() {
    echo -e "${BLUE}▶${NC} $1"
    echo ""
}

# Fonction pour afficher le succès
success() {
    echo -e "${GREEN}✓${NC} $1"
    echo ""
}

# Fonction pour attendre l'utilisateur
wait_user() {
    echo -e "${YELLOW}Appuyez sur Entrée pour continuer...${NC}"
    read
}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$SCRIPT_DIR/.."

# ============================================================================
# ÉTAPE 1: Afficher le programme COBOL source
# ============================================================================
step "ÉTAPE 1: Examen du programme COBOL source"

echo "Voici le programme COBOL que nous allons traduire:"
echo ""
echo "────────────────────── simple-customer.cob ──────────────────────"
cat "$SCRIPT_DIR/simple-customer.cob"
echo "──────────────────────────────────────────────────────────────────"
echo ""

wait_user

# ============================================================================
# ÉTAPE 2: Afficher les données de test
# ============================================================================
step "ÉTAPE 2: Données de test"

echo "Fichier de données au format COBOL (customers.dat):"
echo ""
head -5 "$SCRIPT_DIR/customers.dat"
echo "... (5 autres enregistrements)"
echo ""

echo "Fichier de données au format CSV (customers.csv):"
echo ""
head -6 "$SCRIPT_DIR/customers.csv"
echo "... (5 autres enregistrements)"
echo ""

wait_user

# ============================================================================
# ÉTAPE 3: Lancer la traduction
# ============================================================================
step "ÉTAPE 3: Traduction COBOL → Java"

echo "Commande de traduction:"
echo "  java -jar target/cobol-translator.jar translate ./examples/simple-customer.cob"
echo ""

cd "$PROJECT_ROOT"

if [ ! -f "target/cobol-translator.jar" ]; then
    echo "Construction du JAR du traducteur..."
    mvn clean package -DskipTests -q
    echo ""
fi

java -jar target/cobol-translator.jar translate "$SCRIPT_DIR/simple-customer.cob"

success "Traduction terminée!"

wait_user

# ============================================================================
# ÉTAPE 4: Explorer le code Java généré
# ============================================================================
step "ÉTAPE 4: Code Java généré"

GENERATED_PROJECT="../generated-projects/customer-batch-processing"

if [ -d "$GENERATED_PROJECT" ]; then
    echo "Structure du projet généré:"
    echo ""
    cd "$GENERATED_PROJECT"
    tree -L 3 -I 'target' 2>/dev/null || find . -type d -not -path '*/target/*' -not -path '*/\.*' | head -20
    echo ""

    echo "Fichiers Java générés:"
    echo ""
    find src/main/java -name "*.java" -type f | while read file; do
        echo "  📄 $file"
    done
    echo ""

    wait_user

    echo "Exemple de code généré - CustomerFileRecord.java:"
    echo ""
    echo "────────────────────────────────────────────────────────────────"
    head -50 "$(find src/main/java -name 'CustomerFileRecord.java')"
    echo "..."
    echo "────────────────────────────────────────────────────────────────"
    echo ""

    wait_user
fi

# ============================================================================
# ÉTAPE 5: Rapport de conversion
# ============================================================================
step "ÉTAPE 5: Rapport de conversion"

if [ -f "$GENERATED_PROJECT/docs/CUSTPROC_CONVERSION_REPORT.txt" ]; then
    echo "Extrait du rapport de conversion:"
    echo ""
    echo "════════════════════════════════════════════════════════════════"
    head -60 "$GENERATED_PROJECT/docs/CUSTPROC_CONVERSION_REPORT.txt"
    echo "..."
    echo "════════════════════════════════════════════════════════════════"
    echo ""
    echo "Rapport complet disponible dans:"
    echo "  $GENERATED_PROJECT/docs/CUSTPROC_CONVERSION_REPORT.txt"
    echo ""
fi

wait_user

# ============================================================================
# ÉTAPE 6: Comparaison des approches
# ============================================================================
step "ÉTAPE 6: Comparaison COBOL vs Java"

cat << 'EOF'
┌─────────────────────────────────────────────────────────────────────┐
│                    COBOL vs Java Spring Batch                       │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  COBOL (Procédural)              │  Java (Orienté Objet)           │
│  ──────────────────────────────  │  ────────────────────────────   │
│                                  │                                 │
│  • Programme monolithique        │  • Architecture modulaire       │
│  • Fichiers séquentiels          │  • Spring Batch framework       │
│  • PERFORM loops                 │  • ItemReader/Processor/Writer  │
│  • PIC clauses                   │  • Types Java (BigDecimal)      │
│  • COMP-3 (Packed Decimal)       │  • CSV/JSON/Database            │
│  • Mainframe JCL                 │  • Configuration Spring         │
│                                  │                                 │
├─────────────────────────────────────────────────────────────────────┤
│                         Avantages de la Traduction                  │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ✓ Modernisation de l'infrastructure                               │
│  ✓ Réduction des coûts de licences mainframe                       │
│  ✓ Accès à l'écosystème Java/Spring                                │
│  ✓ Intégration avec services cloud                                 │
│  ✓ Équipes de développement plus larges                            │
│  ✓ Outils de développement modernes                                │
│  ✓ Scalabilité horizontale                                         │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
EOF

echo ""
wait_user

# ============================================================================
# ÉTAPE 7: Prochaines étapes
# ============================================================================
step "ÉTAPE 7: Prochaines étapes"

cat << EOF
Pour continuer avec ce projet traduit:

1️⃣  Examiner le code généré
    cd $GENERATED_PROJECT
    # Ouvrir dans votre IDE favori

2️⃣  Adapter la configuration Spring Batch
    # Modifier src/main/resources/application.properties
    # Configurer les readers/writers selon vos besoins

3️⃣  Ajouter les tests unitaires
    # Le framework de test est déjà configuré
    # Voir src/test/java/

4️⃣  Compiler et tester
    mvn clean package
    mvn spring-boot:run

5️⃣  Comparer avec le programme COBOL original
    # Utiliser le script de test automatisé:
    cd $SCRIPT_DIR
    ./run-tests.sh

6️⃣  Déployer
    # Le JAR est prêt pour le déploiement:
    java -jar target/customer-batch-processing-1.0.0-SNAPSHOT.jar

EOF

echo ""
echo "════════════════════════════════════════════════════════════════"
echo ""
echo -e "${GREEN}✅ Démonstration terminée!${NC}"
echo ""
echo "📚 Ressources disponibles:"
echo "   • examples/README.md - Guide complet"
echo "   • examples/TEST_DATA_README.md - Format des données"
echo "   • examples/generate_test_data.py - Générateur de données"
echo "   • examples/run-tests.sh - Tests automatisés"
echo ""
echo "🚀 Projet généré:"
echo "   $GENERATED_PROJECT"
echo ""
echo "📊 Rapport de conversion:"
echo "   $GENERATED_PROJECT/docs/CUSTPROC_CONVERSION_REPORT.txt"
echo ""
