#!/bin/bash
##############################################################################
# Script de génération de toutes les données de test
# Génère les fichiers de test pour tous les exemples COBOL/JCL
##############################################################################

set -e  # Arrêter en cas d'erreur

echo "========================================================================"
echo "  GÉNÉRATION DE TOUTES LES DONNÉES DE TEST"
echo "========================================================================"
echo ""

# Vérifier que Python 3 est installé
if ! command -v python3 &> /dev/null; then
    echo "❌ Erreur: Python 3 n'est pas installé"
    echo "   Veuillez installer Python 3 pour continuer"
    exit 1
fi

echo "✓ Python 3 détecté: $(python3 --version)"
echo ""

# Créer le répertoire test-data s'il n'existe pas
mkdir -p test-data

# Générer les données pour l'exemple customer simple
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "1. Génération des données pour customer-batch.jcl"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
python3 generate_customer_test_data.py
echo ""

# Générer les données pour l'exemple bancaire complexe
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "2. Génération des données pour banking-transaction.jcl"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
python3 generate_banking_test_data.py
echo ""

# Afficher un résumé
echo "========================================================================"
echo "  GÉNÉRATION TERMINÉE AVEC SUCCÈS!"
echo "========================================================================"
echo ""
echo "Fichiers générés dans test-data/:"
echo ""
ls -lh test-data/ | tail -n +2 | awk '{printf "  %-40s %8s\n", $9, $5}'
echo ""
echo "Utilisation:"
echo "  1. Ces fichiers sont au format mainframe (RECFM=FB)"
echo "  2. Ils peuvent être utilisés avec les JCL correspondants"
echo "  3. Les versions *-readable.txt sont pour vérification humaine"
echo ""
echo "Exemple:"
echo "  java -jar target/cobol-translator.jar \\"
echo "    --files customer.cob,customer-batch.jcl \\"
echo "    --test-data test-data/CUSTOMER.INPUT.DATA"
echo ""
