#!/bin/bash

##############################################################################
# Script de Test de la Conversion Web COBOL vers Java Spring Batch
#
# Ce script teste que l'interface Web génère bien un ZIP complet avec
# tous les fichiers (et pas juste l'arborescence vide)
#
# Usage: ./test-web-conversion.sh
##############################################################################

set -e  # Exit on error

# Couleurs pour l'affichage
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║  Test de Conversion Web COBOL → Java Spring Batch           ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Configuration
PROJECT_NAME="test-web-project"
BASE_PACKAGE="com.test.batch"
OUTPUT_DIR="/home/debian/tmp/cobol-output"
ZIP_FILE="${PROJECT_NAME}.zip"

# 1. Vérifier qu'un fichier COBOL de test existe
echo -e "${YELLOW}[1/6] Vérification du fichier COBOL de test...${NC}"

if [ ! -d "exemples" ]; then
    echo -e "${RED}✗ Répertoire 'exemples' non trouvé${NC}"
    echo "Créez d'abord des exemples COBOL dans le répertoire exemples/"
    exit 1
fi

# Trouver le premier fichier .cob ou .cbl
COBOL_FILE=$(find exemples -name "*.cob" -o -name "*.cbl" | head -n 1)

if [ -z "$COBOL_FILE" ]; then
    echo -e "${RED}✗ Aucun fichier COBOL trouvé dans exemples/${NC}"
    echo "Ajoutez au moins un fichier .cob ou .cbl dans exemples/"
    exit 1
fi

echo -e "${GREEN}✓ Fichier COBOL trouvé: $COBOL_FILE${NC}"

# 2. Vérifier que l'application Spring Boot est démarrée
echo ""
echo -e "${YELLOW}[2/6] Vérification de l'application Spring Boot...${NC}"

if ! curl -s http://localhost:9090/conversion > /dev/null 2>&1; then
    echo -e "${RED}✗ L'application Spring Boot ne répond pas sur http://localhost:9090${NC}"
    echo ""
    echo "Démarrez l'application avec:"
    echo "  mvn spring-boot:run"
    echo ""
    echo "Attendez que vous voyiez:"
    echo "  Started CobolTranslatorWebApplication in X.XXX seconds"
    echo ""
    echo "Puis relancez ce script."
    exit 1
fi

echo -e "${GREEN}✓ Application Spring Boot accessible${NC}"

# 3. Nettoyer les anciens fichiers de test
echo ""
echo -e "${YELLOW}[3/6] Nettoyage des anciens fichiers de test...${NC}"

rm -f "$ZIP_FILE" 2>/dev/null || true
rm -rf "${OUTPUT_DIR}/${PROJECT_NAME}" 2>/dev/null || true

echo -e "${GREEN}✓ Nettoyage effectué${NC}"

# 4. Tester la conversion via curl
echo ""
echo -e "${YELLOW}[4/6] Envoi de la requête de conversion...${NC}"

HTTP_CODE=$(curl -s -w "%{http_code}" -o "$ZIP_FILE" \
    -F "files=@${COBOL_FILE}" \
    -F "projectName=${PROJECT_NAME}" \
    -F "basePackage=${BASE_PACKAGE}" \
    http://localhost:9090/conversion/upload)

if [ "$HTTP_CODE" != "200" ]; then
    echo -e "${RED}✗ Erreur HTTP: $HTTP_CODE${NC}"

    # Afficher le contenu de la réponse si ce n'est pas un ZIP
    if file "$ZIP_FILE" | grep -q "Zip"; then
        echo "Le serveur a retourné un ZIP malgré l'erreur"
    else
        echo "Contenu de l'erreur:"
        cat "$ZIP_FILE"
    fi

    rm -f "$ZIP_FILE"
    exit 1
fi

echo -e "${GREEN}✓ Conversion réussie (HTTP 200)${NC}"

# 5. Vérifier que le ZIP contient des fichiers
echo ""
echo -e "${YELLOW}[5/6] Vérification du contenu du ZIP...${NC}"

if [ ! -f "$ZIP_FILE" ]; then
    echo -e "${RED}✗ Fichier ZIP non créé${NC}"
    exit 1
fi

# Vérifier que c'est bien un ZIP
if ! file "$ZIP_FILE" | grep -q "Zip"; then
    echo -e "${RED}✗ Le fichier téléchargé n'est pas un ZIP valide${NC}"
    file "$ZIP_FILE"
    exit 1
fi

# Compter le nombre de fichiers dans le ZIP
FILE_COUNT=$(unzip -l "$ZIP_FILE" | grep -c "^\s*[0-9]" || true)

echo "Nombre d'entrées dans le ZIP: $FILE_COUNT"

if [ "$FILE_COUNT" -lt 5 ]; then
    echo -e "${RED}✗ Le ZIP contient trop peu de fichiers ($FILE_COUNT)${NC}"
    echo ""
    echo "Contenu du ZIP:"
    unzip -l "$ZIP_FILE"
    exit 1
fi

echo -e "${GREEN}✓ ZIP contient $FILE_COUNT entrées${NC}"

# Vérifier les fichiers essentiels
echo ""
echo "Vérification des fichiers essentiels..."

ESSENTIAL_FILES=(
    "pom.xml"
    "README.md"
    "src/main/resources/application.properties"
)

MISSING_FILES=0
for FILE in "${ESSENTIAL_FILES[@]}"; do
    if unzip -l "$ZIP_FILE" | grep -q "$FILE"; then
        echo -e "  ${GREEN}✓${NC} $FILE"
    else
        echo -e "  ${RED}✗${NC} $FILE (manquant)"
        MISSING_FILES=$((MISSING_FILES + 1))
    fi
done

if [ $MISSING_FILES -gt 0 ]; then
    echo -e "${RED}✗ $MISSING_FILES fichier(s) essentiel(s) manquant(s)${NC}"
    echo ""
    echo "Contenu complet du ZIP:"
    unzip -l "$ZIP_FILE"
    exit 1
fi

# 6. Extraire et vérifier la taille des fichiers
echo ""
echo -e "${YELLOW}[6/6] Vérification de la taille des fichiers...${NC}"

# Créer un répertoire temporaire pour l'extraction
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

unzip -q "$ZIP_FILE" -d "$TEMP_DIR"

# Compter les fichiers réels (pas les répertoires)
REAL_FILE_COUNT=$(find "$TEMP_DIR" -type f | wc -l)

echo "Nombre de fichiers extraits: $REAL_FILE_COUNT"

if [ $REAL_FILE_COUNT -lt 5 ]; then
    echo -e "${RED}✗ Trop peu de fichiers extraits ($REAL_FILE_COUNT)${NC}"
    echo "Le ZIP pourrait contenir uniquement des répertoires vides"
    exit 1
fi

# Vérifier que les fichiers ne sont pas vides
EMPTY_FILES=$(find "$TEMP_DIR" -type f -empty | wc -l)

if [ $EMPTY_FILES -gt 0 ]; then
    echo -e "${YELLOW}⚠ Attention: $EMPTY_FILES fichier(s) vide(s) trouvé(s)${NC}"
    find "$TEMP_DIR" -type f -empty
else
    echo -e "${GREEN}✓ Tous les fichiers ont du contenu${NC}"
fi

# Calculer la taille totale
TOTAL_SIZE=$(du -sh "$TEMP_DIR" | cut -f1)
echo "Taille totale du projet: $TOTAL_SIZE"

# Vérifier que pom.xml contient bien du XML
if grep -q "<project" "$TEMP_DIR/pom.xml" 2>/dev/null; then
    echo -e "${GREEN}✓ pom.xml est un fichier XML valide${NC}"
else
    echo -e "${RED}✗ pom.xml n'est pas valide${NC}"
    exit 1
fi

# 7. Résumé final
echo ""
echo -e "${BLUE}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║                    RÉSUMÉ DU TEST                            ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "${GREEN}✓ Fichier COBOL source:${NC} $COBOL_FILE"
echo -e "${GREEN}✓ Projet généré:${NC} $PROJECT_NAME"
echo -e "${GREEN}✓ Package Java:${NC} $BASE_PACKAGE"
echo -e "${GREEN}✓ ZIP créé:${NC} $ZIP_FILE"
echo -e "${GREEN}✓ Nombre de fichiers:${NC} $REAL_FILE_COUNT"
echo -e "${GREEN}✓ Taille totale:${NC} $TOTAL_SIZE"
echo ""

# Optionnel: Afficher la structure du projet
echo "Structure du projet généré:"
tree -L 3 "$TEMP_DIR" 2>/dev/null || find "$TEMP_DIR" -type d | sed 's|[^/]*/| |g'

echo ""
echo -e "${GREEN}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║            ✓ TOUS LES TESTS SONT PASSÉS !                   ║${NC}"
echo -e "${GREEN}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo "Le ZIP généré est disponible: $ZIP_FILE"
echo "Vous pouvez l'extraire et tester le projet:"
echo ""
echo "  unzip $ZIP_FILE -d extracted-project"
echo "  cd extracted-project"
echo "  mvn clean package"
echo "  mvn spring-boot:run"
echo ""

exit 0
