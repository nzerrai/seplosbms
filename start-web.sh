#!/bin/bash

# Script pour démarrer l'interface web COBOL to Java Translator
# Port: 9090

echo "╔══════════════════════════════════════════════════════════════════════╗"
echo "║                                                                      ║"
echo "║         COBOL TO JAVA TRANSLATOR - Interface Web                    ║"
echo "║                                                                      ║"
echo "╚══════════════════════════════════════════════════════════════════════╝"
echo ""
echo "🚀 Démarrage du serveur web..."
echo ""

# Vérifier si Maven est installé
if ! command -v mvn &> /dev/null; then
    echo "❌ Erreur: Maven n'est pas installé"
    echo "💡 Installez Maven: sudo apt install maven"
    exit 1
fi

echo "📦 Lancement de l'application Spring Boot avec Maven..."
echo ""
echo "🌐 Interface Web: http://localhost:9090/conversion"
echo "💾 Console H2: http://localhost:9090/h2-console"
echo ""

# Démarrer le serveur web avec Maven Spring Boot
mvn spring-boot:run

echo ""
echo "🛑 Serveur arrêté"
