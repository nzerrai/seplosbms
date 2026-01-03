# Commandes Utiles - Parser ANTLR4 + AST

## 🏗️ Build & Compilation

### Compilation Complète
```bash
# Nettoyer, compiler et packager
mvn clean package

# Compilation sans tests
mvn clean package -DskipTests

# Compilation rapide (sans clean)
mvn compile
```

### Compilation de la Grammaire ANTLR4
```bash
# Générer les parsers depuis Cobol.g4
mvn antlr4:antlr4

# Vérifier les parsers générés
ls target/generated-sources/antlr4/com/cobol/translator/grammar/
```

---

## 🧪 Tests

### Exécution des Tests

```bash
# Tous les tests
mvn test

# Tests AST uniquement
mvn test -Dtest=CobolASTParserTest

# Test spécifique
mvn test -Dtest=CobolASTParserTest#testParseSimpleProgram
mvn test -Dtest=CobolASTParserTest#testParseRealFile

# Tests avec sortie détaillée
mvn test -Dtest=CobolASTParserTest -X
```

### Rapports de Tests

```bash
# Consulter les rapports
cat target/surefire-reports/com.cobol.translator.parser.CobolASTParserTest.txt

# Voir tous les rapports
ls -la target/surefire-reports/
```

---

## 🚀 Exécution

### Utiliser le JAR

```bash
# Après build (mvn package)
java -jar target/cobol-translator.jar translate examples/simple-customer.cob

# Avec options
java -jar target/cobol-translator.jar translate examples/simple-customer.cob \
  --package com.mycompany.batch \
  --output-dir ./generated
```

### Parser un Fichier COBOL (Ligne de Commande)

```bash
# Test rapide du parser
cat > /tmp/test.cob << 'EOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'HELLO'.
           STOP RUN.
EOF

# Utiliser le CLI existant
java -jar target/cobol-translator.jar translate /tmp/test.cob
```

---

## 🔍 Vérification & Debug

### Vérifier la Grammaire ANTLR4

```bash
# Afficher les warnings de la grammaire
mvn clean compile 2>&1 | grep -A 2 "warning"

# Vérifier la syntaxe de la grammaire
head -50 src/main/antlr4/com/cobol/translator/grammar/Cobol.g4
```

### Inspecter les Parsers Générés

```bash
# Lister les fichiers générés
ls -lh target/generated-sources/antlr4/com/cobol/translator/grammar/

# Afficher le lexer
head -100 target/generated-sources/antlr4/com/cobol/translator/grammar/CobolLexer.java

# Compter les tokens
grep "public static final int" target/generated-sources/antlr4/com/cobol/translator/grammar/CobolLexer.java | wc -l
```

### Debug AST

```bash
# Compiler avec debug
mvn clean compile -X

# Activer logging SLF4J
export SLF4J_SIMPLE_LOG_LEVEL=DEBUG
java -jar target/cobol-translator.jar translate test.cob
```

---

## 📊 Statistiques

### Compter les Classes AST

```bash
# Nombre de classes AST
ls src/main/java/com/cobol/translator/ast/*.java | wc -l

# Lister toutes les classes
ls src/main/java/com/cobol/translator/ast/

# Compter lignes de code AST
find src/main/java/com/cobol/translator/ast -name "*.java" -exec cat {} \; | wc -l
```

### Statistiques Grammaire

```bash
# Lignes de grammaire
wc -l src/main/antlr4/com/cobol/translator/grammar/Cobol.g4

# Nombre de règles parser
grep "^[a-z].*:" src/main/antlr4/com/cobol/translator/grammar/Cobol.g4 | wc -l

# Nombre de tokens lexer
grep "^[A-Z_].*:" src/main/antlr4/com/cobol/translator/grammar/Cobol.g4 | wc -l
```

---

## 🔧 Développement

### Modifier la Grammaire

```bash
# 1. Éditer la grammaire
nano src/main/antlr4/com/cobol/translator/grammar/Cobol.g4

# 2. Régénérer les parsers
mvn antlr4:antlr4

# 3. Recompiler
mvn compile

# 4. Tester
mvn test -Dtest=CobolASTParserTest
```

### Ajouter un Nouveau Nœud AST

```bash
# 1. Créer la classe
cat > src/main/java/com/cobol/translator/ast/MyNewNode.java << 'EOF'
package com.cobol.translator.ast;

public class MyNewNode extends ASTNode {
    @Override
    public String getNodeType() { return "MyNew"; }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitMyNewNode(this);
    }
}
EOF

# 2. Ajouter dans ASTVisitor
# (éditer ASTVisitor.java manuellement)

# 3. Implémenter dans CobolASTBuilder
# (éditer CobolASTBuilder.java manuellement)

# 4. Compiler
mvn compile
```

---

## 📦 Dépendances

### Afficher l'Arbre de Dépendances

```bash
# Toutes les dépendances
mvn dependency:tree

# Uniquement ANTLR4
mvn dependency:tree | grep antlr

# Conflits potentiels
mvn dependency:tree -Dverbose=true
```

### Mettre à Jour ANTLR4

```bash
# 1. Modifier pom.xml
nano pom.xml
# Changer <antlr.version>4.13.1</antlr.version>

# 2. Recompiler
mvn clean compile
```

---

## 📄 Documentation

### Générer Javadoc

```bash
# Générer la documentation
mvn javadoc:javadoc

# Ouvrir dans navigateur
xdg-open target/site/apidocs/index.html
# ou
open target/site/apidocs/index.html  # macOS
```

### Lister Documentation

```bash
# Documentation projet
ls -lh *.md

# Lire un document
cat PHASE1_SUMMARY.md
cat QUICK_START_AST.md
```

---

## 🧹 Nettoyage

### Nettoyer le Projet

```bash
# Supprimer target/
mvn clean

# Nettoyer complètement (y compris .class)
mvn clean -Dmaven.clean.failOnError=false

# Supprimer aussi les fichiers IDE
rm -rf .idea/ *.iml .vscode/
```

---

## 🎯 Exemples Rapides

### Parser et Analyser un Fichier

```bash
# Créer un programme de test
cat > /tmp/analyze.cob << 'EOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANALYZE-ME.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER PIC 9(5) VALUE 0.
       01  WS-NAME    PIC X(30).
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'Starting...'.
           ADD 1 TO WS-COUNTER.
           DISPLAY 'Count: ' WS-COUNTER.
           STOP RUN.
EOF

# Parser avec le CLI
java -jar target/cobol-translator.jar translate /tmp/analyze.cob
```

### Créer un Test Personnalisé

```bash
# Créer une classe de test
cat > src/test/java/com/cobol/translator/parser/MyCustomTest.java << 'EOF'
package com.cobol.translator.parser;

import com.cobol.translator.ast.*;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class MyCustomTest {
    @Test
    void testMyScenario() {
        String cobol = "       IDENTIFICATION DIVISION.\n" +
                       "       PROGRAM-ID. MYTEST.\n" +
                       "       PROCEDURE DIVISION.\n" +
                       "       PARA1.\n" +
                       "           STOP RUN.\n";

        CobolASTParser parser = new CobolASTParser();
        ProgramNode program = parser.parseString(cobol, "my-test");

        assertNotNull(program);
        assertEquals("MYTEST", program.getProgramName());
    }
}
EOF

# Compiler et tester
mvn test -Dtest=MyCustomTest
```

---

## 🐛 Dépannage

### Erreur de Compilation ANTLR4

```bash
# Vérifier la version ANTLR4
mvn dependency:tree | grep antlr4

# Nettoyer et régénérer
mvn clean
mvn antlr4:antlr4
mvn compile
```

### Tests qui Échouent

```bash
# Voir les détails d'un test
mvn test -Dtest=CobolASTParserTest#testParseSimpleProgram -X

# Exécuter avec logging debug
mvn test -Dtest=CobolASTParserTest -Dorg.slf4j.simpleLogger.defaultLogLevel=debug
```

### JAR Corrompu

```bash
# Vérifier le JAR
jar tf target/cobol-translator.jar | head -20

# Reconstruire proprement
mvn clean
mvn package -DskipTests
```

---

## 📈 Performance

### Mesurer le Temps de Compilation

```bash
# Avec time
time mvn clean package

# Seulement compilation
time mvn compile
```

### Profiler le Parser

```bash
# Activer profiling JVM
java -agentlib:hprof=cpu=samples,depth=10 \
     -jar target/cobol-translator.jar translate examples/simple-customer.cob
```

---

## 🔐 Vérification Qualité

### Vérifier le Code

```bash
# SpotBugs (si configuré)
mvn spotbugs:check

# Checkstyle (si configuré)
mvn checkstyle:check

# PMD (si configuré)
mvn pmd:check
```

### Coverage Tests

```bash
# JaCoCo (si configuré)
mvn jacoco:report

# Voir le rapport
xdg-open target/site/jacoco/index.html
```

---

## 💾 Backup & Export

### Sauvegarder la Grammaire

```bash
# Copier la grammaire
cp src/main/antlr4/com/cobol/translator/grammar/Cobol.g4 ~/backup/

# Archiver le projet AST
tar -czf ast-backup-$(date +%Y%m%d).tar.gz \
    src/main/java/com/cobol/translator/ast/ \
    src/main/java/com/cobol/translator/parser/ \
    src/main/antlr4/
```

### Exporter Documentation

```bash
# Créer archive documentation
tar -czf docs-$(date +%Y%m%d).tar.gz \
    *.md \
    examples/ \
    src/test/
```

---

## 🚀 Raccourcis Utiles

```bash
# Alias pratiques (ajouter à ~/.bashrc ou ~/.zshrc)
alias mct='mvn clean test'
alias mcp='mvn clean package -DskipTests'
alias mat='mvn test -Dtest=CobolASTParserTest'
alias mjr='java -jar target/cobol-translator.jar'

# Utilisation
mct              # Clean + test
mcp              # Build JAR rapide
mat              # Tests AST
mjr translate program.cob  # Parser un fichier
```

---

**Dernière mise à jour** : 2026-01-02
**Version** : 1.0.0-PHASE1
