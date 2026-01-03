# Guide de Démarrage Rapide - Parser ANTLR4 + AST

## 🚀 Utilisation du Nouveau Parser

### Installation

Le parser ANTLR4 + AST est maintenant intégré au projet. Après compilation :

```bash
mvn clean package
```

Le JAR exécutable est généré : `target/cobol-translator.jar`

---

## 📖 Exemples d'Utilisation

### 1. Parser un Programme COBOL Simple

```java
import com.cobol.translator.parser.CobolASTParser;
import com.cobol.translator.ast.*;
import java.nio.file.Paths;

public class Example1_BasicParsing {
    public static void main(String[] args) throws Exception {
        // Créer le parser
        CobolASTParser parser = new CobolASTParser();

        // Parser un fichier COBOL
        ProgramNode program = parser.parse(Paths.get("examples/simple-customer.cob"));

        // Afficher les informations de base
        System.out.println("Programme : " + program.getProgramName());
        System.out.println("Fichier source : " + program.getSourceFile());
        System.out.println("Localisation : " + program.getLocation());
    }
}
```

**Sortie attendue :**
```
Programme : CUSTPROC
Fichier source : examples/simple-customer.cob
Localisation : Line 1, Column 0
```

---

### 2. Explorer la DATA DIVISION

```java
import com.cobol.translator.ast.*;

public class Example2_DataDivision {
    public static void main(String[] args) throws Exception {
        CobolASTParser parser = new CobolASTParser();
        ProgramNode program = parser.parse(Paths.get("examples/simple-customer.cob"));

        DataDivisionNode dataDiv = program.getDataDivision();

        // Analyser la Working-Storage Section
        WorkingStorageSectionNode wss = dataDiv.getWorkingStorageSection();
        System.out.println("\n=== WORKING-STORAGE SECTION ===");

        for (DataItemNode item : wss.getDataItems()) {
            System.out.printf("  %02d %-20s PIC %-15s VALUE %s%n",
                item.getLevel(),
                item.getName(),
                item.getPicture(),
                item.getValue() != null ? item.getValue() : "N/A"
            );
        }

        // Analyser la File Section
        FileSectionNode fileSection = dataDiv.getFileSection();
        System.out.println("\n=== FILE SECTION ===");

        for (FileDescriptionNode fd : fileSection.getFileDescriptions()) {
            System.out.println("Fichier : " + fd.getFileName());

            for (DataItemNode record : fd.getRecords()) {
                System.out.printf("  %02d %-20s PIC %s%n",
                    record.getLevel(),
                    record.getName(),
                    record.getPicture()
                );
            }
        }
    }
}
```

**Sortie attendue :**
```
=== WORKING-STORAGE SECTION ===
  01 WS-EOF            PIC X               VALUE 'N'
  01 WS-COUNT          PIC 9(5)            VALUE 0

=== FILE SECTION ===
Fichier : CUSTOMER-FILE
  01 CUSTOMER-RECORD   PIC null
  05 CUST-ID           PIC 9(6)
  05 CUST-NAME         PIC X(30)
  05 CUST-AMOUNT       PIC 9(7)V99
  05 CUST-DATE         PIC 9(8)
```

---

### 3. Naviguer la PROCEDURE DIVISION

```java
import com.cobol.translator.ast.*;

public class Example3_ProcedureDivision {
    public static void main(String[] args) throws Exception {
        CobolASTParser parser = new CobolASTParser();
        ProgramNode program = parser.parse(Paths.get("examples/simple-customer.cob"));

        ProcedureDivisionNode procDiv = program.getProcedureDivision();

        System.out.println("\n=== PROCEDURE DIVISION ===");
        System.out.println("Nombre de paragraphes : " + procDiv.getParagraphs().size());

        // Lister tous les paragraphes
        for (ParagraphNode para : procDiv.getParagraphs()) {
            System.out.println("\nParagraphe : " + para.getName());
            System.out.println("Statements : " + para.getStatements().size());

            // Lister les statements
            for (StatementNode stmt : para.getStatements()) {
                System.out.printf("  - %s (ligne %d)%n",
                    stmt.getNodeType(),
                    stmt.getLineNumber()
                );
            }
        }
    }
}
```

**Sortie attendue :**
```
=== PROCEDURE DIVISION ===
Nombre de paragraphes : 2

Paragraphe : 0000-MAIN
Statements : 5
  - OpenStatement (ligne 26)
  - PerformStatement (ligne 27)
  - CloseStatement (ligne 33)
  - DisplayStatement (ligne 34)
  - StopStatement (ligne 35)

Paragraphe : 1000-PROCESS-RECORD
Statements : 2
  - AddStatement (ligne 38)
  - IfStatement (ligne 39)
```

---

### 4. Utiliser le Pattern Visitor

```java
import com.cobol.translator.ast.*;

public class Example4_Visitor {

    // Créer un visitor personnalisé
    static class StatementCounter implements ASTVisitor<Integer> {
        private int moveCount = 0;
        private int displayCount = 0;
        private int readCount = 0;

        @Override
        public Integer visitMoveStatementNode(MoveStatementNode node) {
            moveCount++;
            return 1;
        }

        @Override
        public Integer visitDisplayStatementNode(DisplayStatementNode node) {
            displayCount++;
            System.out.println("DISPLAY trouvé à la ligne " + node.getLineNumber());
            return 1;
        }

        @Override
        public Integer visitReadStatementNode(ReadStatementNode node) {
            readCount++;
            return 1;
        }

        // Implémenter les autres méthodes avec return null par défaut
        @Override
        public Integer visitProgramNode(ProgramNode node) {
            // Visiter récursivement tous les enfants
            for (ASTNode child : node.getChildren()) {
                child.accept(this);
            }
            return null;
        }

        // ... autres méthodes visitXXX avec return null

        public void printStats() {
            System.out.println("\n=== STATISTIQUES ===");
            System.out.println("MOVE statements   : " + moveCount);
            System.out.println("DISPLAY statements: " + displayCount);
            System.out.println("READ statements   : " + readCount);
        }
    }

    public static void main(String[] args) throws Exception {
        CobolASTParser parser = new CobolASTParser();
        ProgramNode program = parser.parse(Paths.get("examples/simple-customer.cob"));

        // Utiliser le visitor
        StatementCounter counter = new StatementCounter();
        program.accept(counter);
        counter.printStats();
    }
}
```

---

### 5. Validation Syntaxique

```java
import com.cobol.translator.parser.CobolASTParser;

public class Example5_Validation {
    public static void main(String[] args) {
        CobolASTParser parser = new CobolASTParser();

        // Valider un fichier
        boolean isValid = parser.isValidSyntax(Paths.get("program.cob"));
        System.out.println("Syntaxe valide ? " + isValid);

        // Valider du code en mémoire
        String cobolCode =
            "       IDENTIFICATION DIVISION.\n" +
            "       PROGRAM-ID. TEST.\n" +
            "       PROCEDURE DIVISION.\n" +
            "       MAIN-PARA.\n" +
            "           STOP RUN.\n";

        boolean isCodeValid = parser.isValidSyntax(cobolCode);
        System.out.println("Code valide ? " + isCodeValid);
    }
}
```

---

### 6. Parser depuis une Chaîne

```java
import com.cobol.translator.parser.CobolASTParser;
import com.cobol.translator.ast.ProgramNode;

public class Example6_ParseString {
    public static void main(String[] args) {
        String cobolSource =
            "       IDENTIFICATION DIVISION.\n" +
            "       PROGRAM-ID. DYNAMIC-PROG.\n" +
            "       DATA DIVISION.\n" +
            "       WORKING-STORAGE SECTION.\n" +
            "       01  WS-COUNTER PIC 9(5) VALUE 0.\n" +
            "       PROCEDURE DIVISION.\n" +
            "       MAIN-PARAGRAPH.\n" +
            "           DISPLAY 'Processing...'.\n" +
            "           ADD 1 TO WS-COUNTER.\n" +
            "           DISPLAY 'Count: ' WS-COUNTER.\n" +
            "           STOP RUN.\n";

        CobolASTParser parser = new CobolASTParser();
        ProgramNode program = parser.parseString(cobolSource, "dynamic-program");

        System.out.println("Programme parsé : " + program.getProgramName());
        System.out.println("Variables WS : " +
            program.getDataDivision()
                   .getWorkingStorageSection()
                   .getDataItems().size()
        );
    }
}
```

---

## 🔍 Inspection de l'AST

### Afficher la Structure Complète

```java
public class Example7_TreePrinter {

    public static void printTree(ASTNode node, int indent) {
        String prefix = "  ".repeat(indent);
        System.out.println(prefix + node.getNodeType() +
            " [Ligne " + node.getLineNumber() + "]");

        for (ASTNode child : node.getChildren()) {
            printTree(child, indent + 1);
        }
    }

    public static void main(String[] args) throws Exception {
        CobolASTParser parser = new CobolASTParser();
        ProgramNode program = parser.parse(Paths.get("examples/simple-customer.cob"));

        System.out.println("\n=== ARBRE AST COMPLET ===");
        printTree(program, 0);
    }
}
```

**Sortie (extrait) :**
```
=== ARBRE AST COMPLET ===
Program [Ligne 0]
  IdentificationDivision [Ligne 1]
  EnvironmentDivision [Ligne 5]
  DataDivision [Ligne 11]
    FileSection [Ligne 12]
      FileDescription [Ligne 13]
        DataItem [Ligne 14]
        DataItem [Ligne 15]
        DataItem [Ligne 16]
    WorkingStorageSection [Ligne 20]
      DataItem [Ligne 21]
      DataItem [Ligne 22]
  ProcedureDivision [Ligne 24]
    Paragraph [Ligne 25]
      OpenStatement [Ligne 26]
      PerformStatement [Ligne 27]
      CloseStatement [Ligne 33]
```

---

## 🛠️ Cas d'Usage Avancés

### Extraire Toutes les Variables Utilisées

```java
public class Example8_VariableExtractor {

    static class VariableVisitor implements ASTVisitor<Void> {
        Set<String> variables = new HashSet<>();

        @Override
        public Void visitDataItemNode(DataItemNode node) {
            variables.add(node.getName());
            return null;
        }

        // Implémenter les autres méthodes...

        public Set<String> getVariables() {
            return variables;
        }
    }

    public static void main(String[] args) throws Exception {
        CobolASTParser parser = new CobolASTParser();
        ProgramNode program = parser.parse(Paths.get("examples/simple-customer.cob"));

        VariableVisitor visitor = new VariableVisitor();
        program.accept(visitor);

        System.out.println("Variables trouvées : " + visitor.getVariables());
    }
}
```

---

### Détecter les Patterns COBOL

```java
public class Example9_PatternDetection {

    public static void detectFileProcessing(ProgramNode program) {
        boolean hasFileSection = program.getDataDivision().getFileSection() != null;
        boolean hasRead = false;
        boolean hasWrite = false;

        ProcedureDivisionNode procDiv = program.getProcedureDivision();
        for (ParagraphNode para : procDiv.getParagraphs()) {
            for (StatementNode stmt : para.getStatements()) {
                if (stmt instanceof ReadStatementNode) hasRead = true;
                if (stmt instanceof WriteStatementNode) hasWrite = true;
            }
        }

        if (hasFileSection && hasRead) {
            System.out.println("✓ Pattern détecté : FILE_PROCESSING");
        }
    }

    public static void main(String[] args) throws Exception {
        CobolASTParser parser = new CobolASTParser();
        ProgramNode program = parser.parse(Paths.get("examples/simple-customer.cob"));

        detectFileProcessing(program);
    }
}
```

---

## 📝 Bonnes Pratiques

### 1. Gestion des Erreurs

```java
try {
    ProgramNode program = parser.parse(Paths.get("program.cob"));
    // Traitement...
} catch (RuntimeException e) {
    System.err.println("Erreur de parsing : " + e.getMessage());
    // La cause racine contient les détails ANTLR
    Throwable cause = e.getCause();
    if (cause != null) {
        System.err.println("Détails : " + cause.getMessage());
    }
}
```

### 2. Vérifier la Nullité

```java
ProgramNode program = parser.parse(file);

// Toujours vérifier null avant d'accéder
if (program.getDataDivision() != null) {
    DataDivisionNode dataDiv = program.getDataDivision();

    if (dataDiv.getWorkingStorageSection() != null) {
        // Traiter la Working-Storage
    }
}
```

### 3. Navigation Sécurisée

```java
// Utiliser les getters qui retournent des collections
List<ParagraphNode> paragraphs = procDiv.getParagraphs();
if (!paragraphs.isEmpty()) {
    for (ParagraphNode para : paragraphs) {
        // Traiter chaque paragraphe
    }
}
```

---

## 🎯 Prochaines Étapes

Une fois familiarisé avec le parser AST, vous pouvez :

1. **Implémenter votre propre Visitor** pour analyses personnalisées
2. **Créer des générateurs de code** basés sur l'AST
3. **Développer des transformations** de l'AST
4. **Intégrer avec le système existant** de génération Java

---

## 📚 Référence API

### Classes Principales

- **CobolASTParser** - Point d'entrée pour le parsing
  - `parse(Path)` - Parser un fichier
  - `parseString(String, String)` - Parser du code en mémoire
  - `isValidSyntax(Path/String)` - Valider la syntaxe

- **ProgramNode** - Racine de l'AST
  - `getProgramName()` - Nom du programme
  - `getDataDivision()` - Division DATA
  - `getProcedureDivision()` - Division PROCEDURE

- **ASTVisitor<T>** - Interface pour parcourir l'AST
  - Implémenter les méthodes `visitXXX()` nécessaires
  - Retourner un résultat de type T

### Packages

- `com.cobol.translator.parser` - Parsing et AST builder
- `com.cobol.translator.ast` - Classes de nœuds AST
- `com.cobol.translator.grammar` - Parsers ANTLR4 générés

---

## 💡 Aide et Support

- Documentation technique : [AST_IMPLEMENTATION_STATUS.md](AST_IMPLEMENTATION_STATUS.md:1)
- Résumé Phase 1 : [PHASE1_SUMMARY.md](PHASE1_SUMMARY.md:1)
- Tests unitaires : [CobolASTParserTest.java](src/test/java/com/cobol/translator/parser/CobolASTParserTest.java:1)

Bon développement ! 🚀
