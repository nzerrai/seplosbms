package com.cobol.translator.report;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.model.Statement;

import java.util.HashSet;
import java.util.Set;

/**
 * Génère un rapport de conversion détaillé en analysant le programme COBOL
 * et les résultats de la traduction.
 */
public class ReportGenerator {

    private final CobolProgram program;
    private final ConversionReport report;

    // Constructions COBOL non supportées
    private static final Set<String> UNSUPPORTED_KEYWORDS = new HashSet<>();
    static {
        UNSUPPORTED_KEYWORDS.add("EXEC CICS");
        UNSUPPORTED_KEYWORDS.add("EXEC SQL");
        UNSUPPORTED_KEYWORDS.add("SORT");
        UNSUPPORTED_KEYWORDS.add("MERGE");
        UNSUPPORTED_KEYWORDS.add("INSPECT");
        UNSUPPORTED_KEYWORDS.add("STRING");
        UNSUPPORTED_KEYWORDS.add("UNSTRING");
    }

    // Constructions partiellement supportées
    private static final Set<String> PARTIAL_KEYWORDS = new HashSet<>();
    static {
        PARTIAL_KEYWORDS.add("EVALUATE");
        PARTIAL_KEYWORDS.add("SEARCH ALL");
        PARTIAL_KEYWORDS.add("REDEFINES");
    }

    public ReportGenerator(CobolProgram program) {
        this.program = program;
        this.report = new ConversionReport(
            program.getSourceFile() != null ? program.getSourceFile() : "unknown",
            program.getProgramName()
        );
    }

    /**
     * Analyse le programme COBOL et génère le rapport complet.
     */
    public ConversionReport generate() {
        analyzeDataItems();
        analyzeStatements();
        report.calculateConfidence();
        return report;
    }

    /**
     * Analyse les éléments de données (DATA DIVISION).
     */
    private void analyzeDataItems() {
        int total = program.getDataItems().size();
        int converted = 0;
        int unconverted = 0;

        for (DataItem item : program.getDataItems()) {
            if (isDataItemConvertible(item)) {
                converted++;
            } else {
                unconverted++;
                addDataItemWarning(item);
            }
        }

        report.setTotalDataItems(total);
        report.setConvertedDataItems(converted);
        report.setUnconvertedDataItems(unconverted);
    }

    /**
     * Analyse les instructions (PROCEDURE DIVISION).
     */
    private void analyzeStatements() {
        int total = program.getStatements().size();
        int converted = 0;
        int partial = 0;
        int unconverted = 0;

        for (Statement stmt : program.getStatements()) {
            String type = stmt.getType().toString();
            report.incrementStatementType(type);

            ConversionStatus status = getStatementConversionStatus(stmt);

            switch (status) {
                case CONVERTED:
                    converted++;
                    break;
                case PARTIAL:
                    partial++;
                    addPartialConversionCase(stmt);
                    break;
                case UNCONVERTED:
                    unconverted++;
                    addUnconvertedCase(stmt);
                    break;
            }
        }

        report.setTotalStatements(total);
        report.setConvertedStatements(converted);
        report.setPartiallyConvertedStatements(partial);
        report.setUnconvertedStatements(unconverted);
    }

    /**
     * Détermine si un DataItem peut être converti.
     */
    private boolean isDataItemConvertible(DataItem item) {
        // REDEFINES complexes
        if (item.getPictureClause() != null &&
            item.getPictureClause().contains("REDEFINES") &&
            isComplexRedefines(item)) {
            return false;
        }

        // Types non supportés
        if (item.getPictureClause() != null &&
            (item.getPictureClause().contains("POINTER") ||
             item.getPictureClause().contains("FUNCTION-POINTER"))) {
            return false;
        }

        return true;
    }

    /**
     * Détermine le statut de conversion d'une instruction.
     */
    private ConversionStatus getStatementConversionStatus(Statement stmt) {
        String code = stmt.getOriginalCobol() != null ? stmt.getOriginalCobol().toUpperCase() : "";

        // Non supporté
        for (String keyword : UNSUPPORTED_KEYWORDS) {
            if (code.contains(keyword)) {
                return ConversionStatus.UNCONVERTED;
            }
        }

        // Partiellement supporté
        for (String keyword : PARTIAL_KEYWORDS) {
            if (code.contains(keyword)) {
                return ConversionStatus.PARTIAL;
            }
        }

        // Instructions basiques supportées
        switch (stmt.getType()) {
            case MOVE:
            case COMPUTE:
            case IF:
            case PERFORM:
            case READ:
            case WRITE:
            case DISPLAY:
            case STOP_RUN:
                return ConversionStatus.CONVERTED;
            default:
                return ConversionStatus.PARTIAL;
        }
    }

    /**
     * Ajoute un cas de conversion partielle au rapport.
     */
    private void addPartialConversionCase(Statement stmt) {
        String code = stmt.getOriginalCobol() != null ? stmt.getOriginalCobol().toUpperCase() : "";

        if (code.contains("EVALUATE")) {
            report.addUnconvertedCase(
                "EVALUATE (switch multiple)",
                "Traduit en switch Java mais nécessite révision manuelle",
                "Utiliser switch Java avec validation des cas",
                "switch (field) {\n" +
                "    case VALUE1: /* traitement */ break;\n" +
                "    case VALUE2: /* traitement */ break;\n" +
                "    default: /* traitement */ break;\n" +
                "}"
            );
        } else if (code.contains("SEARCH ALL")) {
            report.addUnconvertedCase(
                "SEARCH ALL (recherche binaire)",
                "Traduit en recherche linéaire, optimisation perdue",
                "Utiliser Collections.binarySearch() pour tableaux triés",
                "int index = Collections.binarySearch(list, key);\n" +
                "if (index >= 0) {\n" +
                "    // Élément trouvé\n" +
                "}"
            );
        } else if (code.contains("REDEFINES")) {
            report.addUnconvertedCase(
                "REDEFINES (redéfinition de zone mémoire)",
                "Union de types non supportée directement en Java",
                "Créer des méthodes de conversion explicites",
                "public String getFieldAsString() {\n" +
                "    return String.valueOf(numericField);\n" +
                "}\n" +
                "public int getFieldAsInt() {\n" +
                "    return Integer.parseInt(stringField);\n" +
                "}"
            );
        }

        report.addWarning("Instruction partiellement convertie ligne " +
            stmt.getLineNumber() + ": " + stmt.getType());
    }

    /**
     * Ajoute un cas non converti au rapport.
     */
    private void addUnconvertedCase(Statement stmt) {
        String code = stmt.getOriginalCobol() != null ? stmt.getOriginalCobol().toUpperCase() : "";

        if (code.contains("EXEC CICS")) {
            report.addUnconvertedCase(
                "EXEC CICS (transactions online)",
                "CICS est un moniteur transactionnel mainframe sans équivalent direct",
                "Réécrire en REST API ou service Spring MVC",
                "@RestController\n" +
                "public class TransactionController {\n" +
                "    @PostMapping(\"/transaction\")\n" +
                "    public ResponseEntity<?> processTransaction(@RequestBody Request req) {\n" +
                "        // Logique métier\n" +
                "    }\n" +
                "}"
            );
        } else if (code.contains("EXEC SQL")) {
            report.addUnconvertedCase(
                "EXEC SQL (DB2 embedded SQL)",
                "SQL embarqué nécessite conversion vers JDBC ou JPA",
                "Utiliser Spring Data JPA ou JdbcTemplate",
                "@Repository\n" +
                "public interface CustomerRepository extends JpaRepository<Customer, Long> {\n" +
                "    @Query(\"SELECT c FROM Customer c WHERE c.status = :status\")\n" +
                "    List<Customer> findByStatus(@Param(\"status\") String status);\n" +
                "}"
            );
        } else if (code.contains("SORT")) {
            report.addUnconvertedCase(
                "SORT (tri de fichiers)",
                "SORT COBOL opère sur fichiers, Java Collections sur objets en mémoire",
                "Utiliser Stream API ou Collections.sort()",
                "List<Record> sortedRecords = records.stream()\n" +
                "    .sorted(Comparator.comparing(Record::getKey))\n" +
                "    .collect(Collectors.toList());"
            );
        } else if (code.contains("INSPECT") || code.contains("STRING") || code.contains("UNSTRING")) {
            report.addUnconvertedCase(
                code.contains("INSPECT") ? "INSPECT (manipulation de chaînes)" :
                code.contains("STRING") ? "STRING (concaténation)" : "UNSTRING (découpage)",
                "Sémantique complexe avec compteurs et positions",
                "Utiliser String.replace(), StringBuilder, ou String.split()",
                "// INSPECT REPLACING\n" +
                "String result = input.replace(\"OLD\", \"NEW\");\n\n" +
                "// STRING\n" +
                "String result = new StringBuilder()\n" +
                "    .append(field1).append(field2).toString();\n\n" +
                "// UNSTRING\n" +
                "String[] parts = input.split(\" \");"
            );
        }

        report.addWarning("Instruction NON convertie ligne " +
            stmt.getLineNumber() + ": " + stmt.getType());
    }

    /**
     * Ajoute un avertissement pour un DataItem non convertible.
     */
    private void addDataItemWarning(DataItem item) {
        if (isComplexRedefines(item)) {
            report.addWarning("REDEFINES complexe détecté: " + item.getName() +
                " - nécessite conversion manuelle");
        } else if (item.getPictureClause() != null &&
                   item.getPictureClause().contains("POINTER")) {
            report.addWarning("Type POINTER non supporté: " + item.getName());
        }
    }

    /**
     * Détermine si un REDEFINES est complexe (plusieurs niveaux).
     */
    private boolean isComplexRedefines(DataItem item) {
        // Simplifié - en production, analyser la structure complète
        if (item.getPictureClause() == null) return false;
        String pic = item.getPictureClause();
        return pic.contains("REDEFINES") &&
               (item.getLevel() < 5 || pic.contains("OCCURS"));
    }

    /**
     * Statut de conversion d'une instruction.
     */
    private enum ConversionStatus {
        CONVERTED,    // Conversion complète automatique
        PARTIAL,      // Conversion partielle, révision nécessaire
        UNCONVERTED   // Non converti, implémentation manuelle requise
    }
}
