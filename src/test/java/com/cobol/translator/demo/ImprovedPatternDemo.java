package com.cobol.translator.demo;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.Statement;
import com.cobol.translator.report.ConversionReport;
import com.cobol.translator.report.ReportGenerator;

/**
 * Démo pour montrer l'amélioration avec instructions ADD et IF.
 */
public class ImprovedPatternDemo {

    public static void main(String[] args) {
        System.out.println("╔════════════════════════════════════════════════════════════════╗");
        System.out.println("║   DÉMO: Pattern Idiomatique avec ADD et IF                    ║");
        System.out.println("╚════════════════════════════════════════════════════════════════╝\n");

        // Créer un programme COBOL avec toutes les instructions
        CobolProgram program = new CobolProgram("CUSTOMER-PROCESSOR");
        program.setSourceFile("customer-processor.cob");

        // Pattern complet incluant ADD, IF, MOVE
        Statement openStmt = new Statement();
        openStmt.setType(Statement.StatementType.OPEN);
        openStmt.setOriginalCobol("OPEN INPUT CUSTOMER-FILE");
        openStmt.setLineNumber(10);
        program.addStatement(openStmt);

        Statement performStmt = new Statement();
        performStmt.setType(Statement.StatementType.PERFORM_UNTIL);
        performStmt.setOriginalCobol("PERFORM UNTIL WS-EOF = 'Y'");
        performStmt.setLineNumber(11);
        performStmt.setUntilCondition("WS-EOF = 'Y'");
        program.addStatement(performStmt);

        Statement readStmt = new Statement();
        readStmt.setType(Statement.StatementType.READ);
        readStmt.setOriginalCobol("READ CUSTOMER-FILE AT END MOVE 'Y' TO WS-EOF NOT AT END PERFORM 1000-PROCESS-RECORD END-READ");
        readStmt.setLineNumber(12);
        program.addStatement(readStmt);

        Statement moveStmt = new Statement();
        moveStmt.setType(Statement.StatementType.MOVE);
        moveStmt.setOriginalCobol("AT END MOVE 'Y' TO WS-EOF");
        moveStmt.setLineNumber(13);
        program.addStatement(moveStmt);

        Statement performProcStmt = new Statement();
        performProcStmt.setType(Statement.StatementType.PERFORM);
        performProcStmt.setOriginalCobol("NOT AT END PERFORM 1000-PROCESS-RECORD");
        performProcStmt.setLineNumber(14);
        program.addStatement(performProcStmt);

        Statement closeStmt = new Statement();
        closeStmt.setType(Statement.StatementType.CLOSE);
        closeStmt.setOriginalCobol("CLOSE CUSTOMER-FILE");
        closeStmt.setLineNumber(16);
        program.addStatement(closeStmt);

        Statement displayStmt = new Statement();
        displayStmt.setType(Statement.StatementType.DISPLAY);
        displayStmt.setOriginalCobol("DISPLAY 'PROCESSED: ' WS-COUNT");
        displayStmt.setLineNumber(17);
        program.addStatement(displayStmt);

        Statement stopStmt = new Statement();
        stopStmt.setType(Statement.StatementType.STOP_RUN);
        stopStmt.setOriginalCobol("STOP RUN.");
        stopStmt.setLineNumber(18);
        program.addStatement(stopStmt);

        Statement addStmt = new Statement();
        addStmt.setType(Statement.StatementType.ADD);
        addStmt.setOriginalCobol("ADD 1 TO WS-COUNT");
        addStmt.setLineNumber(19);
        program.addStatement(addStmt);

        Statement ifStmt = new Statement();
        ifStmt.setType(Statement.StatementType.IF);
        ifStmt.setOriginalCobol("IF CUST-AMOUNT > 1000");
        ifStmt.setLineNumber(20);
        program.addStatement(ifStmt);

        Statement displayHighStmt = new Statement();
        displayHighStmt.setType(Statement.StatementType.DISPLAY);
        displayHighStmt.setOriginalCobol("DISPLAY 'HIGH VALUE: ' CUST-NAME");
        displayHighStmt.setLineNumber(21);
        program.addStatement(displayHighStmt);

        // Générer le rapport AVEC détection de patterns
        System.out.println("📊 Génération du rapport avec détection de patterns...\n");
        ReportGenerator generator = new ReportGenerator(program);
        ConversionReport report = generator.generate();

        // Afficher le rapport
        System.out.println(report.generateTextReport());

        // Résumé détaillé
        System.out.println("\n┌─────────────────────────────────────────────────────────────┐");
        System.out.println("│                     RÉSUMÉ DÉTAILLÉ                         │");
        System.out.println("└─────────────────────────────────────────────────────────────┘");
        System.out.println("Total statements       : " + report.getTotalStatements());
        System.out.println("Converted statements   : " + report.getConvertedStatements());
        System.out.println("Warnings générés       : " + report.getWarnings().size());
        System.out.println("Conversion percentage  : " + String.format("%.1f%%", report.getConversionPercentage()));
        System.out.println();
        
        if (report.getWarnings().isEmpty()) {
            System.out.println("✅ SUCCÈS TOTAL: Aucun warning généré!");
            System.out.println("   Toutes les instructions sont reconnues comme idiomatiques:");
            System.out.println("   - OPEN, READ, PERFORM, CLOSE ✓");
            System.out.println("   - DISPLAY, STOP RUN ✓");
            System.out.println("   - ADD, IF, MOVE ✓");
        } else {
            System.out.println("⚠️  Warnings restants: " + report.getWarnings().size());
            report.getWarnings().forEach(w -> System.out.println("  - " + w));
        }

        // Métriques avant/après
        System.out.println("\n┌─────────────────────────────────────────────────────────────┐");
        System.out.println("│                   MÉTRIQUES AVANT/APRÈS                     │");
        System.out.println("└─────────────────────────────────────────────────────────────┘");
        System.out.println("AVANT correction:");
        System.out.println("  ⚠️  11 warnings (1 par instruction)");
        System.out.println("  📊 Conversion: ~50%");
        System.out.println("  🔴 Confiance: FAIBLE");
        System.out.println();
        System.out.println("APRÈS correction:");
        System.out.println("  ✅ " + report.getWarnings().size() + " warnings");
        System.out.println("  📊 Conversion: " + String.format("%.0f%%", report.getConversionPercentage()));
        System.out.println("  🟢 Confiance: " + report.getOverallConfidence());

        System.out.println("\n╔════════════════════════════════════════════════════════════════╗");
        System.out.println("║           DÉMO TERMINÉE - PATTERN AMÉLIORÉ                    ║");
        System.out.println("╚════════════════════════════════════════════════════════════════╝");
    }
}
