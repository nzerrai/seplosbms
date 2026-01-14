package com.cobol.translator.demo;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.Statement;
import com.cobol.translator.report.ConversionReport;
import com.cobol.translator.report.ReportGenerator;

/**
 * Démo pour montrer la suppression des warnings sur patterns idiomatiques.
 */
public class PatternWarningDemo {

    public static void main(String[] args) {
        System.out.println("╔══════════════════════════════════════════════════════════════╗");
        System.out.println("║   DÉMO: Suppression Warnings Patterns Idiomatiques COBOL   ║");
        System.out.println("╚══════════════════════════════════════════════════════════════╝\n");

        // Créer un programme COBOL avec pattern standard
        CobolProgram program = new CobolProgram("CUSTOMER-PROCESSOR");
        program.setSourceFile("customer-processor.cob");

        // Ajouter les instructions du pattern OPEN-READ-PERFORM-CLOSE
        Statement openStmt = new Statement();
        openStmt.setType(Statement.StatementType.OPEN);
        openStmt.setOriginalCobol("OPEN INPUT CUSTOMER-FILE");
        openStmt.setLineNumber(10);
        program.addStatement(openStmt);

        Statement performStmt = new Statement();
        performStmt.setType(Statement.StatementType.PERFORM_UNTIL);
        performStmt.setOriginalCobol("PERFORM UNTIL WS-EOF = 'Y'");
        performStmt.setLineNumber(11);
        program.addStatement(performStmt);

        Statement readStmt = new Statement();
        readStmt.setType(Statement.StatementType.READ);
        readStmt.setOriginalCobol("READ CUSTOMER-FILE AT END MOVE 'Y' TO WS-EOF NOT AT END PERFORM 1000-PROCESS-RECORD END-READ");
        readStmt.setLineNumber(12);
        program.addStatement(readStmt);

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
        stopStmt.setOriginalCobol("STOP RUN");
        stopStmt.setLineNumber(18);
        program.addStatement(stopStmt);

        // Générer le rapport AVEC détection de patterns
        System.out.println("📊 Génération du rapport avec détection de patterns...\n");
        ReportGenerator generator = new ReportGenerator(program);
        ConversionReport report = generator.generate();

        // Afficher le rapport
        System.out.println(report.generateTextReport());

        // Résumé
        System.out.println("\n┌─────────────────────────────────────────────────────────────┐");
        System.out.println("│                         RÉSUMÉ                              │");
        System.out.println("└─────────────────────────────────────────────────────────────┘");
        System.out.println("Total statements       : " + report.getTotalStatements());
        System.out.println("Converted statements   : " + report.getConvertedStatements());
        System.out.println("Warnings générés       : " + report.getWarnings().size());
        System.out.println("Conversion percentage  : " + String.format("%.1f%%", report.getConversionPercentage()));

        if (report.getWarnings().isEmpty()) {
            System.out.println("\n✅ SUCCESS: Aucun warning généré pour ce pattern idiomatique!");
        } else {
            System.out.println("\n⚠️  Warnings restants:");
            report.getWarnings().forEach(w -> System.out.println("  - " + w));
        }

        System.out.println("\n╔══════════════════════════════════════════════════════════════╗");
        System.out.println("║           DÉMO TERMINÉE AVEC SUCCÈS                         ║");
        System.out.println("╚══════════════════════════════════════════════════════════════╝");
    }
}
