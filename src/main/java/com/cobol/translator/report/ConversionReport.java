package com.cobol.translator.report;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Compte Rendu (CR) de conversion COBOL vers Java.
 *
 * Génère un rapport détaillé avec :
 * - Pourcentage de traduction
 * - Indicateur de confiance
 * - Liste des cas non convertis
 * - Conseils sur les alternatives
 */
public class ConversionReport {

    private String sourceFile;
    private String programName;
    private LocalDateTime conversionDate;

    // Statistiques de conversion
    private int totalStatements;
    private int convertedStatements;
    private int partiallyConvertedStatements;
    private int unconvertedStatements;

    private int totalDataItems;
    private int convertedDataItems;
    private int unconvertedDataItems;

    // Indicateurs de confiance
    private ConfidenceLevel overallConfidence;
    private Map<String, Integer> statementTypeCounts = new HashMap<>();

    // Cas non convertis avec alternatives
    private List<UnconvertedCase> unconvertedCases = new ArrayList<>();

    // Avertissements
    private List<String> warnings = new ArrayList<>();
    
    // Notes positives sur les patterns idiomatiques
    private List<String> positiveNotes = new ArrayList<>();

    public ConversionReport(String sourceFile, String programName) {
        this.sourceFile = sourceFile;
        this.programName = programName;
        this.conversionDate = LocalDateTime.now();
    }

    /**
     * Ajoute un cas non converti avec son alternative.
     */
    public void addUnconvertedCase(String cobolConstruct, String reason,
                                   String alternative, String example) {
        UnconvertedCase case_ = new UnconvertedCase(
            cobolConstruct, reason, alternative, example
        );
        unconvertedCases.add(case_);
    }

    /**
     * Ajoute un avertissement.
     */
    public void addWarning(String warning) {
        warnings.add(warning);
    }
    
    /**
     * Ajoute une note positive sur un pattern idiomatique détecté.
     */
    public void addPositiveNote(String note) {
        positiveNotes.add(note);
    }

    /**
     * Calcule le pourcentage de conversion.
     */
    public double getConversionPercentage() {
        if (totalStatements == 0) return 0.0;
        return (convertedStatements * 100.0) / totalStatements;
    }

    /**
     * Calcule le pourcentage de conversion partielle.
     */
    public double getPartialConversionPercentage() {
        if (totalStatements == 0) return 0.0;
        return (partiallyConvertedStatements * 100.0) / totalStatements;
    }

    /**
     * Calcule le pourcentage d'échec.
     */
    public double getFailurePercentage() {
        if (totalStatements == 0) return 0.0;
        return (unconvertedStatements * 100.0) / totalStatements;
    }

    /**
     * Calcule l'indicateur de confiance global.
     */
    public void calculateConfidence() {
        double conversionRate = getConversionPercentage();
        double partialRate = getPartialConversionPercentage();

        // Critères pour la confiance
        if (conversionRate >= 90 && partialRate <= 5) {
            overallConfidence = ConfidenceLevel.TRES_HAUTE;
        } else if (conversionRate >= 75 && partialRate <= 15) {
            overallConfidence = ConfidenceLevel.HAUTE;
        } else if (conversionRate >= 60 && partialRate <= 25) {
            overallConfidence = ConfidenceLevel.MOYENNE;
        } else if (conversionRate >= 40) {
            overallConfidence = ConfidenceLevel.FAIBLE;
        } else {
            overallConfidence = ConfidenceLevel.TRES_FAIBLE;
        }
    }

    /**
     * Génère le rapport au format texte.
     */
    public String generateTextReport() {
        StringBuilder report = new StringBuilder();

        // En-tête
        report.append("╔══════════════════════════════════════════════════════════════════════════╗\n");
        report.append("║                                                                          ║\n");
        report.append("║           COMPTE RENDU DE CONVERSION COBOL → JAVA                       ║\n");
        report.append("║                                                                          ║\n");
        report.append("╚══════════════════════════════════════════════════════════════════════════╝\n\n");

        // Informations générales
        report.append("📄 INFORMATIONS GÉNÉRALES\n");
        report.append("═══════════════════════════════════════════════════════════════════════════\n");
        report.append(String.format("Programme COBOL  : %s\n", programName));
        report.append(String.format("Fichier source   : %s\n", sourceFile));
        report.append(String.format("Date conversion  : %s\n\n",
            conversionDate.format(DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss"))));

        // Statistiques de conversion
        report.append("📊 STATISTIQUES DE CONVERSION\n");
        report.append("═══════════════════════════════════════════════════════════════════════════\n");
        report.append(String.format("Instructions totales        : %d\n", totalStatements));
        report.append(String.format("  ✅ Converties            : %d (%.1f%%)\n",
            convertedStatements, getConversionPercentage()));
        report.append(String.format("  ⚠️  Partielles           : %d (%.1f%%)\n",
            partiallyConvertedStatements, getPartialConversionPercentage()));
        report.append(String.format("  ❌ Non converties        : %d (%.1f%%)\n\n",
            unconvertedStatements, getFailurePercentage()));

        report.append(String.format("Champs de données totaux    : %d\n", totalDataItems));
        report.append(String.format("  ✅ Convertis             : %d\n", convertedDataItems));
        report.append(String.format("  ❌ Non convertis         : %d\n\n", unconvertedDataItems));

        // Indicateur de confiance
        report.append("🎯 INDICATEUR DE CONFIANCE\n");
        report.append("═══════════════════════════════════════════════════════════════════════════\n");
        report.append(String.format("Niveau de confiance : %s %s\n\n",
            overallConfidence.getIcon(), overallConfidence.getLabel()));
        report.append(String.format("Interprétation : %s\n\n",
            overallConfidence.getDescription()));

        // Graphique visuel
        report.append("📈 RÉPARTITION VISUELLE\n");
        report.append("═══════════════════════════════════════════════════════════════════════════\n");
        report.append(generateVisualBar());
        report.append("\n\n");

        // Répartition par type d'instruction
        if (!statementTypeCounts.isEmpty()) {
            report.append("📋 RÉPARTITION PAR TYPE D'INSTRUCTION\n");
            report.append("═══════════════════════════════════════════════════════════════════════════\n");
            statementTypeCounts.entrySet().stream()
                .sorted((e1, e2) -> e2.getValue().compareTo(e1.getValue()))
                .forEach(entry ->
                    report.append(String.format("  %-20s : %3d instruction(s)\n",
                        entry.getKey(), entry.getValue()))
                );
            report.append("\n");
        }

        // Cas non convertis
        if (!unconvertedCases.isEmpty()) {
            report.append("❌ CAS NON CONVERTIS ET ALTERNATIVES\n");
            report.append("═══════════════════════════════════════════════════════════════════════════\n");
            for (int i = 0; i < unconvertedCases.size(); i++) {
                UnconvertedCase case_ = unconvertedCases.get(i);
                report.append(String.format("\n%d. %s\n", i + 1, case_.cobolConstruct));
                report.append("───────────────────────────────────────────────────────────────────────────\n");
                report.append(String.format("   Raison       : %s\n", case_.reason));
                report.append(String.format("   Alternative  : %s\n", case_.alternative));
                if (case_.example != null && !case_.example.isEmpty()) {
                    report.append(String.format("   Exemple      :\n%s\n", indent(case_.example, 6)));
                }
            }
            report.append("\n");
        }

        // Notes positives sur les patterns idiomatiques
        if (!positiveNotes.isEmpty()) {
            report.append("✅ PATTERNS IDIOMATIQUES DÉTECTÉS\n");
            report.append("═══════════════════════════════════════════════════════════════════════════\n");
            for (int i = 0; i < positiveNotes.size(); i++) {
                report.append(String.format("%d. %s\n", i + 1, positiveNotes.get(i)));
            }
            report.append("\n");
        }

        // Avertissements
        if (!warnings.isEmpty()) {
            report.append("⚠️  AVERTISSEMENTS\n");
            report.append("═══════════════════════════════════════════════════════════════════════════\n");
            for (int i = 0; i < warnings.size(); i++) {
                report.append(String.format("%d. %s\n", i + 1, warnings.get(i)));
            }
            report.append("\n");
        }

        // Recommandations
        report.append("💡 RECOMMANDATIONS\n");
        report.append("═══════════════════════════════════════════════════════════════════════════\n");
        report.append(generateRecommendations());
        report.append("\n");

        // Conclusion
        report.append("✓ CONCLUSION\n");
        report.append("═══════════════════════════════════════════════════════════════════════════\n");
        report.append(generateConclusion());

        return report.toString();
    }

    /**
     * Génère une barre visuelle de progression.
     */
    private String generateVisualBar() {
        int barWidth = 60;
        int converted = (int) (getConversionPercentage() * barWidth / 100);
        int partial = (int) (getPartialConversionPercentage() * barWidth / 100);
        int failed = barWidth - converted - partial;

        StringBuilder bar = new StringBuilder();
        bar.append("Conversion : [");
        for (int i = 0; i < converted; i++) bar.append("█");
        for (int i = 0; i < partial; i++) bar.append("▓");
        for (int i = 0; i < failed; i++) bar.append("░");
        bar.append("] ");
        bar.append(String.format("%.1f%%", getConversionPercentage()));

        return bar.toString();
    }

    /**
     * Génère les recommandations basées sur le rapport.
     */
    private String generateRecommendations() {
        StringBuilder rec = new StringBuilder();

        if (overallConfidence == ConfidenceLevel.TRES_HAUTE ||
            overallConfidence == ConfidenceLevel.HAUTE) {
            rec.append("✅ Le code généré peut être utilisé avec un minimum de révision.\n");
            rec.append("   - Effectuer une revue de code standard\n");
            rec.append("   - Tester avec des données réelles\n");
            rec.append("   - Valider les calculs financiers\n");
        } else if (overallConfidence == ConfidenceLevel.MOYENNE) {
            rec.append("⚠️  Le code généré nécessite une révision approfondie.\n");
            rec.append("   - Examiner toutes les instructions partiellement converties\n");
            rec.append("   - Implémenter manuellement les cas non convertis\n");
            rec.append("   - Tests unitaires obligatoires pour chaque fonction\n");
            rec.append("   - Comparaison des résultats COBOL vs Java\n");
        } else {
            rec.append("❌ Le code généré nécessite un travail manuel important.\n");
            rec.append("   - Considérer une réécriture manuelle pour les parties critiques\n");
            rec.append("   - S'appuyer sur les experts COBOL et Java\n");
            rec.append("   - Tester exhaustivement chaque fonctionnalité\n");
            rec.append("   - Prévoir un temps de développement supplémentaire\n");
        }

        if (!unconvertedCases.isEmpty()) {
            rec.append(String.format("\n⚠️  %d cas nécessitent une attention particulière (voir section ci-dessus).\n",
                unconvertedCases.size()));
        }

        return rec.toString();
    }

    /**
     * Génère la conclusion du rapport.
     */
    private String generateConclusion() {
        StringBuilder conclusion = new StringBuilder();

        conclusion.append(String.format("Taux de conversion automatique : %.1f%%\n",
            getConversionPercentage()));
        conclusion.append(String.format("Confiance globale : %s\n",
            overallConfidence.getLabel()));

        if (overallConfidence == ConfidenceLevel.TRES_HAUTE ||
            overallConfidence == ConfidenceLevel.HAUTE) {
            conclusion.append("\n✅ La migration est VIABLE avec un effort de révision raisonnable.\n");
        } else if (overallConfidence == ConfidenceLevel.MOYENNE) {
            conclusion.append("\n⚠️  La migration est FAISABLE mais nécessite un travail manuel significatif.\n");
        } else {
            conclusion.append("\n❌ La migration AUTOMATIQUE n'est PAS recommandée. ");
            conclusion.append("Envisager une approche hybride ou manuelle.\n");
        }

        return conclusion.toString();
    }

    /**
     * Indente un texte.
     */
    private String indent(String text, int spaces) {
        String indent = " ".repeat(spaces);
        return text.lines()
            .map(line -> indent + line)
            .reduce((a, b) -> a + "\n" + b)
            .orElse("");
    }

    // Getters et Setters

    public void setTotalStatements(int total) { this.totalStatements = total; }
    public void setConvertedStatements(int converted) { this.convertedStatements = converted; }
    public void setPartiallyConvertedStatements(int partial) {
        this.partiallyConvertedStatements = partial;
    }
    public void setUnconvertedStatements(int unconverted) {
        this.unconvertedStatements = unconverted;
    }

    public void setTotalDataItems(int total) { this.totalDataItems = total; }
    public void setConvertedDataItems(int converted) { this.convertedDataItems = converted; }
    public void setUnconvertedDataItems(int unconverted) {
        this.unconvertedDataItems = unconverted;
    }

    public void incrementStatementType(String type) {
        statementTypeCounts.merge(type, 1, Integer::sum);
    }

    // Additional getters for accessing report data
    public String getSourceFile() { return sourceFile; }
    public String getProgramName() { return programName; }
    public LocalDateTime getConversionDate() { return conversionDate; }
    public int getTotalStatements() { return totalStatements; }
    public int getConvertedStatements() { return convertedStatements; }
    public int getPartiallyConvertedStatements() { return partiallyConvertedStatements; }
    public int getUnconvertedStatements() { return unconvertedStatements; }
    public int getTotalDataItems() { return totalDataItems; }
    public int getConvertedDataItems() { return convertedDataItems; }
    public int getUnconvertedDataItems() { return unconvertedDataItems; }
    public List<UnconvertedCase> getUnconvertedCases() { return unconvertedCases; }
    public List<String> getWarnings() { return warnings; }

    public ConfidenceLevel getOverallConfidence() { return overallConfidence; }

    /**
     * Niveau de confiance dans la conversion.
     */
    public enum ConfidenceLevel {
        TRES_HAUTE("🟢", "TRÈS HAUTE",
            "Le code généré est fiable et peut être utilisé en production avec une révision minimale."),
        HAUTE("🟢", "HAUTE",
            "Le code généré est de bonne qualité et nécessite une révision standard."),
        MOYENNE("🟡", "MOYENNE",
            "Le code généré nécessite une révision approfondie et des tests approfondis."),
        FAIBLE("🟠", "FAIBLE",
            "Le code généré nécessite un travail manuel important avant utilisation."),
        TRES_FAIBLE("🔴", "TRÈS FAIBLE",
            "La conversion automatique n'est pas recommandée. Privilégier une approche manuelle.");

        private final String icon;
        private final String label;
        private final String description;

        ConfidenceLevel(String icon, String label, String description) {
            this.icon = icon;
            this.label = label;
            this.description = description;
        }

        public String getIcon() { return icon; }
        public String getLabel() { return label; }
        public String getDescription() { return description; }
    }

    /**
     * Cas non converti avec son alternative.
     */
    public static class UnconvertedCase {
        private final String cobolConstruct;
        private final String reason;
        private final String alternative;
        private final String example;

        public UnconvertedCase(String cobolConstruct, String reason,
                              String alternative, String example) {
            this.cobolConstruct = cobolConstruct;
            this.reason = reason;
            this.alternative = alternative;
            this.example = example;
        }

        public String getCobolConstruct() { return cobolConstruct; }
        public String getReason() { return reason; }
        public String getAlternative() { return alternative; }
        public String getExample() { return example; }
    }
}
