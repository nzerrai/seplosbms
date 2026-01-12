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
    private String jclFile;
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

    // Avertissements détaillés avec code Java
    private List<WarningDetail> warningDetails = new ArrayList<>();

    // Notes positives sur les patterns idiomatiques
    private List<String> positiveNotes = new ArrayList<>();

    // Analyse JCL
    private JCLAnalysis jclAnalysis;

    // Classes Java générées/impactées
    private List<GeneratedJavaClass> generatedClasses = new ArrayList<>();

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
     * Ajoute un avertissement détaillé avec contexte Java.
     */
    public void addWarningDetail(String message, String javaFile, int javaLine,
                                 String javaCodeSnippet, String cobolLine) {
        warningDetails.add(new WarningDetail(message, javaFile, javaLine, javaCodeSnippet, cobolLine));
        // Ajouter aussi à la liste simple pour compatibilité
        warnings.add(message);
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
        if (jclFile != null && !jclFile.isEmpty()) {
            report.append(String.format("Fichier JCL      : %s\n", jclFile));
        }
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

    public void setJclFile(String jclFile) {
        this.jclFile = jclFile;
    }

    public void setJclAnalysis(JCLAnalysis jclAnalysis) {
        this.jclAnalysis = jclAnalysis;
    }

    public void setGeneratedClasses(List<GeneratedJavaClass> generatedClasses) {
        this.generatedClasses = generatedClasses;
    }

    public void addGeneratedClass(GeneratedJavaClass javaClass) {
        this.generatedClasses.add(javaClass);
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
    public List<WarningDetail> getWarningDetails() { return warningDetails; }
    public List<String> getPositiveNotes() { return positiveNotes; }
    public JCLAnalysis getJclAnalysis() { return jclAnalysis; }
    public List<GeneratedJavaClass> getGeneratedClasses() { return generatedClasses; }

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

    /**
     * Traduction d'un élément JCL vers Java.
     */
    public static class JCLTranslation {
        private String jclElement;          // Type d'élément JCL (STEP, DD, IF, PROC, etc.)
        private String jclSourceCode;       // Code JCL source
        private String javaTargetCode;      // Code Java généré
        private String javaClassName;       // Nom de la classe Java impactée
        private String explanation;         // Explication de la traduction
        private TranslationType type;       // Type de traduction

        public enum TranslationType {
            STEP_EXECUTION("Step Execution", "Exécution d'un step JCL → Tasklet/Chunk Spring Batch"),
            DD_STATEMENT("DD Statement", "DD statement → FlatFileItemReader/Writer"),
            CONDITIONAL("Conditional", "IF/THEN/ELSE → JobExecutionDecider"),
            PROC_INVOCATION("PROC Invocation", "PROC call → Method call avec paramètres"),
            TEMP_DATASET("Temporary Dataset", "&&TEMP → ExecutionContext + File I/O"),
            JOB_DEFINITION("Job Definition", "JOB card → @Configuration class"),
            OTHER("Other", "Autre traduction");

            private final String label;
            private final String description;

            TranslationType(String label, String description) {
                this.label = label;
                this.description = description;
            }

            public String getLabel() { return label; }
            public String getDescription() { return description; }
        }

        public JCLTranslation() {}

        public JCLTranslation(String jclElement, String jclSourceCode, String javaTargetCode,
                             String javaClassName, TranslationType type) {
            this.jclElement = jclElement;
            this.jclSourceCode = jclSourceCode;
            this.javaTargetCode = javaTargetCode;
            this.javaClassName = javaClassName;
            this.type = type;
        }

        // Getters and setters
        public String getJclElement() { return jclElement; }
        public void setJclElement(String jclElement) { this.jclElement = jclElement; }

        public String getJclSourceCode() { return jclSourceCode; }
        public void setJclSourceCode(String jclSourceCode) { this.jclSourceCode = jclSourceCode; }

        public String getJavaTargetCode() { return javaTargetCode; }
        public void setJavaTargetCode(String javaTargetCode) { this.javaTargetCode = javaTargetCode; }

        public String getJavaClassName() { return javaClassName; }
        public void setJavaClassName(String javaClassName) { this.javaClassName = javaClassName; }

        public String getExplanation() { return explanation; }
        public void setExplanation(String explanation) { this.explanation = explanation; }

        public TranslationType getType() { return type; }
        public void setType(TranslationType type) { this.type = type; }
    }

    /**
     * Analyse du fichier JCL source.
     */
    public static class JCLAnalysis {
        private String jclFileName;
        private String jobName;
        private int totalSteps;
        private int conditionalSteps;
        private int procInvocations;
        private int temporaryDatasets;
        private List<String> stepsDetected = new ArrayList<>();
        private List<String> conditionsFound = new ArrayList<>();
        private List<String> procsUsed = new ArrayList<>();
        private List<String> tempDatasetsUsed = new ArrayList<>();
        private Map<String, String> ddStatements = new HashMap<>();
        private List<JCLTranslation> translations = new ArrayList<>();

        public JCLAnalysis() {}

        public JCLAnalysis(String jclFileName, String jobName) {
            this.jclFileName = jclFileName;
            this.jobName = jobName;
        }

        // Getters and setters
        public String getJclFileName() { return jclFileName; }
        public void setJclFileName(String jclFileName) { this.jclFileName = jclFileName; }

        public String getJobName() { return jobName; }
        public void setJobName(String jobName) { this.jobName = jobName; }

        public int getTotalSteps() { return totalSteps; }
        public void setTotalSteps(int totalSteps) { this.totalSteps = totalSteps; }

        public int getConditionalSteps() { return conditionalSteps; }
        public void setConditionalSteps(int conditionalSteps) { this.conditionalSteps = conditionalSteps; }

        public int getProcInvocations() { return procInvocations; }
        public void setProcInvocations(int procInvocations) { this.procInvocations = procInvocations; }

        public int getTemporaryDatasets() { return temporaryDatasets; }
        public void setTemporaryDatasets(int temporaryDatasets) { this.temporaryDatasets = temporaryDatasets; }

        public List<String> getStepsDetected() { return stepsDetected; }
        public void setStepsDetected(List<String> stepsDetected) { this.stepsDetected = stepsDetected; }
        public void addStep(String stepName) { this.stepsDetected.add(stepName); }

        public List<String> getConditionsFound() { return conditionsFound; }
        public void setConditionsFound(List<String> conditionsFound) { this.conditionsFound = conditionsFound; }
        public void addCondition(String condition) { this.conditionsFound.add(condition); }

        public List<String> getProcsUsed() { return procsUsed; }
        public void setProcsUsed(List<String> procsUsed) { this.procsUsed = procsUsed; }
        public void addProc(String procName) { this.procsUsed.add(procName); }

        public List<String> getTempDatasetsUsed() { return tempDatasetsUsed; }
        public void setTempDatasetsUsed(List<String> tempDatasetsUsed) { this.tempDatasetsUsed = tempDatasetsUsed; }
        public void addTempDataset(String datasetName) { this.tempDatasetsUsed.add(datasetName); }

        public Map<String, String> getDdStatements() { return ddStatements; }
        public void setDdStatements(Map<String, String> ddStatements) { this.ddStatements = ddStatements; }
        public void addDdStatement(String ddName, String dataset) { this.ddStatements.put(ddName, dataset); }

        public List<JCLTranslation> getTranslations() { return translations; }
        public void setTranslations(List<JCLTranslation> translations) { this.translations = translations; }
        public void addTranslation(JCLTranslation translation) { this.translations.add(translation); }
    }

    /**
     * Classe Java générée ou impactée par la traduction.
     */
    public static class GeneratedJavaClass {
        private String className;
        private String packageName;
        private String filePath;
        private ClassType type;
        private int linesOfCode;
        private String purpose;
        private boolean isNew;
        private List<String> methods = new ArrayList<>();

        public enum ClassType {
            CONFIGURATION("Configuration", "Configuration Spring Batch"),
            PROCESSOR("Processor", "ItemProcessor pour traitement"),
            READER("Reader", "ItemReader pour lecture"),
            WRITER("Writer", "ItemWriter pour écriture"),
            ENTITY("Entity", "Classe entité JPA"),
            REPOSITORY("Repository", "Repository JPA"),
            VALIDATOR("Validator", "Validator de données"),
            LISTENER("Listener", "JobExecutionListener ou StepExecutionListener"),
            DECIDER("Decider", "JobExecutionDecider pour conditions"),
            MAPPER("Mapper", "Mapper de données"),
            UTILITY("Utility", "Classe utilitaire"),
            OTHER("Other", "Autre type");

            private final String label;
            private final String description;

            ClassType(String label, String description) {
                this.label = label;
                this.description = description;
            }

            public String getLabel() { return label; }
            public String getDescription() { return description; }
        }

        public GeneratedJavaClass() {}

        public GeneratedJavaClass(String className, String packageName, ClassType type) {
            this.className = className;
            this.packageName = packageName;
            this.type = type;
            this.isNew = true;
        }

        public String getFullClassName() {
            return packageName + "." + className;
        }

        // Getters and setters
        public String getClassName() { return className; }
        public void setClassName(String className) { this.className = className; }

        public String getPackageName() { return packageName; }
        public void setPackageName(String packageName) { this.packageName = packageName; }

        public String getFilePath() { return filePath; }
        public void setFilePath(String filePath) { this.filePath = filePath; }

        public ClassType getType() { return type; }
        public void setType(ClassType type) { this.type = type; }

        public int getLinesOfCode() { return linesOfCode; }
        public void setLinesOfCode(int linesOfCode) { this.linesOfCode = linesOfCode; }

        public String getPurpose() { return purpose; }
        public void setPurpose(String purpose) { this.purpose = purpose; }

        public boolean isNew() { return isNew; }
        public void setNew(boolean isNew) { this.isNew = isNew; }

        public List<String> getMethods() { return methods; }
        public void setMethods(List<String> methods) { this.methods = methods; }
        public void addMethod(String methodName) { this.methods.add(methodName); }
    }

    /**
     * Détail d'un avertissement avec contexte Java.
     */
    public static class WarningDetail {
        private String message;            // Message d'avertissement
        private String javaFile;          // Nom du fichier Java (ex: "DataProcessor.java")
        private int javaLine;             // Ligne dans le fichier Java
        private String javaCodeSnippet;   // Extrait du code Java (5-10 lignes autour)
        private String cobolLine;         // Ligne COBOL d'origine (optionnel)

        public WarningDetail(String message, String javaFile, int javaLine,
                            String javaCodeSnippet, String cobolLine) {
            this.message = message;
            this.javaFile = javaFile;
            this.javaLine = javaLine;
            this.javaCodeSnippet = javaCodeSnippet;
            this.cobolLine = cobolLine;
        }

        // Getters
        public String getMessage() { return message; }
        public String getJavaFile() { return javaFile; }
        public int getJavaLine() { return javaLine; }
        public String getJavaCodeSnippet() { return javaCodeSnippet; }
        public String getCobolLine() { return cobolLine; }

        // Setters
        public void setMessage(String message) { this.message = message; }
        public void setJavaFile(String javaFile) { this.javaFile = javaFile; }
        public void setJavaLine(int javaLine) { this.javaLine = javaLine; }
        public void setJavaCodeSnippet(String javaCodeSnippet) { this.javaCodeSnippet = javaCodeSnippet; }
        public void setCobolLine(String cobolLine) { this.cobolLine = cobolLine; }
    }
}
