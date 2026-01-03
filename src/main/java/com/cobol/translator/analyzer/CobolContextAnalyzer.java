package com.cobol.translator.analyzer;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.model.Statement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Analyseur de contexte COBOL avancé.
 *
 * Fournit une analyse contextuelle approfondie pour améliorer la fiabilité
 * de la traduction:
 * - Analyse de flux de données (data flow)
 * - Détection des dépendances entre variables
 * - Validation sémantique
 * - Détection de patterns et anti-patterns
 */
public class CobolContextAnalyzer {

    private static final Logger logger = LoggerFactory.getLogger(CobolContextAnalyzer.class);

    private final CobolProgram program;
    private Map<String, DataItem> dataItemIndex;
    private Map<String, List<Statement>> variableUsages;
    private Map<String, Set<String>> dataDependencies;
    private List<AnalysisWarning> warnings;

    public CobolContextAnalyzer(CobolProgram program) {
        this.program = program;
        this.dataItemIndex = new HashMap<>();
        this.variableUsages = new HashMap<>();
        this.dataDependencies = new HashMap<>();
        this.warnings = new ArrayList<>();
    }

    /**
     * Exécute l'analyse contextuelle complète.
     */
    public AnalysisResult analyze() {
        logger.info("Starting contextual analysis of COBOL program: {}", program.getProgramName());

        // Phase 1: Indexation des données
        indexDataItems();

        // Phase 2: Analyse du flux de données
        analyzeDataFlow();

        // Phase 3: Détection des dépendances
        analyzeDependencies();

        // Phase 4: Validation sémantique
        performSemanticValidation();

        // Phase 5: Détection de patterns
        detectPatterns();

        // Phase 6: Analyse de complexité
        analyzeComplexity();

        logger.info("Contextual analysis completed with {} warnings", warnings.size());

        return new AnalysisResult(
            dataItemIndex,
            variableUsages,
            dataDependencies,
            warnings
        );
    }

    /**
     * Phase 1: Indexe tous les data items pour un accès rapide.
     */
    private void indexDataItems() {
        logger.debug("Indexing {} data items", program.getDataItems().size());

        for (DataItem item : program.getDataItems()) {
            dataItemIndex.put(item.getName().toUpperCase(), item);
            dataItemIndex.put(item.getJavaFieldName(), item);
        }

        logger.debug("Data item index created with {} entries", dataItemIndex.size());
    }

    /**
     * Phase 2: Analyse le flux de données à travers les statements.
     */
    private void analyzeDataFlow() {
        logger.debug("Analyzing data flow across {} statements", program.getStatements().size());

        for (Statement stmt : program.getStatements()) {
            analyzeStatementDataFlow(stmt);
        }

        logger.debug("Data flow analysis found usages for {} variables", variableUsages.size());
    }

    /**
     * Analyse le flux de données pour un statement particulier.
     */
    private void analyzeStatementDataFlow(Statement stmt) {
        String cobol = stmt.getOriginalCobol();
        if (cobol == null) return;

        // Extraire les variables mentionnées dans le statement
        Set<String> mentionedVars = extractVariables(cobol);

        for (String var : mentionedVars) {
            variableUsages.computeIfAbsent(var, k -> new ArrayList<>()).add(stmt);
        }

        // Analyser les patterns MOVE pour détecter les flux
        if (stmt.getType() == Statement.StatementType.MOVE) {
            analyzeMoveStatement(stmt);
        }
    }

    /**
     * Extrait les noms de variables d'un statement COBOL.
     */
    private Set<String> extractVariables(String cobolCode) {
        Set<String> variables = new HashSet<>();

        // Pattern pour identifier les identifiants COBOL (mots avec tirets)
        Pattern varPattern = Pattern.compile("\\b([A-Z][A-Z0-9-]+)\\b");
        Matcher matcher = varPattern.matcher(cobolCode.toUpperCase());

        while (matcher.find()) {
            String candidate = matcher.group(1);

            // Vérifier si c'est un data item connu (pas un mot-clé)
            if (dataItemIndex.containsKey(candidate) && !isCobolKeyword(candidate)) {
                variables.add(candidate);
            }
        }

        return variables;
    }

    /**
     * Analyse un statement MOVE pour détecter les dépendances.
     */
    private void analyzeMoveStatement(Statement stmt) {
        String cobol = stmt.getOriginalCobol();

        // Pattern MOVE source TO target
        Pattern movePattern = Pattern.compile(
            "MOVE\\s+(.+?)\\s+TO\\s+(.+)",
            Pattern.CASE_INSENSITIVE
        );

        Matcher matcher = movePattern.matcher(cobol);
        if (matcher.find()) {
            String source = matcher.group(1).trim();
            String target = matcher.group(2).trim();

            // Extraire les variables source et cible
            Set<String> sourceVars = extractVariables(source);
            Set<String> targetVars = extractVariables(target);

            // Enregistrer la dépendance: target dépend de source
            for (String targetVar : targetVars) {
                dataDependencies.computeIfAbsent(targetVar, k -> new HashSet<>())
                    .addAll(sourceVars);
            }

            stmt.setSource(source);
            stmt.setTarget(target);
        }
    }

    /**
     * Phase 3: Analyse les dépendances entre variables.
     */
    private void analyzeDependencies() {
        logger.debug("Analyzing dependencies between variables");

        // Calculer les dépendances transitives
        Map<String, Set<String>> transitiveDeps = computeTransitiveDependencies();

        // Détecter les cycles de dépendances (potentiellement problématiques)
        for (String var : transitiveDeps.keySet()) {
            if (transitiveDeps.get(var).contains(var)) {
                warnings.add(new AnalysisWarning(
                    WarningLevel.MEDIUM,
                    "Circular dependency detected",
                    "Variable " + var + " has circular dependencies",
                    "Review the data flow logic"
                ));
            }
        }

        logger.debug("Dependency analysis found {} dependency chains", transitiveDeps.size());
    }

    /**
     * Calcule les dépendances transitives.
     */
    private Map<String, Set<String>> computeTransitiveDependencies() {
        Map<String, Set<String>> transitive = new HashMap<>();

        for (String var : dataDependencies.keySet()) {
            transitive.put(var, new HashSet<>());
            computeTransitiveForVariable(var, transitive.get(var), new HashSet<>());
        }

        return transitive;
    }

    /**
     * Calcule récursivement les dépendances transitives pour une variable.
     */
    private void computeTransitiveForVariable(String var, Set<String> result, Set<String> visited) {
        if (visited.contains(var)) {
            return; // Éviter les cycles infinis
        }

        visited.add(var);
        Set<String> directDeps = dataDependencies.get(var);

        if (directDeps != null) {
            result.addAll(directDeps);
            for (String dep : directDeps) {
                computeTransitiveForVariable(dep, result, visited);
            }
        }
    }

    /**
     * Phase 4: Validation sémantique du programme.
     */
    private void performSemanticValidation() {
        logger.debug("Performing semantic validation");

        // Vérifier l'utilisation des variables
        validateVariableUsage();

        // Vérifier les types de données
        validateDataTypes();

        // Vérifier la structure de contrôle
        validateControlFlow();

        logger.debug("Semantic validation completed");
    }

    /**
     * Valide l'utilisation des variables.
     */
    private void validateVariableUsage() {
        // Détecter les variables non utilisées
        Set<String> definedVars = dataItemIndex.keySet();
        Set<String> usedVars = variableUsages.keySet();

        for (String var : definedVars) {
            if (!usedVars.contains(var)) {
                DataItem item = dataItemIndex.get(var);
                if (item != null && item.getLevel() == 1) {
                    // Variable de niveau 01 non utilisée
                    warnings.add(new AnalysisWarning(
                        WarningLevel.LOW,
                        "Unused variable",
                        "Variable " + var + " is defined but never used",
                        "Consider removing unused variables"
                    ));
                }
            }
        }

        // Détecter les variables référencées mais non définies
        for (String var : usedVars) {
            if (!definedVars.contains(var) && !isCobolKeyword(var)) {
                warnings.add(new AnalysisWarning(
                    WarningLevel.HIGH,
                    "Undefined variable",
                    "Variable " + var + " is used but not defined",
                    "Add variable definition or check for typos"
                ));
            }
        }
    }

    /**
     * Valide la cohérence des types de données.
     */
    private void validateDataTypes() {
        for (Statement stmt : program.getStatements()) {
            if (stmt.getType() == Statement.StatementType.MOVE) {
                validateMoveTypeCompatibility(stmt);
            }
        }
    }

    /**
     * Valide la compatibilité des types dans un MOVE.
     */
    private void validateMoveTypeCompatibility(Statement stmt) {
        if (stmt.getSource() == null || stmt.getTarget() == null) {
            return;
        }

        Set<String> sourceVars = extractVariables(stmt.getSource());
        Set<String> targetVars = extractVariables(stmt.getTarget());

        for (String sourceVar : sourceVars) {
            for (String targetVar : targetVars) {
                DataItem source = dataItemIndex.get(sourceVar);
                DataItem target = dataItemIndex.get(targetVar);

                if (source != null && target != null) {
                    if (!areTypesCompatible(source, target)) {
                        warnings.add(new AnalysisWarning(
                            WarningLevel.MEDIUM,
                            "Type incompatibility",
                            String.format("MOVE from %s (%s) to %s (%s) may lose data",
                                sourceVar, source.getJavaType(),
                                targetVar, target.getJavaType()),
                            "Review type compatibility and add explicit conversion if needed"
                        ));
                    }
                }
            }
        }
    }

    /**
     * Vérifie la compatibilité entre deux types.
     */
    private boolean areTypesCompatible(DataItem source, DataItem target) {
        String sourceType = source.getJavaType();
        String targetType = target.getJavaType();

        if (sourceType == null || targetType == null) {
            return true; // Assume compatible if unknown
        }

        // Règles de compatibilité
        if (sourceType.equals(targetType)) {
            return true;
        }

        // String compatible avec tout
        if (targetType.equals("String")) {
            return true;
        }

        // Numérique vers numérique (avec avertissement potentiel)
        Set<String> numericTypes = Set.of("Integer", "Long", "BigDecimal", "Double");
        if (numericTypes.contains(sourceType) && numericTypes.contains(targetType)) {
            return true; // Compatible mais peut nécessiter conversion
        }

        return false;
    }

    /**
     * Valide la structure de contrôle.
     */
    private void validateControlFlow() {
        // Compter les niveaux d'imbrication
        int maxNesting = 0;
        int currentNesting = 0;

        for (Statement stmt : program.getStatements()) {
            if (stmt.getType() == Statement.StatementType.IF ||
                stmt.getType() == Statement.StatementType.PERFORM ||
                stmt.getType() == Statement.StatementType.PERFORM_UNTIL) {
                currentNesting++;
                maxNesting = Math.max(maxNesting, currentNesting);
            }

            // Note: Dans un parser réel, on devrait détecter les END-IF, etc.
        }

        if (maxNesting > 5) {
            warnings.add(new AnalysisWarning(
                WarningLevel.MEDIUM,
                "High complexity",
                "Control flow nesting level is " + maxNesting,
                "Consider refactoring for better readability"
            ));
        }
    }

    /**
     * Phase 5: Détection de patterns COBOL courants.
     */
    private void detectPatterns() {
        logger.debug("Detecting COBOL patterns");

        detectAccumulatorPattern();
        detectFileProcessingPattern();
        detectTableSearchPattern();

        logger.debug("Pattern detection completed");
    }

    /**
     * Détecte le pattern d'accumulateur (compteur/total).
     */
    private void detectAccumulatorPattern() {
        // Chercher les variables utilisées dans ADD ... TO ...
        for (Statement stmt : program.getStatements()) {
            if (stmt.getType() == Statement.StatementType.ADD) {
                String cobol = stmt.getOriginalCobol();
                if (cobol != null && cobol.toUpperCase().contains(" TO ")) {
                    // C'est probablement un accumulateur
                    Set<String> vars = extractVariables(cobol);
                    for (String var : vars) {
                        DataItem item = dataItemIndex.get(var);
                        if (item != null && item.isNumeric()) {
                            item.setPattern("ACCUMULATOR");
                        }
                    }
                }
            }
        }
    }

    /**
     * Détecte le pattern de traitement de fichier.
     */
    private void detectFileProcessingPattern() {
        boolean hasRead = false;
        boolean hasWrite = false;
        boolean hasLoop = false;

        for (Statement stmt : program.getStatements()) {
            if (stmt.getType() == Statement.StatementType.READ) hasRead = true;
            if (stmt.getType() == Statement.StatementType.WRITE) hasWrite = true;
            if (stmt.getType() == Statement.StatementType.PERFORM_UNTIL) hasLoop = true;
        }

        if (hasRead && hasLoop) {
            logger.info("Detected file processing pattern");
            program.setPattern("FILE_PROCESSING");
        }
    }

    /**
     * Détecte le pattern de recherche dans un tableau.
     */
    private void detectTableSearchPattern() {
        for (Statement stmt : program.getStatements()) {
            if (stmt.getType() == Statement.StatementType.SEARCH) {
                logger.info("Detected table search pattern");
                program.setPattern("TABLE_SEARCH");
            }
        }
    }

    /**
     * Phase 6: Analyse de la complexité du programme.
     */
    private void analyzeComplexity() {
        logger.debug("Analyzing program complexity");

        int cyclomaticComplexity = calculateCyclomaticComplexity();
        int dataComplexity = program.getDataItems().size();
        int statementComplexity = program.getStatements().size();

        logger.info("Complexity metrics: cyclomatic={}, data items={}, statements={}",
            cyclomaticComplexity, dataComplexity, statementComplexity);

        if (cyclomaticComplexity > 20) {
            warnings.add(new AnalysisWarning(
                WarningLevel.HIGH,
                "High cyclomatic complexity",
                "Cyclomatic complexity is " + cyclomaticComplexity,
                "Consider breaking down into smaller methods/paragraphs"
            ));
        }
    }

    /**
     * Calcule la complexité cyclomatique.
     */
    private int calculateCyclomaticComplexity() {
        int complexity = 1; // Base complexity

        for (Statement stmt : program.getStatements()) {
            switch (stmt.getType()) {
                case IF:
                case EVALUATE:
                case PERFORM_UNTIL:
                case SEARCH:
                    complexity++;
                    break;
                default:
                    break;
            }
        }

        return complexity;
    }

    /**
     * Vérifie si un mot est un mot-clé COBOL.
     */
    private boolean isCobolKeyword(String word) {
        Set<String> keywords = Set.of(
            "MOVE", "TO", "FROM", "COMPUTE", "IF", "THEN", "ELSE", "END-IF",
            "PERFORM", "UNTIL", "TIMES", "READ", "WRITE", "OPEN", "CLOSE",
            "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "DISPLAY", "ACCEPT",
            "STOP", "RUN", "GO", "GOBACK", "EXIT", "EVALUATE", "WHEN",
            "VALUE", "VALUES", "PIC", "PICTURE", "USAGE", "COMP", "COMP-3",
            "BINARY", "OCCURS", "REDEFINES", "AT", "END", "NOT", "AND", "OR",
            "EQUAL", "GREATER", "LESS", "THAN", "ZERO", "SPACE", "SPACES"
        );

        return keywords.contains(word);
    }

    /**
     * Résultat de l'analyse.
     */
    public static class AnalysisResult {
        private final Map<String, DataItem> dataItemIndex;
        private final Map<String, List<Statement>> variableUsages;
        private final Map<String, Set<String>> dataDependencies;
        private final List<AnalysisWarning> warnings;

        public AnalysisResult(
            Map<String, DataItem> dataItemIndex,
            Map<String, List<Statement>> variableUsages,
            Map<String, Set<String>> dataDependencies,
            List<AnalysisWarning> warnings
        ) {
            this.dataItemIndex = dataItemIndex;
            this.variableUsages = variableUsages;
            this.dataDependencies = dataDependencies;
            this.warnings = warnings;
        }

        public Map<String, DataItem> getDataItemIndex() { return dataItemIndex; }
        public Map<String, List<Statement>> getVariableUsages() { return variableUsages; }
        public Map<String, Set<String>> getDataDependencies() { return dataDependencies; }
        public List<AnalysisWarning> getWarnings() { return warnings; }

        public List<AnalysisWarning> getWarningsByLevel(WarningLevel level) {
            return warnings.stream()
                .filter(w -> w.getLevel() == level)
                .collect(Collectors.toList());
        }
    }

    /**
     * Avertissement d'analyse.
     */
    public static class AnalysisWarning {
        private final WarningLevel level;
        private final String category;
        private final String message;
        private final String recommendation;

        public AnalysisWarning(WarningLevel level, String category, String message, String recommendation) {
            this.level = level;
            this.category = category;
            this.message = message;
            this.recommendation = recommendation;
        }

        public WarningLevel getLevel() { return level; }
        public String getCategory() { return category; }
        public String getMessage() { return message; }
        public String getRecommendation() { return recommendation; }

        @Override
        public String toString() {
            return String.format("[%s] %s: %s - %s", level, category, message, recommendation);
        }
    }

    /**
     * Niveau de sévérité d'un avertissement.
     */
    public enum WarningLevel {
        LOW, MEDIUM, HIGH, CRITICAL
    }
}
