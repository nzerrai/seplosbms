package com.cobol.translator.redefines;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Analyseur de clauses REDEFINES en COBOL
 * Détecte les redéfinitions multiples, chaînées et hiérarchiques
 */
public class RedefinesAnalyzer {
    
    private final Map<String, RedefinesInfo> redefinesMap;
    private final List<String> analysisLog;
    
    // Patterns pour la détection
    private static final Pattern FIELD_PATTERN = Pattern.compile(
        "^\\s*(\\d{2})\\s+(\\S+)\\s*(.*)$", Pattern.MULTILINE
    );
    private static final Pattern REDEFINES_PATTERN = Pattern.compile(
        "REDEFINES\\s+(\\S+)", Pattern.CASE_INSENSITIVE
    );
    private static final Pattern PIC_PATTERN = Pattern.compile(
        "PIC(?:TURE)?\\s+([\\w()]+)", Pattern.CASE_INSENSITIVE
    );
    
    public RedefinesAnalyzer() {
        this.redefinesMap = new HashMap<>();
        this.analysisLog = new ArrayList<>();
    }
    
    /**
     * Analyse le code COBOL pour détecter toutes les clauses REDEFINES
     * @param cobolCode Le code COBOL source
     * @return Map des champs redéfinis avec leurs informations
     */
    public Map<String, RedefinesInfo> analyze(String cobolCode) {
        log("Starting REDEFINES analysis...");
        
        // Première passe : collecter tous les champs
        Map<String, FieldDefinition> allFields = collectAllFields(cobolCode);
        log(String.format("Found %d total fields", allFields.size()));
        
        // Deuxième passe : identifier les redéfinitions
        for (FieldDefinition field : allFields.values()) {
            if (field.redefinesTarget != null) {
                processRedefines(field, allFields);
            }
        }
        
        log(String.format("Analysis complete. Found %d redefined fields", redefinesMap.size()));
        return new HashMap<>(redefinesMap);
    }
    
    /**
     * Collecte tous les champs définis dans le code COBOL
     */
    private Map<String, FieldDefinition> collectAllFields(String cobolCode) {
        Map<String, FieldDefinition> fields = new HashMap<>();
        String[] lines = cobolCode.split("\n");
        
        for (int i = 0; i < lines.length; i++) {
            String line = lines[i];
            
            // Ignorer les commentaires
            if (line.trim().startsWith("*")) {
                continue;
            }
            
            Matcher fieldMatcher = FIELD_PATTERN.matcher(line);
            if (fieldMatcher.find()) {
                int level = Integer.parseInt(fieldMatcher.group(1));
                String name = fieldMatcher.group(2).replaceAll("\\.$", ""); // Remove trailing period
                String rest = fieldMatcher.group(3);
                
                // Vérifier si c'est une redéfinition
                Matcher redefinesMatcher = REDEFINES_PATTERN.matcher(rest);
                String redefinesTarget = null;
                if (redefinesMatcher.find()) {
                    redefinesTarget = redefinesMatcher.group(1).replaceAll("\\.$", ""); // Remove trailing period
                }
                
                // Extraire le PIC
                Matcher picMatcher = PIC_PATTERN.matcher(rest);
                String picClause = picMatcher.find() ? picMatcher.group(1) : null;
                
                // Collecter les sous-champs pour ce champ
                List<FieldDefinition> subFields = collectSubFields(lines, i + 1, level);
                
                FieldDefinition field = new FieldDefinition(
                    level, name, picClause, redefinesTarget, subFields, i
                );
                
                fields.put(name, field);
                log(String.format("Found field: %s (level %d, pic: %s, redefines: %s)",
                    name, level, picClause, redefinesTarget));
            }
        }
        
        return fields;
    }
    
    /**
     * Collecte les sous-champs d'un champ structuré
     */
    private List<FieldDefinition> collectSubFields(String[] lines, int startIndex, int parentLevel) {
        List<FieldDefinition> subFields = new ArrayList<>();
        
        for (int i = startIndex; i < lines.length; i++) {
            String line = lines[i];
            
            if (line.trim().startsWith("*")) {
                continue;
            }
            
            Matcher fieldMatcher = FIELD_PATTERN.matcher(line);
            if (fieldMatcher.find()) {
                int level = Integer.parseInt(fieldMatcher.group(1));
                
                // Si le niveau est inférieur ou égal, on a fini avec les sous-champs
                if (level <= parentLevel) {
                    break;
                }
                
                // Si c'est un sous-champ direct (niveau = parent + 5)
                if (level == parentLevel + 5) {
                    String name = fieldMatcher.group(2).replaceAll("\\.$", ""); // Remove trailing period
                    String rest = fieldMatcher.group(3);
                    
                    Matcher picMatcher = PIC_PATTERN.matcher(rest);
                    String picClause = picMatcher.find() ? picMatcher.group(1) : null;
                    
                    FieldDefinition subField = new FieldDefinition(
                        level, name, picClause, null, new ArrayList<>(), i
                    );
                    subFields.add(subField);
                }
            }
        }
        
        return subFields;
    }
    
    /**
     * Traite une redéfinition et l'ajoute à la map
     */
    private void processRedefines(FieldDefinition field, Map<String, FieldDefinition> allFields) {
        String targetName = field.redefinesTarget;
        FieldDefinition target = allFields.get(targetName);
        
        if (target == null) {
            log(String.format("WARNING: REDEFINES target '%s' not found for field '%s'",
                targetName, field.name));
            return;
        }
        
        // Créer ou récupérer le RedefinesInfo pour le champ cible
        RedefinesInfo info = redefinesMap.computeIfAbsent(targetName, k -> {
            int size = calculateFieldSize(target);
            ViewType viewType = determineViewType(target);
            return new RedefinesInfo(targetName, target.picClause != null ? target.picClause : "STRUCTURED", size);
        });
        
        // Créer la vue pour cette redéfinition
        RedefinesView view = createView(field);
        info.addView(view);
        
        log(String.format("Added redefinition: %s REDEFINES %s", field.name, targetName));
    }
    
    /**
     * Crée une RedefinesView à partir d'une FieldDefinition
     */
    private RedefinesView createView(FieldDefinition field) {
        ViewType viewType = determineViewType(field);
        int size = calculateFieldSize(field);
        
        List<RedefinesView.SubField> subFields = null;
        if (!field.subFields.isEmpty()) {
            subFields = new ArrayList<>();
            int offset = 0;
            for (FieldDefinition subField : field.subFields) {
                int subSize = calculateFieldSize(subField);
                subFields.add(new RedefinesView.SubField(
                    subField.name,
                    subField.picClause,
                    subSize,
                    offset
                ));
                offset += subSize;
            }
        }
        
        return new RedefinesView(
            field.name,
            field.picClause != null ? field.picClause : "STRUCTURED",
            size,
            viewType,
            subFields
        );
    }
    
    /**
     * Détermine le type de vue d'un champ
     */
    private ViewType determineViewType(FieldDefinition field) {
        if (!field.subFields.isEmpty()) {
            return ViewType.STRUCTURED;
        }
        
        if (field.picClause == null) {
            return ViewType.STRUCTURED;
        }
        
        if (field.picClause.startsWith("9")) {
            return ViewType.NUMERIC;
        } else if (field.picClause.startsWith("X")) {
            return ViewType.ALPHANUMERIC;
        }
        
        return ViewType.ALPHANUMERIC;
    }
    
    /**
     * Calcule la taille en bytes d'un champ
     */
    private int calculateFieldSize(FieldDefinition field) {
        if (!field.subFields.isEmpty()) {
            // Pour un champ structuré, sommer les tailles des sous-champs
            return field.subFields.stream()
                .mapToInt(this::calculateFieldSize)
                .sum();
        }
        
        if (field.picClause == null) {
            return 0;
        }
        
        // Extraire la taille du PIC (ex: X(10) -> 10, 9(5) -> 5)
        Pattern sizePattern = Pattern.compile("\\((\\d+)\\)");
        Matcher matcher = sizePattern.matcher(field.picClause);
        
        if (matcher.find()) {
            return Integer.parseInt(matcher.group(1));
        }
        
        // Si pas de parenthèses, compter les caractères (ex: XXX -> 3, 999 -> 3)
        return field.picClause.replaceAll("[^X9]", "").length();
    }
    
    /**
     * Génère un rapport d'analyse
     */
    public String generateReport() {
        StringBuilder report = new StringBuilder();
        report.append("═══════════════════════════════════════════════\n");
        report.append("  REDEFINES ANALYSIS REPORT\n");
        report.append("═══════════════════════════════════════════════\n\n");
        
        report.append(String.format("Total redefined fields: %d\n\n", redefinesMap.size()));
        
        for (RedefinesInfo info : redefinesMap.values()) {
            report.append(info.toString()).append("\n");
        }
        
        // Ajouter le log d'analyse
        report.append("───────────────────────────────────────────────\n");
        report.append("Analysis Log:\n");
        report.append("───────────────────────────────────────────────\n");
        for (String logEntry : analysisLog) {
            report.append("  ").append(logEntry).append("\n");
        }
        
        return report.toString();
    }
    
    /**
     * Retourne la map des redéfinitions
     */
    public Map<String, RedefinesInfo> getRedefinesMap() {
        return new HashMap<>(redefinesMap);
    }
    
    /**
     * Retourne le log d'analyse
     */
    public List<String> getAnalysisLog() {
        return new ArrayList<>(analysisLog);
    }
    
    /**
     * Ajoute une entrée au log
     */
    private void log(String message) {
        analysisLog.add(message);
    }
    
    /**
     * Classe interne pour représenter une définition de champ pendant l'analyse
     */
    private static class FieldDefinition {
        final int level;
        final String name;
        final String picClause;
        final String redefinesTarget;
        final List<FieldDefinition> subFields;
        final int lineNumber;
        
        FieldDefinition(int level, String name, String picClause, String redefinesTarget,
                       List<FieldDefinition> subFields, int lineNumber) {
            this.level = level;
            this.name = name;
            this.picClause = picClause;
            this.redefinesTarget = redefinesTarget;
            this.subFields = subFields;
            this.lineNumber = lineNumber;
        }
    }
}
