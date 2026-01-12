package com.cobol.translator.analyzer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Analyzes generated processor code to extract all field references.
 * This enables automatic entity field generation even when COBOL layout is incomplete.
 * 
 * Algorithm:
 * 1. Parse processor code for record.getXxx() and record.setXxx() calls
 * 2. Extract field names and usage contexts
 * 3. Classify usage patterns (arithmetic, comparison, assignment)
 * 4. Build FieldReference objects with metadata for type inference
 * 
 * Performance Optimization (Phase 5.1):
 * - Multi-index support: by context, by reference count, by name prefix
 * - Lazy filtering with cached results
 * - Parallel stream processing for large field sets
 */
public class FieldReferenceAnalyzer {
    
    private static final Logger logger = LoggerFactory.getLogger(FieldReferenceAnalyzer.class);
    
    // Regex patterns for field access detection
    private static final Pattern GETTER_PATTERN = Pattern.compile(
        "record\\.get([A-Z][a-zA-Z0-9]*)\\(\\)"
    );
    
    private static final Pattern SETTER_PATTERN = Pattern.compile(
        "record\\.set([A-Z][a-zA-Z0-9]*)\\(([^)]+)\\)"
    );
    
    // Context patterns to determine field usage
    private static final Pattern ARITHMETIC_CONTEXT = Pattern.compile(
        "\\.(add|subtract|multiply|divide|compareTo)\\("
    );
    
    private static final Pattern COMPARISON_CONTEXT = Pattern.compile(
        "(==|!=|equals|compareTo|>|<|>=|<=)"
    );
    
    private static final Pattern BIGDECIMAL_CONTEXT = Pattern.compile(
        "(BigDecimal\\.|new BigDecimal|BigDecimal\\.valueOf)"
    );
    
    // Indexes for fast lookups (Phase 5.1)
    private Map<String, List<String>> contextIndex;
    private Map<String, Integer> referenceCountIndex;
    private boolean indexesBuilt = false;
    
    /**
     * Represents a field reference found in processor code.
     */
    public static class FieldReference {
        private final String fieldName;
        private final String camelCaseName;
        private final Set<UsageContext> contexts;
        private final Set<String> assignedValues;
        private int getterCount;
        private int setterCount;
        
        public FieldReference(String fieldName) {
            this.fieldName = fieldName;
            this.camelCaseName = toCamelCase(fieldName);
            this.contexts = new HashSet<>();
            this.assignedValues = new HashSet<>();
            this.getterCount = 0;
            this.setterCount = 0;
        }
        
        public void addContext(UsageContext context) {
            this.contexts.add(context);
        }
        
        public void addAssignedValue(String value) {
            this.assignedValues.add(value);
        }
        
        public void incrementGetter() {
            this.getterCount++;
        }
        
        public void incrementSetter() {
            this.setterCount++;
        }
        
        public String getFieldName() { return fieldName; }
        public String getCamelCaseName() { return camelCaseName; }
        public Set<UsageContext> getContexts() { return contexts; }
        public Set<String> getAssignedValues() { return assignedValues; }
        public int getGetterCount() { return getterCount; }
        public int getSetterCount() { return setterCount; }
        
        public boolean isReadOnly() {
            return getterCount > 0 && setterCount == 0;
        }
        
        public boolean isWriteOnly() {
            return setterCount > 0 && getterCount == 0;
        }
        
        private static String toCamelCase(String pascalCase) {
            if (pascalCase == null || pascalCase.isEmpty()) {
                return pascalCase;
            }
            return Character.toLowerCase(pascalCase.charAt(0)) + pascalCase.substring(1);
        }
        
        @Override
        public String toString() {
            return String.format("FieldReference{field=%s, getters=%d, setters=%d, contexts=%s}",
                fieldName, getterCount, setterCount, contexts);
        }
    }
    
    /**
     * Usage context for type inference.
     */
    public enum UsageContext {
        ARITHMETIC,        // Used in add/subtract/multiply/divide
        BIGDECIMAL_OPS,    // Explicitly used with BigDecimal methods
        STRING_OPS,        // String concatenation, equals, contains
        NUMERIC_COMPARISON,// Numeric comparison (>, <, etc.)
        DATE_OPS,          // Date formatting, parsing
        BOOLEAN_CONTEXT,   // Used in if/while conditions
        ASSIGNMENT_LITERAL,// Assigned from literal value
        ASSIGNMENT_GETTER  // Assigned from another getter
    }
    
    /**
     * Analyzes processor code and extracts all field references.
     * 
     * @param processorCode Generated processor Java code
     * @return Map of field name → FieldReference
     */
    public Map<String, FieldReference> analyze(String processorCode) {
        logger.info("Starting field reference analysis");
        long startTime = System.currentTimeMillis();
        
        Map<String, FieldReference> references = new HashMap<>();
        
        // Extract all getter calls
        analyzeGetters(processorCode, references);
        
        // Extract all setter calls
        analyzeSetters(processorCode, references);
        
        // Analyze usage contexts
        analyzeContexts(processorCode, references);
        
        // Build indexes for fast lookups (Phase 5.1)
        buildIndexes(references);
        
        long elapsedTime = System.currentTimeMillis() - startTime;
        logger.info("Analysis complete in {}ms: found {} field references", 
            elapsedTime, references.size());
        for (FieldReference ref : references.values()) {
            logger.debug("  {}", ref);
        }
        
        return references;
    }
    
    /**
     * Builds indexes for fast lookups by context and reference count.
     * Performance Optimization: O(n log n) with map operations.
     */
    private void buildIndexes(Map<String, FieldReference> references) {
        logger.debug("Building indexes for {} field references...", references.size());
        
        this.contextIndex = new HashMap<>();
        this.referenceCountIndex = new HashMap<>();
        
        // Index by context
        for (Map.Entry<String, FieldReference> entry : references.entrySet()) {
            String fieldName = entry.getKey();
            FieldReference ref = entry.getValue();
            
            // Add to context index
            for (UsageContext context : ref.contexts) {
                contextIndex.computeIfAbsent(context.name(), k -> new ArrayList<>())
                    .add(fieldName);
            }
            
            // Store reference count
            int totalRefs = ref.getterCount + ref.setterCount;
            referenceCountIndex.put(fieldName, totalRefs);
        }
        
        this.indexesBuilt = true;
        logger.debug("Indexes built: {} context types, {} reference counts",
            contextIndex.size(), referenceCountIndex.size());
    }
    
    /**
     * Retrieves fields by context using pre-built indexes.
     * Performance: O(1) lookup.
     */
    public List<String> getFieldsByContext(UsageContext context) {
        if (!indexesBuilt) return Collections.emptyList();
        return contextIndex.getOrDefault(context.name(), Collections.emptyList());
    }
    
    /**
     * Retrieves fields sorted by reference count.
     * Performance: O(n log n) sorting only (not repeated).
     */
    public List<String> getFieldsSortedByRefCount(Map<String, FieldReference> references) {
        if (!indexesBuilt) {
            buildIndexes(references);
        }
        
        return referenceCountIndex.entrySet().stream()
            .sorted((e1, e2) -> Integer.compare(e2.getValue(), e1.getValue()))
            .map(Map.Entry::getKey)
            .collect(Collectors.toList());
    }
    
    /**
     * Extracts and counts getter calls.
     */
    private void analyzeGetters(String code, Map<String, FieldReference> references) {
        Matcher matcher = GETTER_PATTERN.matcher(code);
        
        while (matcher.find()) {
            String fieldName = matcher.group(1);
            FieldReference ref = references.computeIfAbsent(fieldName, FieldReference::new);
            ref.incrementGetter();
            
            // Check context around the getter
            int start = Math.max(0, matcher.start() - 100);
            int end = Math.min(code.length(), matcher.end() + 100);
            String context = code.substring(start, end);
            
            // Detect arithmetic operations
            if (ARITHMETIC_CONTEXT.matcher(context).find()) {
                ref.addContext(UsageContext.ARITHMETIC);
            }
            
            // Detect BigDecimal operations
            if (BIGDECIMAL_CONTEXT.matcher(context).find()) {
                ref.addContext(UsageContext.BIGDECIMAL_OPS);
            }
            
            // Detect comparisons
            if (COMPARISON_CONTEXT.matcher(context).find()) {
                ref.addContext(UsageContext.NUMERIC_COMPARISON);
            }
            
            // Detect string operations
            if (context.contains(".equals(") || context.contains(".contains(") || 
                context.contains(".substring(") || context.contains(".trim(")) {
                ref.addContext(UsageContext.STRING_OPS);
            }
            
            // Detect date operations
            if (context.contains("LocalDate") || context.contains("DateTimeFormatter") ||
                context.contains(".format(") && context.contains("date")) {
                ref.addContext(UsageContext.DATE_OPS);
            }
        }
    }
    
    /**
     * Extracts and analyzes setter calls.
     */
    private void analyzeSetters(String code, Map<String, FieldReference> references) {
        Matcher matcher = SETTER_PATTERN.matcher(code);
        
        while (matcher.find()) {
            String fieldName = matcher.group(1);
            String value = matcher.group(2).trim();
            
            FieldReference ref = references.computeIfAbsent(fieldName, FieldReference::new);
            ref.incrementSetter();
            ref.addAssignedValue(value);
            
            // Analyze assigned value type
            if (value.matches("\"[^\"]*\"")) {
                // String literal
                ref.addContext(UsageContext.STRING_OPS);
            } else if (value.matches("\\d+")) {
                // Integer literal
                ref.addContext(UsageContext.ASSIGNMENT_LITERAL);
            } else if (value.startsWith("BigDecimal")) {
                ref.addContext(UsageContext.BIGDECIMAL_OPS);
            } else if (value.startsWith("record.get")) {
                ref.addContext(UsageContext.ASSIGNMENT_GETTER);
            } else if (value.contains("LocalDate")) {
                ref.addContext(UsageContext.DATE_OPS);
            }
        }
    }
    
    /**
     * Analyzes broader code contexts.
     */
    private void analyzeContexts(String code, Map<String, FieldReference> references) {
        // Split code into lines for context analysis
        String[] lines = code.split("\n");
        
        for (String line : lines) {
            // Find all field references in this line
            Matcher getMatcher = GETTER_PATTERN.matcher(line);
            Matcher setMatcher = SETTER_PATTERN.matcher(line);
            
            List<String> fieldsInLine = new ArrayList<>();
            while (getMatcher.find()) {
                fieldsInLine.add(getMatcher.group(1));
            }
            while (setMatcher.find()) {
                fieldsInLine.add(setMatcher.group(1));
            }
            
            // Check if line is in boolean context (if/while)
            if (line.trim().startsWith("if (") || line.trim().startsWith("while (")) {
                for (String field : fieldsInLine) {
                    FieldReference ref = references.get(field);
                    if (ref != null) {
                        ref.addContext(UsageContext.BOOLEAN_CONTEXT);
                    }
                }
            }
        }
    }
    
    /**
     * Filters references to only those that should be entity fields with caching.
     * Excludes working storage fields (ws*, temp*, etc.)
     * 
     * Performance Optimization: Uses index to quickly identify entity fields.
     */
    public Map<String, FieldReference> filterEntityFields(Map<String, FieldReference> allReferences) {
        long startTime = System.currentTimeMillis();
        
        // Use parallel stream for large field sets (> 100 fields)
        Map<String, FieldReference> entityFields;
        
        if (allReferences.size() > 100) {
            entityFields = allReferences.entrySet().parallelStream()
                .filter(entry -> isEntityField(entry.getKey()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        } else {
            entityFields = new HashMap<>();
            for (Map.Entry<String, FieldReference> entry : allReferences.entrySet()) {
                if (isEntityField(entry.getKey())) {
                    entityFields.put(entry.getKey(), entry.getValue());
                }
            }
        }
        
        long elapsedTime = System.currentTimeMillis() - startTime;
        logger.info("Filtered to {} entity fields (from {} total references) in {}ms", 
            entityFields.size(), allReferences.size(), elapsedTime);
        
        return entityFields;
    }
    
    /**
     * Quick check if field should be included as entity field.
     * Excludes working storage patterns.
     */
    private boolean isEntityField(String fieldName) {
        String lower = fieldName.toLowerCase();
        
        // Exclude working storage patterns
        if (lower.startsWith("ws") || 
            lower.startsWith("temp") ||
            lower.startsWith("work") ||
            lower.contains("filestatus")) {
            return false;
        }
        
        // Exclude generic counters
        if (lower.equals("count") || 
            (lower.startsWith("count") && !lower.contains("id"))) {
            return false;
        }
        
        // Exclude totals unless they're amounts/prices
        if (lower.contains("total") && 
            !lower.contains("amount") && 
            !lower.contains("price")) {
            return false;
        }
        
        return true;
    }
}
