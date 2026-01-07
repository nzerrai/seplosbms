package com.cobol.translator.copybook;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Resolves and loads COBOL copybooks with COPY and COPY REPLACING support.
 * Maintains a cache for performance optimization.
 */
public class CopybookResolver {
    
    private static final Logger logger = LoggerFactory.getLogger(CopybookResolver.class);
    
    // Pattern to match COPY statements
    private static final Pattern COPY_PATTERN = Pattern.compile(
        "\\bCOPY\\s+([A-Z0-9-]+)(?:\\s+REPLACING\\s+(.+?))?\\s*\\.",
        Pattern.CASE_INSENSITIVE | Pattern.DOTALL
    );
    
    // Pattern for REPLACING clauses
    private static final Pattern REPLACING_PATTERN = Pattern.compile(
        "==([^=]+)==\\s+BY\\s+==([^=]+)==",
        Pattern.CASE_INSENSITIVE
    );
    
    private final List<Path> copybookSearchPaths;
    private final Map<String, String> copybookCache;
    private final Set<String> resolvedCopybooks;
    
    public CopybookResolver() {
        this.copybookSearchPaths = new ArrayList<>();
        this.copybookCache = new ConcurrentHashMap<>();
        this.resolvedCopybooks = new HashSet<>();
    }
    
    public CopybookResolver(List<String> searchPaths) {
        this();
        for (String path : searchPaths) {
            addSearchPath(Paths.get(path));
        }
    }
    
    /**
     * Add a directory to search for copybooks
     */
    public void addSearchPath(Path path) {
        if (Files.isDirectory(path)) {
            copybookSearchPaths.add(path);
            logger.debug("Added copybook search path: {}", path);
        } else {
            logger.warn("Invalid copybook search path (not a directory): {}", path);
        }
    }
    
    /**
     * Resolve all COPY statements in COBOL source code
     * @param sourceCode Original COBOL source
     * @return COBOL source with copybooks expanded inline
     */
    public String resolveAllCopybooks(String sourceCode) {
        return resolveAllCopybooks(sourceCode, new HashSet<>());
    }
    
    /**
     * Resolve copybooks recursively with cycle detection
     */
    private String resolveAllCopybooks(String sourceCode, Set<String> processingStack) {
        StringBuilder result = new StringBuilder();
        Matcher matcher = COPY_PATTERN.matcher(sourceCode);
        int lastEnd = 0;
        
        while (matcher.find()) {
            // Append text before COPY statement
            result.append(sourceCode, lastEnd, matcher.start());
            
            String copybookName = matcher.group(1);
            String replacingClause = matcher.group(2);
            
            // Check for circular references
            if (processingStack.contains(copybookName)) {
                logger.error("Circular copybook reference detected: {}", copybookName);
                result.append("      * ERROR: Circular COPY reference: ").append(copybookName).append("\n");
                lastEnd = matcher.end();
                continue;
            }
            
            try {
                // Add comment indicating copybook expansion
                result.append("      * ===== BEGIN COPYBOOK: ").append(copybookName).append(" =====\n");
                
                // Load and expand copybook
                String copybookContent = loadCopybook(copybookName);
                
                // Apply REPLACING if present
                if (replacingClause != null && !replacingClause.trim().isEmpty()) {
                    copybookContent = applyReplacing(copybookContent, replacingClause);
                }
                
                // Recursively resolve nested copybooks
                processingStack.add(copybookName);
                copybookContent = resolveAllCopybooks(copybookContent, processingStack);
                processingStack.remove(copybookName);
                
                result.append(copybookContent);
                result.append("      * ===== END COPYBOOK: ").append(copybookName).append(" =====\n");
                
                resolvedCopybooks.add(copybookName);
                logger.info("Resolved copybook: {}", copybookName);
                
            } catch (CopybookNotFoundException e) {
                logger.error("Copybook not found: {}", copybookName);
                result.append("      * ERROR: COPYBOOK NOT FOUND: ").append(copybookName).append("\n");
            } catch (IOException e) {
                logger.error("Error reading copybook {}: {}", copybookName, e.getMessage());
                result.append("      * ERROR: Failed to read copybook: ").append(copybookName).append("\n");
            }
            
            lastEnd = matcher.end();
        }
        
        // Append remaining text
        result.append(sourceCode.substring(lastEnd));
        
        return result.toString();
    }
    
    /**
     * Load a copybook from search paths
     */
    private String loadCopybook(String copybookName) throws IOException, CopybookNotFoundException {
        // Check cache first
        if (copybookCache.containsKey(copybookName)) {
            logger.debug("Loading copybook from cache: {}", copybookName);
            return copybookCache.get(copybookName);
        }
        
        // Try different file extensions
        String[] extensions = {".cpy", ".CPY", ".cbl", ".CBL", ".cob", ".COB", ""};
        
        for (Path searchPath : copybookSearchPaths) {
            for (String ext : extensions) {
                Path copybookPath = searchPath.resolve(copybookName + ext);
                if (Files.exists(copybookPath)) {
                    logger.debug("Found copybook at: {}", copybookPath);
                    String content = Files.readString(copybookPath);
                    copybookCache.put(copybookName, content);
                    return content;
                }
            }
        }
        
        throw new CopybookNotFoundException("Copybook not found in search paths: " + copybookName);
    }
    
    /**
     * Apply COPY REPLACING transformations
     */
    private String applyReplacing(String copybookContent, String replacingClause) {
        logger.debug("Applying REPLACING clause: {}", replacingClause);
        
        String result = copybookContent;
        Matcher matcher = REPLACING_PATTERN.matcher(replacingClause);
        
        while (matcher.find()) {
            String oldValue = matcher.group(1).trim();
            String newValue = matcher.group(2).trim();
            
            logger.debug("Replacing '{}' with '{}'", oldValue, newValue);
            
            // Use word boundaries to avoid partial replacements
            result = result.replaceAll("\\b" + Pattern.quote(oldValue) + "\\b", newValue);
        }
        
        return result;
    }
    
    /**
     * Get list of all resolved copybook names
     */
    public Set<String> getResolvedCopybooks() {
        return new HashSet<>(resolvedCopybooks);
    }
    
    /**
     * Clear the copybook cache
     */
    public void clearCache() {
        copybookCache.clear();
        resolvedCopybooks.clear();
        logger.info("Copybook cache cleared");
    }
    
    /**
     * Check if a copybook exists in search paths
     */
    public boolean copybookExists(String copybookName) {
        try {
            loadCopybook(copybookName);
            return true;
        } catch (Exception e) {
            return false;
        }
    }
}
