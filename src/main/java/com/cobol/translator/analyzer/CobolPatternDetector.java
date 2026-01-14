package com.cobol.translator.analyzer;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.FileDefinition;
import com.cobol.translator.model.Paragraph;
import com.cobol.translator.model.Statement;
import com.cobol.translator.model.Statement.StatementType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Détecteur de patterns COBOL idiomatiques.
 * 
 * Reconnaît les structures COBOL standard et marque les programmes
 * pour éviter les faux warnings et optimiser la génération de code.
 * 
 * Patterns détectés:
 * - OPEN-READ-PERFORM-CLOSE (lecture séquentielle standard)
 * - PERFORM UNTIL avec AT END (boucle de traitement)
 * - INITIALIZE-PROCESS-FINALIZE (structure batch classique)
 * - File processing avec compteurs
 * - SEARCH/SEARCH ALL (recherche table)
 */
public class CobolPatternDetector {
    
    private static final Logger logger = LoggerFactory.getLogger(CobolPatternDetector.class);
    
    // Patterns regex
    private static final Pattern EOF_VARIABLE_PATTERN = Pattern.compile("WS-(EOF|END-OF-FILE|END-OF-.*)", Pattern.CASE_INSENSITIVE);
    private static final Pattern COUNTER_PATTERN = Pattern.compile("WS-(COUNT|COUNTER|.*-COUNT|.*-COUNTER)", Pattern.CASE_INSENSITIVE);
    
    /**
     * Détecte tous les patterns dans le programme COBOL.
     */
    public Map<String, Object> detectPatterns(CobolProgram program) {
        Map<String, Object> detectedPatterns = new HashMap<>();
        
        // Pattern 1: OPEN-READ-PERFORM-CLOSE (Lecture séquentielle standard)
        FileProcessingPattern filePattern = detectFileProcessingPattern(program);
        if (filePattern.isDetected()) {
            detectedPatterns.put("FILE_PROCESSING", filePattern);
            program.setPattern("FILE_PROCESSING");
            logger.info("✅ Detected standard file processing pattern - this is idiomatic COBOL");
        }
        
        // Pattern 2: INITIALIZE-PROCESS-FINALIZE (Structure batch)
        BatchStructurePattern batchPattern = detectBatchStructurePattern(program);
        if (batchPattern.isDetected()) {
            detectedPatterns.put("BATCH_STRUCTURE", batchPattern);
            logger.info("✅ Detected standard batch structure pattern - this is idiomatic COBOL");
        }
        
        // Pattern 3: Table search patterns
        if (detectTableSearchPattern(program)) {
            detectedPatterns.put("TABLE_SEARCH", true);
            logger.info("✅ Detected table search pattern");
        }
        
        // Calculer score basé sur patterns détectés
        int idiomaticScore = calculateIdiomaticScore(detectedPatterns);
        detectedPatterns.put("IDIOMATIC_SCORE", idiomaticScore);
        
        return detectedPatterns;
    }
    
    /**
     * Détecte le pattern OPEN-READ-PERFORM-CLOSE.
     * 
     * Pattern attendu:
     * OPEN INPUT fichier
     * PERFORM UNTIL eof-flag = 'Y'
     *   READ fichier
     *     AT END MOVE 'Y' TO eof-flag
     *     NOT AT END PERFORM traitement
     *   END-READ
     * END-PERFORM
     * CLOSE fichier
     */
    private FileProcessingPattern detectFileProcessingPattern(CobolProgram program) {
        FileProcessingPattern pattern = new FileProcessingPattern();
        
        List<Statement> statements = program.getStatements();
        boolean hasOpen = false;
        boolean hasRead = false;
        boolean hasPerformUntil = false;
        boolean hasAtEnd = false;
        boolean hasClose = false;
        boolean hasDisplay = false;
        String eofVariable = null;
        String processedCounterVariable = null;
        
        // Chercher dans les paragraphes aussi
        List<Statement> allStatements = new ArrayList<>(statements);
        if (program.getParagraphs() != null) {
            for (Paragraph para : program.getParagraphs()) {
                if (para.getStatements() != null) {
                    allStatements.addAll(para.getStatements());
                }
            }
        }
        
        // Analyser les statements
        for (Statement stmt : allStatements) {
            if (stmt.getType() == StatementType.OPEN) {
                hasOpen = true;
                pattern.setOpenStatement(stmt);
            } else if (stmt.getType() == StatementType.READ) {
                hasRead = true;
                pattern.setReadStatement(stmt);
                
                // Détecter AT END dans READ
                String originalCobol = stmt.getOriginalCobol();
                if (originalCobol != null && originalCobol.toUpperCase().contains("AT END")) {
                    hasAtEnd = true;
                    // Extraire variable EOF
                    eofVariable = extractEofVariable(originalCobol);
                }
            } else if (stmt.getType() == StatementType.PERFORM_UNTIL) {
                hasPerformUntil = true;
                pattern.setPerformUntilStatement(stmt);
                
                // Vérifier que la condition UNTIL utilise une variable EOF
                String condition = stmt.getUntilCondition();
                if (condition != null) {
                    Matcher matcher = EOF_VARIABLE_PATTERN.matcher(condition);
                    if (matcher.find()) {
                        eofVariable = matcher.group();
                        pattern.setEofVariable(eofVariable);
                    }
                }
            } else if (stmt.getType() == StatementType.CLOSE) {
                hasClose = true;
                pattern.setCloseStatement(stmt);
            } else if (stmt.getType() == StatementType.DISPLAY) {
                hasDisplay = true;
                // Chercher variables de compteur
                String displayText = stmt.getOriginalCobol();
                if (displayText != null) {
                    Matcher matcher = COUNTER_PATTERN.matcher(displayText);
                    if (matcher.find()) {
                        processedCounterVariable = matcher.group();
                        pattern.setCounterVariable(processedCounterVariable);
                    }
                }
            }
        }
        
        // Le pattern est détecté si on a les éléments clés
        boolean detected = hasOpen && hasRead && hasPerformUntil && hasAtEnd && hasClose;
        pattern.setDetected(detected);
        
        if (detected) {
            pattern.setScore(100); // Pattern parfait
            logger.info("✅ Perfect file processing pattern detected:");
            logger.info("   - OPEN INPUT: {}", pattern.getOpenStatement() != null);
            logger.info("   - PERFORM UNTIL {}: true", eofVariable);
            logger.info("   - READ with AT END: true");
            logger.info("   - CLOSE: true");
            if (hasDisplay) {
                logger.info("   - DISPLAY counter {}: true", processedCounterVariable);
            }
        }
        
        return pattern;
    }
    
    /**
     * Extrait la variable EOF d'une clause AT END.
     */
    private String extractEofVariable(String cobolText) {
        // Pattern: AT END MOVE 'Y' TO WS-EOF
        Pattern pattern = Pattern.compile("AT\\s+END.*MOVE\\s+['\"]Y['\"]\\s+TO\\s+(\\S+)", 
                                         Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(cobolText);
        if (matcher.find()) {
            return matcher.group(1);
        }
        return null;
    }
    
    /**
     * Détecte la structure batch standard.
     * 
     * Pattern:
     * MAIN-CONTROL.
     *   PERFORM INITIALIZE
     *   PERFORM PROCESS-xxx UNTIL condition
     *   PERFORM FINALIZE
     *   STOP RUN.
     */
    private BatchStructurePattern detectBatchStructurePattern(CobolProgram program) {
        BatchStructurePattern pattern = new BatchStructurePattern();
        
        boolean hasInitParagraph = false;
        boolean hasProcessParagraph = false;
        boolean hasFinalizeParagraph = false;
        boolean hasMainControl = false;
        
        if (program.getParagraphs() != null) {
            for (Paragraph para : program.getParagraphs()) {
                String name = para.getName().toUpperCase();
                
                if (name.contains("MAIN") || name.contains("CONTROL") || name.startsWith("000-")) {
                    hasMainControl = true;
                    pattern.setMainParagraph(para);
                }
                
                if (name.contains("INIT") || name.startsWith("100-")) {
                    hasInitParagraph = true;
                    pattern.setInitParagraph(para);
                }
                
                if (name.contains("PROCESS") || name.contains("READ") || 
                    (name.startsWith("200-") || name.startsWith("0200-"))) {
                    hasProcessParagraph = true;
                    pattern.setProcessParagraph(para);
                }
                
                if (name.contains("FINAL") || name.contains("TERMINATE") || 
                    (name.startsWith("300-") || name.startsWith("0300-"))) {
                    hasFinalizeParagraph = true;
                    pattern.setFinalizeParagraph(para);
                }
            }
        }
        
        boolean detected = hasMainControl && hasInitParagraph && hasProcessParagraph;
        pattern.setDetected(detected);
        
        if (detected) {
            int score = 80;
            if (hasFinalizeParagraph) score = 100;
            pattern.setScore(score);
            
            logger.info("✅ Standard batch structure detected:");
            logger.info("   - Main control paragraph: {}", hasMainControl);
            logger.info("   - Initialize paragraph: {}", hasInitParagraph);
            logger.info("   - Process paragraph: {}", hasProcessParagraph);
            logger.info("   - Finalize paragraph: {}", hasFinalizeParagraph);
        }
        
        return pattern;
    }
    
    /**
     * Détecte les patterns de recherche de table.
     */
    private boolean detectTableSearchPattern(CobolProgram program) {
        for (Statement stmt : program.getStatements()) {
            if (stmt.getType() == StatementType.SEARCH) {
                return true;
            }
        }
        
        // Chercher dans les paragraphes
        if (program.getParagraphs() != null) {
            for (Paragraph para : program.getParagraphs()) {
                if (para.getStatements() != null) {
                    for (Statement stmt : para.getStatements()) {
                        if (stmt.getType() == StatementType.SEARCH) {
                            return true;
                        }
                    }
                }
            }
        }
        
        return false;
    }
    
    /**
     * Calcule le score idiomatique du programme COBOL.
     * Un score élevé indique un code COBOL bien structuré et standard.
     */
    private int calculateIdiomaticScore(Map<String, Object> patterns) {
        int score = 0;
        
        if (patterns.containsKey("FILE_PROCESSING")) {
            FileProcessingPattern fp = (FileProcessingPattern) patterns.get("FILE_PROCESSING");
            score += fp.getScore();
        }
        
        if (patterns.containsKey("BATCH_STRUCTURE")) {
            BatchStructurePattern bp = (BatchStructurePattern) patterns.get("BATCH_STRUCTURE");
            score += bp.getScore() / 2; // Demi-poids
        }
        
        if (patterns.containsKey("TABLE_SEARCH")) {
            score += 20;
        }
        
        return Math.min(100, score);
    }
    
    /**
     * Pattern de traitement de fichier.
     */
    public static class FileProcessingPattern {
        private boolean detected = false;
        private int score = 0;
        private Statement openStatement;
        private Statement readStatement;
        private Statement performUntilStatement;
        private Statement closeStatement;
        private String eofVariable;
        private String counterVariable;
        
        // Getters and setters
        public boolean isDetected() { return detected; }
        public void setDetected(boolean detected) { this.detected = detected; }
        public int getScore() { return score; }
        public void setScore(int score) { this.score = score; }
        public Statement getOpenStatement() { return openStatement; }
        public void setOpenStatement(Statement openStatement) { this.openStatement = openStatement; }
        public Statement getReadStatement() { return readStatement; }
        public void setReadStatement(Statement readStatement) { this.readStatement = readStatement; }
        public Statement getPerformUntilStatement() { return performUntilStatement; }
        public void setPerformUntilStatement(Statement performUntilStatement) { this.performUntilStatement = performUntilStatement; }
        public Statement getCloseStatement() { return closeStatement; }
        public void setCloseStatement(Statement closeStatement) { this.closeStatement = closeStatement; }
        public String getEofVariable() { return eofVariable; }
        public void setEofVariable(String eofVariable) { this.eofVariable = eofVariable; }
        public String getCounterVariable() { return counterVariable; }
        public void setCounterVariable(String counterVariable) { this.counterVariable = counterVariable; }
    }
    
    /**
     * Pattern de structure batch.
     */
    public static class BatchStructurePattern {
        private boolean detected = false;
        private int score = 0;
        private Paragraph mainParagraph;
        private Paragraph initParagraph;
        private Paragraph processParagraph;
        private Paragraph finalizeParagraph;
        
        // Getters and setters
        public boolean isDetected() { return detected; }
        public void setDetected(boolean detected) { this.detected = detected; }
        public int getScore() { return score; }
        public void setScore(int score) { this.score = score; }
        public Paragraph getMainParagraph() { return mainParagraph; }
        public void setMainParagraph(Paragraph mainParagraph) { this.mainParagraph = mainParagraph; }
        public Paragraph getInitParagraph() { return initParagraph; }
        public void setInitParagraph(Paragraph initParagraph) { this.initParagraph = initParagraph; }
        public Paragraph getProcessParagraph() { return processParagraph; }
        public void setProcessParagraph(Paragraph processParagraph) { this.processParagraph = processParagraph; }
        public Paragraph getFinalizeParagraph() { return finalizeParagraph; }
        public void setFinalizeParagraph(Paragraph finalizeParagraph) { this.finalizeParagraph = finalizeParagraph; }
    }
}
