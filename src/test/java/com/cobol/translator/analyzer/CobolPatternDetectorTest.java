package com.cobol.translator.analyzer;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.FileDefinition;
import com.cobol.translator.model.Paragraph;
import com.cobol.translator.model.Statement;
import com.cobol.translator.model.Statement.StatementType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests pour CobolPatternDetector.
 */
class CobolPatternDetectorTest {

    private CobolPatternDetector detector;
    private CobolProgram program;

    @BeforeEach
    void setUp() {
        detector = new CobolPatternDetector();
        program = new CobolProgram("TEST-PROGRAM");
    }

    @Test
    @DisplayName("Detect standard file processing pattern: OPEN-READ-PERFORM-CLOSE")
    void testDetectStandardFileProcessingPattern() {
        // Arrange: Create standard COBOL file processing structure
        List<Statement> statements = new ArrayList<>();
        
        // OPEN INPUT CUSTOMER-FILE
        Statement openStmt = new Statement();
        openStmt.setType(StatementType.OPEN);
        openStmt.setOriginalCobol("OPEN INPUT CUSTOMER-FILE");
        statements.add(openStmt);
        
        // PERFORM UNTIL WS-EOF = 'Y'
        Statement performStmt = new Statement();
        performStmt.setType(StatementType.PERFORM_UNTIL);
        performStmt.setUntilCondition("WS-EOF = 'Y'");
        performStmt.setOriginalCobol("PERFORM UNTIL WS-EOF = 'Y'");
        statements.add(performStmt);
        
        // READ CUSTOMER-FILE AT END MOVE 'Y' TO WS-EOF
        Statement readStmt = new Statement();
        readStmt.setType(StatementType.READ);
        readStmt.setOriginalCobol("READ CUSTOMER-FILE AT END MOVE 'Y' TO WS-EOF NOT AT END PERFORM 1000-PROCESS-RECORD END-READ");
        statements.add(readStmt);
        
        // CLOSE CUSTOMER-FILE
        Statement closeStmt = new Statement();
        closeStmt.setType(StatementType.CLOSE);
        closeStmt.setOriginalCobol("CLOSE CUSTOMER-FILE");
        statements.add(closeStmt);
        
        // DISPLAY 'PROCESSED: ' WS-COUNT
        Statement displayStmt = new Statement();
        displayStmt.setType(StatementType.DISPLAY);
        displayStmt.setOriginalCobol("DISPLAY 'PROCESSED: ' WS-COUNT");
        statements.add(displayStmt);
        
        program.setStatements(statements);
        
        // Act
        Map<String, Object> patterns = detector.detectPatterns(program);
        
        // Assert
        assertTrue(patterns.containsKey("FILE_PROCESSING"), 
                  "Should detect file processing pattern");
        
        CobolPatternDetector.FileProcessingPattern filePattern = 
            (CobolPatternDetector.FileProcessingPattern) patterns.get("FILE_PROCESSING");
        
        assertTrue(filePattern.isDetected(), "Pattern should be detected");
        assertEquals(100, filePattern.getScore(), "Should have perfect score");
        assertEquals("WS-EOF", filePattern.getEofVariable(), "Should detect EOF variable");
        assertEquals("WS-COUNT", filePattern.getCounterVariable(), "Should detect counter variable");
        
        // Verify idiomatic score
        assertTrue(patterns.containsKey("IDIOMATIC_SCORE"));
        int score = (Integer) patterns.get("IDIOMATIC_SCORE");
        assertEquals(100, score, "Should have perfect idiomatic score");
    }

    @Test
    @DisplayName("Detect batch structure pattern: INIT-PROCESS-FINALIZE")
    void testDetectBatchStructurePattern() {
        // Arrange: Create standard batch structure
        List<Paragraph> paragraphs = new ArrayList<>();
        
        // Main control paragraph
        Paragraph mainPara = new Paragraph("000-MAIN-CONTROL");
        paragraphs.add(mainPara);
        
        // Initialize paragraph
        Paragraph initPara = new Paragraph("100-INITIALIZE");
        paragraphs.add(initPara);
        
        // Process paragraph
        Paragraph processPara = new Paragraph("200-PROCESS-RECORDS");
        paragraphs.add(processPara);
        
        // Finalize paragraph
        Paragraph finalizePara = new Paragraph("300-FINALIZE");
        paragraphs.add(finalizePara);
        
        program.setParagraphs(paragraphs);
        
        // Act
        Map<String, Object> patterns = detector.detectPatterns(program);
        
        // Assert
        assertTrue(patterns.containsKey("BATCH_STRUCTURE"), 
                  "Should detect batch structure pattern");
        
        CobolPatternDetector.BatchStructurePattern batchPattern = 
            (CobolPatternDetector.BatchStructurePattern) patterns.get("BATCH_STRUCTURE");
        
        assertTrue(batchPattern.isDetected(), "Batch pattern should be detected");
        assertEquals(100, batchPattern.getScore(), "Should have perfect score with finalize");
        assertNotNull(batchPattern.getMainParagraph());
        assertNotNull(batchPattern.getInitParagraph());
        assertNotNull(batchPattern.getProcessParagraph());
        assertNotNull(batchPattern.getFinalizeParagraph());
    }

    @Test
    @DisplayName("Detect batch structure without finalize paragraph")
    void testDetectBatchStructureWithoutFinalize() {
        // Arrange
        List<Paragraph> paragraphs = new ArrayList<>();
        paragraphs.add(new Paragraph("MAIN-CONTROL"));
        paragraphs.add(new Paragraph("INIT-PROCESS"));
        paragraphs.add(new Paragraph("PROCESS-TRANSACTIONS"));
        
        program.setParagraphs(paragraphs);
        
        // Act
        Map<String, Object> patterns = detector.detectPatterns(program);
        
        // Assert
        assertTrue(patterns.containsKey("BATCH_STRUCTURE"));
        CobolPatternDetector.BatchStructurePattern batchPattern = 
            (CobolPatternDetector.BatchStructurePattern) patterns.get("BATCH_STRUCTURE");
        
        assertTrue(batchPattern.isDetected());
        assertEquals(80, batchPattern.getScore(), "Should have 80 score without finalize");
    }

    @Test
    @DisplayName("Detect table search pattern")
    void testDetectTableSearchPattern() {
        // Arrange
        List<Statement> statements = new ArrayList<>();
        
        Statement searchStmt = new Statement();
        searchStmt.setType(StatementType.SEARCH);
        searchStmt.setOriginalCobol("SEARCH CUSTOMER-TABLE");
        statements.add(searchStmt);
        
        program.setStatements(statements);
        
        // Act
        Map<String, Object> patterns = detector.detectPatterns(program);
        
        // Assert
        assertTrue(patterns.containsKey("TABLE_SEARCH"), "Should detect table search");
        assertTrue((Boolean) patterns.get("TABLE_SEARCH"));
        
        int score = (Integer) patterns.get("IDIOMATIC_SCORE");
        assertEquals(20, score, "Should have score for table search");
    }

    @Test
    @DisplayName("Detect combined patterns with high idiomatic score")
    void testDetectCombinedPatterns() {
        // Arrange: Create both file processing and batch structure
        
        // File processing statements
        List<Statement> statements = new ArrayList<>();
        Statement openStmt = new Statement();
        openStmt.setType(StatementType.OPEN);
        openStmt.setOriginalCobol("OPEN INPUT FILE");
        statements.add(openStmt);
        
        Statement performStmt = new Statement();
        performStmt.setType(StatementType.PERFORM_UNTIL);
        performStmt.setUntilCondition("WS-EOF = 'Y'");
        statements.add(performStmt);
        
        Statement readStmt = new Statement();
        readStmt.setType(StatementType.READ);
        readStmt.setOriginalCobol("READ FILE AT END MOVE 'Y' TO WS-EOF");
        statements.add(readStmt);
        
        Statement closeStmt = new Statement();
        closeStmt.setType(StatementType.CLOSE);
        statements.add(closeStmt);
        
        program.setStatements(statements);
        
        // Batch structure paragraphs
        List<Paragraph> paragraphs = new ArrayList<>();
        paragraphs.add(new Paragraph("MAIN"));
        paragraphs.add(new Paragraph("INITIALIZE"));
        paragraphs.add(new Paragraph("PROCESS"));
        paragraphs.add(new Paragraph("FINALIZE"));
        program.setParagraphs(paragraphs);
        
        // Act
        Map<String, Object> patterns = detector.detectPatterns(program);
        
        // Assert
        assertTrue(patterns.containsKey("FILE_PROCESSING"));
        assertTrue(patterns.containsKey("BATCH_STRUCTURE"));
        
        int score = (Integer) patterns.get("IDIOMATIC_SCORE");
        assertTrue(score >= 100, "Should have very high score with both patterns: " + score);
    }

    @Test
    @DisplayName("Extract EOF variable from AT END clause")
    void testExtractEofVariable() {
        // Arrange
        List<Statement> statements = new ArrayList<>();
        
        Statement openStmt = new Statement();
        openStmt.setType(StatementType.OPEN);
        statements.add(openStmt);
        
        Statement performStmt = new Statement();
        performStmt.setType(StatementType.PERFORM_UNTIL);
        performStmt.setUntilCondition("WS-END-OF-FILE = 'Y'");
        statements.add(performStmt);
        
        Statement readStmt = new Statement();
        readStmt.setType(StatementType.READ);
        readStmt.setOriginalCobol("READ FILE AT END MOVE 'Y' TO WS-END-OF-FILE");
        statements.add(readStmt);
        
        Statement closeStmt = new Statement();
        closeStmt.setType(StatementType.CLOSE);
        statements.add(closeStmt);
        
        program.setStatements(statements);
        
        // Act
        Map<String, Object> patterns = detector.detectPatterns(program);
        
        // Assert
        CobolPatternDetector.FileProcessingPattern filePattern = 
            (CobolPatternDetector.FileProcessingPattern) patterns.get("FILE_PROCESSING");
        
        assertEquals("WS-END-OF-FILE", filePattern.getEofVariable());
    }

    @Test
    @DisplayName("No patterns detected for non-standard code")
    void testNoPatternDetection() {
        // Arrange: Create non-standard COBOL
        List<Statement> statements = new ArrayList<>();
        
        Statement moveStmt = new Statement();
        moveStmt.setType(StatementType.MOVE);
        statements.add(moveStmt);
        
        program.setStatements(statements);
        
        // Act
        Map<String, Object> patterns = detector.detectPatterns(program);
        
        // Assert
        assertFalse(patterns.containsKey("FILE_PROCESSING"));
        assertFalse(patterns.containsKey("BATCH_STRUCTURE"));
        
        int score = (Integer) patterns.get("IDIOMATIC_SCORE");
        assertEquals(0, score, "Should have 0 score with no patterns");
    }

    @Test
    @DisplayName("Detect file processing in paragraphs, not just top-level")
    void testDetectFileProcessingInParagraphs() {
        // Arrange: File processing statements in paragraph
        Paragraph mainPara = new Paragraph("MAIN");
        
        List<Statement> paraStatements = new ArrayList<>();
        
        Statement openStmt = new Statement();
        openStmt.setType(StatementType.OPEN);
        paraStatements.add(openStmt);
        
        Statement performStmt = new Statement();
        performStmt.setType(StatementType.PERFORM_UNTIL);
        performStmt.setUntilCondition("EOF-FLAG = 'Y'");
        paraStatements.add(performStmt);
        
        Statement readStmt = new Statement();
        readStmt.setType(StatementType.READ);
        readStmt.setOriginalCobol("READ FILE AT END MOVE 'Y' TO EOF-FLAG");
        paraStatements.add(readStmt);
        
        Statement closeStmt = new Statement();
        closeStmt.setType(StatementType.CLOSE);
        paraStatements.add(closeStmt);
        
        mainPara.setStatements(paraStatements);
        
        List<Paragraph> paragraphs = new ArrayList<>();
        paragraphs.add(mainPara);
        program.setParagraphs(paragraphs);
        
        // Act
        Map<String, Object> patterns = detector.detectPatterns(program);
        
        // Assert
        assertTrue(patterns.containsKey("FILE_PROCESSING"), 
                  "Should detect file processing even when in paragraphs");
        
        CobolPatternDetector.FileProcessingPattern filePattern = 
            (CobolPatternDetector.FileProcessingPattern) patterns.get("FILE_PROCESSING");
        assertTrue(filePattern.isDetected());
    }

    @Test
    @DisplayName("Incomplete file processing pattern not detected")
    void testIncompleteFileProcessingPattern() {
        // Arrange: Missing CLOSE statement
        List<Statement> statements = new ArrayList<>();
        
        Statement openStmt = new Statement();
        openStmt.setType(StatementType.OPEN);
        statements.add(openStmt);
        
        Statement readStmt = new Statement();
        readStmt.setType(StatementType.READ);
        readStmt.setOriginalCobol("READ FILE AT END MOVE 'Y' TO WS-EOF");
        statements.add(readStmt);
        
        // Missing PERFORM UNTIL and CLOSE
        
        program.setStatements(statements);
        
        // Act
        Map<String, Object> patterns = detector.detectPatterns(program);
        
        // Assert
        if (patterns.containsKey("FILE_PROCESSING")) {
            CobolPatternDetector.FileProcessingPattern filePattern = 
                (CobolPatternDetector.FileProcessingPattern) patterns.get("FILE_PROCESSING");
            assertFalse(filePattern.isDetected(), 
                       "Incomplete pattern should not be detected");
        }
    }
}
