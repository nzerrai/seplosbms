package com.cobol.translator.generator;

import com.cobol.translator.model.Paragraph;
import com.cobol.translator.model.Statement;
import com.cobol.translator.model.Statement.StatementType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for BusinessLogicTranslator - verifies COBOL to Java translation.
 */
class BusinessLogicTranslatorTest {

    private BusinessLogicTranslator translator;

    @BeforeEach
    void setUp() {
        translator = new BusinessLogicTranslator();
    }

    @Test
    void testTranslateIfStatement() {
        // COBOL: IF TR-ACCOUNT-NUMBER = ZERO
        Statement ifStmt = new Statement();
        ifStmt.setType(StatementType.IF);
        ifStmt.setCondition("TR-ACCOUNT-NUMBER = ZERO");
        ifStmt.setOriginalCobol("IF TR-ACCOUNT-NUMBER = ZERO");
        
        Paragraph paragraph = new Paragraph("TEST-IF");
        paragraph.setStatements(List.of(ifStmt));
        
        String result = translator.translateParagraph(paragraph, "TransactionRecord");
        
        assertNotNull(result);
        assertTrue(result.contains("if ("), "Should contain Java if statement");
        assertTrue(result.contains("Translated from COBOL paragraph: TEST-IF"));
    }

    @Test
    void testTranslateMoveStatement() {
        // COBOL: MOVE 'Y' TO WS-VALID-TRANSACTION
        Statement moveStmt = new Statement();
        moveStmt.setType(StatementType.MOVE);
        moveStmt.setSource("'Y'");
        moveStmt.setTarget("WS-VALID-TRANSACTION");
        moveStmt.setOriginalCobol("MOVE 'Y' TO WS-VALID-TRANSACTION");
        
        Paragraph paragraph = new Paragraph("TEST-MOVE");
        paragraph.setStatements(List.of(moveStmt));
        
        String result = translator.translateParagraph(paragraph, "TransactionRecord");
        
        assertNotNull(result);
        assertTrue(result.contains("// COBOL: MOVE"));
        assertTrue(result.contains("setWsValidTransaction"));
        assertTrue(result.contains("\"Y\""), "Should convert COBOL literal to Java String");
    }

    @Test
    void testTranslateComputeStatement() {
        // COBOL: COMPUTE NEW-BALANCE = CURRENT-BALANCE + AMOUNT
        Statement computeStmt = new Statement();
        computeStmt.setType(StatementType.COMPUTE);
        computeStmt.setTarget("NEW-BALANCE");
        computeStmt.setExpression("CURRENT-BALANCE + AMOUNT");
        computeStmt.setOriginalCobol("COMPUTE NEW-BALANCE = CURRENT-BALANCE + AMOUNT");
        
        Paragraph paragraph = new Paragraph("TEST-COMPUTE");
        paragraph.setStatements(List.of(computeStmt));
        
        String result = translator.translateParagraph(paragraph, "AccountRecord");
        
        assertNotNull(result);
        assertTrue(result.contains("// COBOL: COMPUTE"));
        assertTrue(result.contains("setNewBalance"));
    }

    @Test
    void testTranslateAddStatement() {
        // COBOL: ADD 1 TO WS-COUNTER
        Statement addStmt = new Statement();
        addStmt.setType(StatementType.ADD);
        addStmt.setSource("1");
        addStmt.setTarget("WS-COUNTER");
        addStmt.setOriginalCobol("ADD 1 TO WS-COUNTER");
        
        Paragraph paragraph = new Paragraph("TEST-ADD");
        paragraph.setStatements(List.of(addStmt));
        
        String result = translator.translateParagraph(paragraph, "CounterRecord");
        
        assertNotNull(result);
        assertTrue(result.contains("// COBOL: ADD"));
        assertTrue(result.contains("setWsCounter"));
        assertTrue(result.contains(".add("), "Should use BigDecimal.add()");
    }

    @Test
    void testTranslatePerformStatement() {
        // COBOL: PERFORM 210-VALIDATE-TRANSACTION
        Statement performStmt = new Statement();
        performStmt.setType(StatementType.PERFORM);
        performStmt.setParagraphName("210-VALIDATE-TRANSACTION");
        performStmt.setOriginalCobol("PERFORM 210-VALIDATE-TRANSACTION");
        
        Paragraph paragraph = new Paragraph("TEST-PERFORM");
        paragraph.setStatements(List.of(performStmt));
        
        String result = translator.translateParagraph(paragraph, "TransactionRecord");
        
        assertNotNull(result);
        assertTrue(result.contains("// COBOL: PERFORM"));
        // The translator generates method calls with record parameter
        assertTrue(result.contains("(record)"), 
                   "Should contain a method call with record parameter");
    }

    @Test
    void testTranslatePerformTimes() {
        // COBOL: PERFORM 100-INIT 5 TIMES
        Statement performStmt = new Statement();
        performStmt.setType(StatementType.PERFORM);
        performStmt.setParagraphName("100-INIT");
        performStmt.setPerformTimes(5);
        performStmt.setOriginalCobol("PERFORM 100-INIT 5 TIMES");
        
        Paragraph paragraph = new Paragraph("TEST-PERFORM-TIMES");
        paragraph.setStatements(List.of(performStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("for (int i = 0; i < 5; i++)"), 
                   "Should convert PERFORM n TIMES to for loop");
    }

    @Test
    void testTranslatePerformUntil() {
        // COBOL: PERFORM 110-READ UNTIL END-OF-FILE
        Statement performStmt = new Statement();
        performStmt.setType(StatementType.PERFORM);
        performStmt.setParagraphName("110-READ");
        performStmt.setUntilCondition("END-OF-FILE");
        performStmt.setOriginalCobol("PERFORM 110-READ UNTIL END-OF-FILE");
        
        Paragraph paragraph = new Paragraph("TEST-PERFORM-UNTIL");
        paragraph.setStatements(List.of(performStmt));
        
        String result = translator.translateParagraph(paragraph, "FileRecord");
        
        assertNotNull(result);
        assertTrue(result.contains("while (!("), 
                   "Should convert PERFORM UNTIL to while loop");
    }

    @Test
    void testTranslateDisplayStatement() {
        // COBOL: DISPLAY 'Processing complete'
        Statement displayStmt = new Statement();
        displayStmt.setType(StatementType.DISPLAY);
        displayStmt.setSource("'Processing complete'");
        displayStmt.setOriginalCobol("DISPLAY 'Processing complete'");
        
        Paragraph paragraph = new Paragraph("TEST-DISPLAY");
        paragraph.setStatements(List.of(displayStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("logger.info"), 
                   "Should convert DISPLAY to logger.info()");
        assertTrue(result.contains("\"Processing complete\""));
    }

    @Test
    void testTranslateEvaluateTrue() {
        // COBOL: EVALUATE TRUE
        Statement evaluateStmt = new Statement();
        evaluateStmt.setType(StatementType.EVALUATE);
        evaluateStmt.setExpression("TRUE");
        evaluateStmt.setOriginalCobol("EVALUATE TRUE");
        
        // WHEN clauses (children)
        Statement whenDebit = new Statement();
        whenDebit.setCondition("TR-DEBIT");
        evaluateStmt.getChildren().add(whenDebit);
        
        Statement whenCredit = new Statement();
        whenCredit.setCondition("TR-CREDIT");
        evaluateStmt.getChildren().add(whenCredit);
        
        Paragraph paragraph = new Paragraph("TEST-EVALUATE");
        paragraph.setStatements(List.of(evaluateStmt));
        
        String result = translator.translateParagraph(paragraph, "TransactionRecord");
        
        assertNotNull(result);
        assertTrue(result.contains("if ("), "Should generate if statement");
        assertTrue(result.contains("} else if ("), "Should generate else if for multiple WHEN");
    }

    @Test
    void testTranslateComplexParagraph() {
        // Complex paragraph with multiple statements
        List<Statement> statements = new ArrayList<>();
        
        // MOVE 'Y' TO WS-VALID
        Statement move1 = new Statement();
        move1.setType(StatementType.MOVE);
        move1.setSource("'Y'");
        move1.setTarget("WS-VALID");
        statements.add(move1);
        
        // IF ACCOUNT-NUMBER = ZERO
        Statement if1 = new Statement();
        if1.setType(StatementType.IF);
        if1.setCondition("ACCOUNT-NUMBER = ZERO");
        statements.add(if1);
        
        // ADD 1 TO COUNTER
        Statement add1 = new Statement();
        add1.setType(StatementType.ADD);
        add1.setSource("1");
        add1.setTarget("COUNTER");
        statements.add(add1);
        
        Paragraph paragraph = new Paragraph("COMPLEX-PARAGRAPH");
        paragraph.setStatements(statements);
        
        String result = translator.translateParagraph(paragraph, "ComplexRecord");
        
        assertNotNull(result);
        assertTrue(result.contains("COMPLEX-PARAGRAPH"));
        assertTrue(result.contains("setWsValid"));
        assertTrue(result.contains("if ("));
        assertTrue(result.contains("setCounter"));
        
        // Should have comments for traceability
        assertTrue(result.contains("// COBOL:"));
    }

    @Test
    void testEmptyParagraphGeneratesTodo() {
        Paragraph emptyParagraph = new Paragraph("EMPTY");
        emptyParagraph.setStatements(new ArrayList<>());
        
        String result = translator.translateParagraph(emptyParagraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("TODO"), 
                   "Empty paragraph should generate TODO comment");
    }
}
