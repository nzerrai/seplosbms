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

    // ==================== Phase 3 Improvements Tests ====================
    
    @Test
    void testNullSourceHandling() {
        // MOVE with null source should generate TODO
        Statement moveStmt = new Statement();
        moveStmt.setType(StatementType.MOVE);
        moveStmt.setSource(null);
        moveStmt.setTarget("WS-FIELD");
        
        Paragraph paragraph = new Paragraph("NULL-TEST");
        paragraph.setStatements(List.of(moveStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("TODO") || result.contains("null"));
    }

    @Test
    void testEmptyStringHandling() {
        // Statement with empty string should be handled gracefully
        Statement moveStmt = new Statement();
        moveStmt.setType(StatementType.MOVE);
        moveStmt.setSource("");
        moveStmt.setTarget("");
        
        Paragraph paragraph = new Paragraph("EMPTY-STRING-TEST");
        paragraph.setStatements(List.of(moveStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        // Should not throw exception
    }

    @Test
    void testComplexConditionTranslation() {
        // Test condition with AND/OR/NOT operators
        Statement ifStmt = new Statement();
        ifStmt.setType(StatementType.IF);
        ifStmt.setCondition("ACCOUNT = ZERO AND STATUS = 'ACTIVE' OR FLAG = 'Y'");
        
        Paragraph paragraph = new Paragraph("COMPLEX-CONDITION");
        paragraph.setStatements(List.of(ifStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("&&") || result.contains("if ("));
    }

    @Test
    void testFieldNameConversion() {
        // Test COBOL field name to Java conversion
        Statement moveStmt = new Statement();
        moveStmt.setType(StatementType.MOVE);
        moveStmt.setSource("'TEST'");
        moveStmt.setTarget("TR-ACCOUNT-NUMBER");
        
        Paragraph paragraph = new Paragraph("FIELD-NAME-TEST");
        paragraph.setStatements(List.of(moveStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("setTrAccountNumber") || result.contains("settraccountnumber"));
    }

    @Test
    void testNumericLiteralHandling() {
        // Test numeric literal conversion
        Statement computeStmt = new Statement();
        computeStmt.setType(StatementType.COMPUTE);
        computeStmt.setTarget("WS-RESULT");
        computeStmt.setExpression("100 + 50");
        
        Paragraph paragraph = new Paragraph("NUMERIC-LITERAL");
        paragraph.setStatements(List.of(computeStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("setWsResult") || result.contains("100"));
    }

    @Test
    void testSpecialCobolValues() {
        // Test ZERO, SPACES, etc.
        Statement ifStmt = new Statement();
        ifStmt.setType(StatementType.IF);
        ifStmt.setCondition("WS-COUNTER = ZERO");
        
        Paragraph paragraph = new Paragraph("SPECIAL-VALUES");
        paragraph.setStatements(List.of(ifStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("0") || result.contains("ZERO"));
    }

    @Test
    void testPerformTimesWithZero() {
        // PERFORM 0 TIMES should be handled
        Statement performStmt = new Statement();
        performStmt.setType(StatementType.PERFORM);
        performStmt.setParagraphName("100-INIT");
        performStmt.setPerformTimes(0);
        
        Paragraph paragraph = new Paragraph("PERFORM-ZERO");
        paragraph.setStatements(List.of(performStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        // Should generate TODO or skip
    }

    @Test
    void testNestedIfStatements() {
        // Test nested IF structures
        Statement childIf = new Statement();
        childIf.setType(StatementType.IF);
        childIf.setCondition("INNER-CONDITION");
        
        Statement parentIf = new Statement();
        parentIf.setType(StatementType.IF);
        parentIf.setCondition("OUTER-CONDITION");
        parentIf.setChildren(List.of(childIf));
        
        Paragraph paragraph = new Paragraph("NESTED-IF");
        paragraph.setStatements(List.of(parentIf));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        // Should have nested if structure
        assertTrue(result.contains("if ("));
    }

    @Test
    void testEvaluateTrueWithoutWhenClauses() {
        // EVALUATE TRUE without WHEN clauses
        Statement evalStmt = new Statement();
        evalStmt.setType(StatementType.EVALUATE);
        evalStmt.setExpression("TRUE");
        evalStmt.setChildren(new ArrayList<>()); // Empty children
        
        Paragraph paragraph = new Paragraph("EVAL-EMPTY");
        paragraph.setStatements(List.of(evalStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        // Should handle gracefully
    }

    @Test
    void testArithmeticOperationValidation() {
        // ADD without target should generate TODO
        Statement addStmt = new Statement();
        addStmt.setType(StatementType.ADD);
        addStmt.setSource("100");
        addStmt.setTarget(null);
        
        Paragraph paragraph = new Paragraph("ADD-INVALID");
        paragraph.setStatements(List.of(addStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("TODO") || result.contains("Invalid"));
    }

    @Test
    void testDivideWithRoundingMode() {
        // DIVIDE should use HALF_UP rounding
        Statement divideStmt = new Statement();
        divideStmt.setType(StatementType.DIVIDE);
        divideStmt.setSource("3");
        divideStmt.setTarget("WS-RESULT");
        
        Paragraph paragraph = new Paragraph("DIVIDE-TEST");
        paragraph.setStatements(List.of(divideStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("RoundingMode") || result.contains("divide"));
    }

    // ==================== Phase 4: Advanced Statements Tests ====================

    @Test
    void testTranslateGoToStatement() {
        // COBOL: GO TO 100-PROCESS
        Statement goToStmt = new Statement();
        goToStmt.setType(StatementType.GO_TO);
        goToStmt.setParagraphName("100-PROCESS");
        goToStmt.setOriginalCobol("GO TO 100-PROCESS");
        
        Paragraph paragraph = new Paragraph("TEST-GOTO");
        paragraph.setStatements(List.of(goToStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("// COBOL: GO TO"));
        assertTrue(result.contains("process") || result.contains("return"));
    }

    @Test
    void testTranslateInspectTallying() {
        // COBOL: INSPECT WS-STRING TALLYING WS-COUNT FOR ALL 'A'
        Statement inspectStmt = new Statement();
        inspectStmt.setType(StatementType.INSPECT);
        inspectStmt.setTarget("WS-STRING");
        inspectStmt.setExpression("TALLYING WS-COUNT FOR ALL 'A'");
        
        Paragraph paragraph = new Paragraph("TEST-INSPECT");
        paragraph.setStatements(List.of(inspectStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("// COBOL: INSPECT"));
        assertTrue(result.contains("TALLYING") || result.contains("count") || result.contains("tally"));
    }

    @Test
    void testTranslateInspectReplacing() {
        // COBOL: INSPECT WS-STRING REPLACING ALL 'A' BY 'B'
        Statement inspectStmt = new Statement();
        inspectStmt.setType(StatementType.INSPECT);
        inspectStmt.setTarget("WS-STRING");
        inspectStmt.setExpression("REPLACING ALL 'A' BY 'B'");
        
        Paragraph paragraph = new Paragraph("TEST-INSPECT-REPLACE");
        paragraph.setStatements(List.of(inspectStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("// COBOL: INSPECT"));
        assertTrue(result.contains("REPLACING") || result.contains("replace"));
    }

    @Test
    void testTranslateStringStatement() {
        // COBOL: STRING 'Hello' 'World' INTO WS-RESULT
        Statement stringStmt = new Statement();
        stringStmt.setType(StatementType.STRING);
        stringStmt.setSource("'Hello','World'");
        stringStmt.setTarget("WS-RESULT");
        
        Paragraph paragraph = new Paragraph("TEST-STRING");
        paragraph.setStatements(List.of(stringStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("// COBOL: STRING"));
        assertTrue(result.contains("StringBuilder") || result.contains("append"));
    }

    @Test
    void testTranslateUnstringStatement() {
        // COBOL: UNSTRING WS-INPUT INTO WS-FIELD1 WS-FIELD2 WS-FIELD3
        Statement unstringStmt = new Statement();
        unstringStmt.setType(StatementType.UNSTRING);
        unstringStmt.setSource("WS-INPUT");
        unstringStmt.setTarget("WS-FIELD1,WS-FIELD2,WS-FIELD3");
        unstringStmt.setExpression("DELIMITED BY ','");
        
        Paragraph paragraph = new Paragraph("TEST-UNSTRING");
        paragraph.setStatements(List.of(unstringStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("// COBOL: UNSTRING"));
        assertTrue(result.contains("split") || result.contains("parts"));
    }

    @Test
    void testTranslateSearchStatement() {
        // COBOL: SEARCH TABLE-ENTRY WHEN KEY = SEARCH-KEY
        Statement searchStmt = new Statement();
        searchStmt.setType(StatementType.SEARCH);
        searchStmt.setSource("TABLE-ENTRY");
        searchStmt.setCondition("KEY = SEARCH-KEY");
        searchStmt.setExpression("LINEAR");
        
        Paragraph paragraph = new Paragraph("TEST-SEARCH");
        paragraph.setStatements(List.of(searchStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("// COBOL: SEARCH"));
        assertTrue(result.contains("for") || result.contains("idx"));
    }

    @Test
    void testTranslateCallStatement() {
        // COBOL: CALL 'SUBPROG' USING WS-PARAM1 WS-PARAM2
        Statement callStmt = new Statement();
        callStmt.setType(StatementType.CALL);
        callStmt.setSource("'SUBPROG'");
        callStmt.setExpression("USING WS-PARAM1 WS-PARAM2");
        
        Paragraph paragraph = new Paragraph("TEST-CALL");
        paragraph.setStatements(List.of(callStmt));
        
        String result = translator.translateParagraph(paragraph, "Record");
        
        assertNotNull(result);
        assertTrue(result.contains("// COBOL: CALL"));
        assertTrue(result.contains("subprog") || result.contains("SUBPROG"));
    }
}
