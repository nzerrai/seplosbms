package com.cobol.translator.semantic;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test rapide de validation - Phase 1 Sémantique
 */
public class SemanticFoundationTest {
    private SymbolTable symbolTable;
    private TypeChecker typeChecker;

    @BeforeEach
    public void setUp() {
        symbolTable = new SymbolTable();
        typeChecker = new TypeChecker(symbolTable);
    }

    @Test
    public void testSymbolTableCreation() {
        assertNotNull(symbolTable);
        assertEquals(0, symbolTable.size());
    }

    @Test
    public void testRegisterVariable() {
        symbolTable.registerVariable("WS-COUNTER", "PIC 9(5)");
        assertTrue(symbolTable.symbolExists("WS-COUNTER"));
        assertEquals(1, symbolTable.size());
    }

    @Test
    public void testVariableLookup() {
        symbolTable.registerVariable("CUST-ID", "PIC 9(6)");
        Symbol symbol = symbolTable.lookupSymbol("CUST-ID");
        assertNotNull(symbol);
        assertEquals("CUST-ID", symbol.getName());
        assertEquals(SymbolType.VARIABLE, symbol.getType());
    }

    @Test
    public void testPicToJavaTypeConversion() {
        symbolTable.registerVariable("WS-NUMERIC", "PIC 9(5)");
        Symbol symbol = symbolTable.lookupSymbol("WS-NUMERIC");
        assertNotNull(symbol.getJavaType());
        assertTrue(symbol.getJavaType().contains("Integer") || symbol.getJavaType().contains("Long"));
    }

    @Test
    public void testDecimalType() {
        symbolTable.registerVariable("WS-AMOUNT", "PIC 9(9)V99");
        Symbol symbol = symbolTable.lookupSymbol("WS-AMOUNT");
        assertEquals("BigDecimal", symbol.getJavaType());
    }

    @Test
    public void testStringType() {
        symbolTable.registerVariable("WS-NAME", "PIC X(30)");
        Symbol symbol = symbolTable.lookupSymbol("WS-NAME");
        assertEquals("String", symbol.getJavaType());
    }

    @Test
    public void testTypeChecker() {
        assertNotNull(typeChecker);
        assertNotNull(typeChecker.getErrorCollector());
        assertFalse(typeChecker.hasErrors());
    }

    @Test
    public void testUndefinedVariableDetection() {
        typeChecker.validateMove("UNDEFINED-VAR", "WS-TARGET", 1);
        assertTrue(typeChecker.hasErrors());
        assertEquals(1, typeChecker.getErrorCollector().getErrorCount());
    }

    @Test
    public void testMoveValidation() {
        symbolTable.registerVariable("WS-SOURCE", "PIC 9(5)");
        symbolTable.registerVariable("WS-TARGET", "PIC 9(7)");
        
        boolean result = typeChecker.validateMove("WS-SOURCE", "WS-TARGET", 1);
        assertTrue(result);
        assertFalse(typeChecker.hasErrors());
    }

    @Test
    public void testParagraphRegistration() {
        symbolTable.registerParagraph("1000-MAIN-LOGIC");
        assertTrue(symbolTable.symbolExists("1000-MAIN-LOGIC"));
        
        Symbol para = symbolTable.lookupSymbol("1000-MAIN-LOGIC");
        assertEquals(SymbolType.PARAGRAPH, para.getType());
    }

    @Test
    public void testFileRegistration() {
        symbolTable.registerFile("CUSTOMER-FILE");
        assertTrue(symbolTable.symbolExists("CUSTOMER-FILE"));
        
        Symbol file = symbolTable.lookupSymbol("CUSTOMER-FILE");
        assertEquals(SymbolType.FILE, file.getType());
    }

    @Test
    public void testScopeManagement() {
        // Global scope
        symbolTable.registerVariable("GLOBAL-VAR", "PIC X(10)");
        
        // Enter new scope
        symbolTable.enterScope("WORKING-STORAGE", Scope.ScopeLevel.SECTION);
        symbolTable.registerVariable("LOCAL-VAR", "PIC 9(5)");
        
        // Should find both
        assertTrue(symbolTable.symbolExists("GLOBAL-VAR"));
        assertTrue(symbolTable.symbolExists("LOCAL-VAR"));
        
        // Exit scope
        symbolTable.exitScope();
        
        // Should find global, not local
        assertTrue(symbolTable.symbolExists("GLOBAL-VAR"));
        assertFalse(symbolTable.symbolExists("LOCAL-VAR"));
    }

    @Test
    public void testSemanticErrorCollection() {
        SemanticErrorCollector collector = new SemanticErrorCollector();
        
        collector.addError("E001", "Undefined variable", 10);
        collector.addWarning("W001", "Type mismatch", 11);
        
        assertTrue(collector.hasErrors());
        assertTrue(collector.hasWarnings());
        assertEquals(1, collector.getErrorCount());
        assertEquals(1, collector.getWarningCount());
    }

    @Test
    public void testFullWorkflow() {
        // Setup program structure
        symbolTable.registerVariable("WS-COUNTER", "PIC 9(5)");
        symbolTable.registerVariable("WS-TOTAL", "PIC 9(9)V99");
        symbolTable.registerParagraph("MAIN-LOGIC");
        symbolTable.registerFile("INPUT-FILE");
        
        // Validate operations
        assertTrue(typeChecker.validateMove("WS-COUNTER", "WS-TOTAL", 1));
        assertTrue(typeChecker.validatePerform("MAIN-LOGIC", 2));
        assertTrue(typeChecker.validateFileOperation("INPUT-FILE", "READ", 3));
        
        // Should have no errors
        assertFalse(typeChecker.hasErrors());
    }
}
