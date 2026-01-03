package com.cobol.translator.parser;

import com.cobol.translator.ast.*;
import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test for the ANTLR4-based COBOL AST Parser
 */
class CobolASTParserTest {

    @Test
    void testParseSimpleProgram() {
        String cobolSource =
            "       IDENTIFICATION DIVISION.\n" +
            "       PROGRAM-ID. TESTPROG.\n" +
            "       DATA DIVISION.\n" +
            "       WORKING-STORAGE SECTION.\n" +
            "       01  WS-VAR PIC X(10).\n" +
            "       PROCEDURE DIVISION.\n" +
            "       MAIN-PARA.\n" +
            "           DISPLAY 'HELLO'.\n" +
            "           STOP RUN.\n";

        CobolASTParser parser = new CobolASTParser();
        ProgramNode program = parser.parseString(cobolSource, "test-program");

        assertNotNull(program, "Program node should not be null");
        assertEquals("TESTPROG", program.getProgramName(), "Program name should be TESTPROG");
        assertNotNull(program.getIdentificationDivision(), "Identification division should exist");
        assertNotNull(program.getDataDivision(), "Data division should exist");
        assertNotNull(program.getProcedureDivision(), "Procedure division should exist");
    }

    @Test
    void testParseWithWorkingStorage() {
        String cobolSource =
            "       IDENTIFICATION DIVISION.\n" +
            "       PROGRAM-ID. WSPROG.\n" +
            "       DATA DIVISION.\n" +
            "       WORKING-STORAGE SECTION.\n" +
            "       01  WS-COUNT PIC 9(5) VALUE 0.\n" +
            "       01  WS-NAME  PIC X(30).\n" +
            "       PROCEDURE DIVISION.\n" +
            "       MAIN-PARA.\n" +
            "           STOP RUN.\n";

        CobolASTParser parser = new CobolASTParser();
        ProgramNode program = parser.parseString(cobolSource, "ws-test");

        assertNotNull(program);
        assertNotNull(program.getDataDivision());

        WorkingStorageSectionNode wss = program.getDataDivision().getWorkingStorageSection();
        assertNotNull(wss, "Working-Storage section should exist");

        assertFalse(wss.getDataItems().isEmpty(), "Should have data items");
        assertEquals(2, wss.getDataItems().size(), "Should have 2 data items");

        DataItemNode firstItem = wss.getDataItems().get(0);
        assertEquals(1, firstItem.getLevel(), "First item should be level 01");
        assertEquals("WS-COUNT", firstItem.getName(), "First item name should be WS-COUNT");
    }

    @Test
    void testParseRealFile() throws Exception {
        Path cobolFile = Paths.get("examples/simple-customer.cob");

        if (!cobolFile.toFile().exists()) {
            System.out.println("Skipping test - example file not found");
            return;
        }

        CobolASTParser parser = new CobolASTParser();
        ProgramNode program = parser.parse(cobolFile);

        assertNotNull(program, "Program should be parsed");
        assertEquals("CUSTPROC", program.getProgramName(), "Program name should be CUSTPROC");

        // Verify divisions exist
        assertNotNull(program.getIdentificationDivision());
        assertNotNull(program.getEnvironmentDivision());
        assertNotNull(program.getDataDivision());
        assertNotNull(program.getProcedureDivision());

        // Verify file section
        DataDivisionNode dataDiv = program.getDataDivision();
        assertNotNull(dataDiv.getFileSection(), "File section should exist");

        FileSectionNode fileSection = dataDiv.getFileSection();
        assertFalse(fileSection.getFileDescriptions().isEmpty(), "Should have file descriptions");

        // Verify working-storage
        WorkingStorageSectionNode wss = dataDiv.getWorkingStorageSection();
        assertNotNull(wss, "Working-storage section should exist");
        assertFalse(wss.getDataItems().isEmpty(), "Should have working-storage items");

        // Verify procedure division
        ProcedureDivisionNode procDiv = program.getProcedureDivision();
        assertFalse(procDiv.getParagraphs().isEmpty(), "Should have paragraphs");

        System.out.println("✓ Successfully parsed COBOL program: " + program.getProgramName());
        System.out.println("  - File descriptions: " + fileSection.getFileDescriptions().size());
        System.out.println("  - Working-storage items: " + wss.getDataItems().size());
        System.out.println("  - Paragraphs: " + procDiv.getParagraphs().size());
    }

    @Test
    void testIsValidSyntax() {
        CobolASTParser parser = new CobolASTParser();

        String validCode =
            "       IDENTIFICATION DIVISION.\n" +
            "       PROGRAM-ID. VALID.\n" +
            "       PROCEDURE DIVISION.\n" +
            "       MAIN-PARA.\n" +
            "           STOP RUN.\n";

        assertTrue(parser.isValidSyntax(validCode), "Valid COBOL should return true");

        String invalidCode = "THIS IS NOT VALID COBOL";
        assertFalse(parser.isValidSyntax(invalidCode), "Invalid COBOL should return false");
    }
}
