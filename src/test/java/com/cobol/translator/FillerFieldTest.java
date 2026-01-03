package com.cobol.translator;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.parser.CobolParser;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for FILLER field support
 */
public class FillerFieldTest {

    @Test
    public void shouldParseFILLERFields() {
        String cobolSource = """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. FILLERTEST.

            DATA DIVISION.
            WORKING-STORAGE SECTION.
            01  TEST-RECORD.
                05  FIELD-A          PIC X(10).
                05  FILLER           PIC X(5).
                05  FIELD-B          PIC 9(6).
                05  FILLER           PIC X(2).
                05  FIELD-C          PIC X(20).
            """;

        CobolParser parser = new CobolParser();
        CobolProgram program = parser.parse(cobolSource);

        List<DataItem> items = program.getDataItems();

        // Should have TEST-RECORD (01), FIELD-A, FILLER-1, FIELD-B, FILLER-2, FIELD-C
        assertEquals(6, items.size(), "Should parse 6 data items");

        // Find FILLER fields
        long fillerCount = items.stream()
            .filter(DataItem::isFiller)
            .count();

        assertEquals(2, fillerCount, "Should detect 2 FILLER fields");

        // Check FILLER fields have unique names
        DataItem filler1 = items.stream()
            .filter(item -> item.getName().equals("FILLER-1"))
            .findFirst()
            .orElse(null);

        assertNotNull(filler1, "Should find FILLER-1");
        assertTrue(filler1.isFiller(), "FILLER-1 should be marked as filler");
        assertEquals("X(5)", filler1.getPictureClause(), "FILLER-1 should have PIC X(5)");

        DataItem filler2 = items.stream()
            .filter(item -> item.getName().equals("FILLER-2"))
            .findFirst()
            .orElse(null);

        assertNotNull(filler2, "Should find FILLER-2");
        assertTrue(filler2.isFiller(), "FILLER-2 should be marked as filler");
        assertEquals("X(2)", filler2.getPictureClause(), "FILLER-2 should have PIC X(2)");
    }

    @Test
    public void shouldParseFILLERWithValue() {
        String cobolSource = """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. FILLERVAL.

            DATA DIVISION.
            WORKING-STORAGE SECTION.
            01  HEADER-LINE.
                05  FILLER           PIC X(10) VALUE 'CUSTOMER: '.
                05  HEADER-ID        PIC 9(6).
                05  FILLER           PIC X(3) VALUE ' - '.
                05  HEADER-NAME      PIC X(30).
            """;

        CobolParser parser = new CobolParser();
        CobolProgram program = parser.parse(cobolSource);

        List<DataItem> items = program.getDataItems();

        // Find FILLER fields
        List<DataItem> fillers = items.stream()
            .filter(DataItem::isFiller)
            .toList();

        assertEquals(2, fillers.size(), "Should detect 2 FILLER fields");

        // All FILLER fields should have unique names
        assertEquals("FILLER-1", fillers.get(0).getName());
        assertEquals("FILLER-2", fillers.get(1).getName());
    }

    @Test
    public void shouldHandleMixedCaseFILLER() {
        String cobolSource = """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. MIXEDCASE.

            DATA DIVISION.
            WORKING-STORAGE SECTION.
            01  TEST-REC.
                05  DATA-1           PIC X(10).
                05  FILLER           PIC X(5).
                05  filler           PIC X(3).
                05  Filler           PIC X(2).
                05  DATA-2           PIC 9(6).
            """;

        CobolParser parser = new CobolParser();
        CobolProgram program = parser.parse(cobolSource);

        List<DataItem> items = program.getDataItems();

        // Count FILLER fields (case-insensitive)
        long fillerCount = items.stream()
            .filter(DataItem::isFiller)
            .count();

        assertEquals(3, fillerCount, "Should detect 3 FILLER fields (case-insensitive)");

        // Check unique names
        List<String> fillerNames = items.stream()
            .filter(DataItem::isFiller)
            .map(DataItem::getName)
            .toList();

        assertTrue(fillerNames.contains("FILLER-1"));
        assertTrue(fillerNames.contains("FILLER-2"));
        assertTrue(fillerNames.contains("FILLER-3"));
    }
}
