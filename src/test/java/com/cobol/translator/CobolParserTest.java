package com.cobol.translator;

import com.cobol.translator.model.CobolProgram;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.parser.CobolParser;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Tests for COBOL parser.
 */
class CobolParserTest {

    private final CobolParser parser = new CobolParser();

    @Test
    void shouldParseProgramName() {
        String cobol = """
               IDENTIFICATION DIVISION.
               PROGRAM-ID. TESTPROG.
               """;

        CobolProgram program = parser.parse(cobol);

        assertThat(program.getProgramName()).isEqualTo("TESTPROG");
    }

    @Test
    void shouldParseDataItems() {
        String cobol = """
               IDENTIFICATION DIVISION.
               PROGRAM-ID. TESTPROG.

               DATA DIVISION.
               WORKING-STORAGE SECTION.
               01  WS-CUSTOMER.
                   05  WS-ID           PIC 9(6).
                   05  WS-NAME         PIC X(30).
                   05  WS-AMOUNT       PIC 9(7)V99 COMP-3.
               """;

        CobolProgram program = parser.parse(cobol);

        assertThat(program.getDataItems()).hasSize(4); // 01 + 3 fields

        DataItem idField = program.getDataItems().stream()
                .filter(d -> "WS-ID".equals(d.getName()))
                .findFirst()
                .orElseThrow();

        assertThat(idField.getLevel()).isEqualTo(5);
        assertThat(idField.getPictureClause()).isEqualTo("9(6)");
        assertThat(idField.getJavaType()).isEqualTo("Integer");
        assertThat(idField.getJavaFieldName()).isEqualTo("wsId");
    }

    @Test
    void shouldDetectComp3AsDecimal() {
        String cobol = """
               DATA DIVISION.
               WORKING-STORAGE SECTION.
               01  WS-AMOUNT       PIC 9(7)V99 COMP-3.
               """;

        CobolProgram program = parser.parse(cobol);

        DataItem amount = program.getDataItems().stream()
                .filter(d -> "WS-AMOUNT".equals(d.getName()))
                .findFirst()
                .orElseThrow();

        assertThat(amount.isComp3()).isTrue();
        assertThat(amount.hasDecimals()).isTrue();
        assertThat(amount.getJavaType()).isEqualTo("BigDecimal");
    }

    @Test
    void shouldDetectDateFields() {
        String cobol = """
               DATA DIVISION.
               WORKING-STORAGE SECTION.
               01  WS-ORDER-DATE   PIC 9(8).
               """;

        CobolProgram program = parser.parse(cobol);

        DataItem dateField = program.getDataItems().stream()
                .filter(d -> "WS-ORDER-DATE".equals(d.getName()))
                .findFirst()
                .orElseThrow();

        assertThat(dateField.isPotentialDateField()).isTrue();
        assertThat(dateField.getJavaType()).isEqualTo("LocalDate");
    }

    @Test
    void shouldParseFileDefinitions() {
        String cobol = """
               DATA DIVISION.
               FILE SECTION.
               FD  CUSTOMER-FILE.
               01  CUSTOMER-RECORD.
                   05  CUST-ID     PIC 9(6).
               """;

        CobolProgram program = parser.parse(cobol);

        assertThat(program.getFileDefinitions()).hasSize(1);
        assertThat(program.getFileDefinitions().get(0).getFileName())
                .isEqualTo("CUSTOMER-FILE");
    }
}
