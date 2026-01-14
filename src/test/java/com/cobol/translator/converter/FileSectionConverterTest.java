package com.cobol.translator.converter;

import com.cobol.translator.ast.*;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.model.FileDefinition;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for FileSectionConverter
 */
class FileSectionConverterTest {

    private FileSectionConverter converter;
    private FileSectionNode fileSectionNode;

    @BeforeEach
    void setUp() {
        converter = new FileSectionConverter();
        fileSectionNode = new FileSectionNode();
    }

    @Test
    @DisplayName("Convertit une FILE SECTION simple avec un fichier séquentiel")
    void convertsSimpleSequentialFile() {
        // Arrange
        FileDescriptionNode fdNode = new FileDescriptionNode("CUSTOMER-FILE");
        fdNode.setOrganization("SEQUENTIAL");
        fdNode.setRecordContains("80 CHARACTERS");
        fdNode.setLabelRecords("STANDARD");

        DataItemNode record = new DataItemNode(1, "CUSTOMER-RECORD");
        // No picture = group item
        fdNode.addRecord(record);

        fileSectionNode.addFileDescription(fdNode);

        // Act
        List<FileDefinition> result = converter.convertFileSection(fileSectionNode);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());

        FileDefinition fileDef = result.get(0);
        assertEquals("CUSTOMER-FILE", fileDef.getFileName());
        assertEquals("SEQUENTIAL", fileDef.getOrganization());
        assertEquals(Integer.valueOf(80), fileDef.getMinRecordLength());
        assertEquals(Integer.valueOf(80), fileDef.getMaxRecordLength());
        assertTrue(fileDef.isFixedLength());
        assertTrue(fileDef.isSequential());
        assertFalse(fileDef.isIndexed());
        assertEquals("STANDARD", fileDef.getLabelRecords());
    }

    @Test
    @DisplayName("Convertit un fichier à longueur variable")
    void convertsVariableLengthFile() {
        // Arrange
        FileDescriptionNode fdNode = new FileDescriptionNode("VAR-FILE");
        fdNode.setRecordContains("50 TO 150 CHARACTERS");

        fileSectionNode.addFileDescription(fdNode);

        // Act
        List<FileDefinition> result = converter.convertFileSection(fileSectionNode);

        // Assert
        FileDefinition fileDef = result.get(0);
        assertEquals(Integer.valueOf(50), fileDef.getMinRecordLength());
        assertEquals(Integer.valueOf(150), fileDef.getMaxRecordLength());
        assertFalse(fileDef.isFixedLength());
    }

    @Test
    @DisplayName("Convertit un fichier indexé (VSAM KSDS)")
    void convertsIndexedFile() {
        // Arrange
        FileDescriptionNode fdNode = new FileDescriptionNode("INDEXED-FILE");
        fdNode.setOrganization("INDEXED");
        fdNode.setAccessMode("DYNAMIC");
        fdNode.setRecordKey("CUSTOMER-ID");
        fdNode.setFileStatus("WS-FILE-STATUS");
        fdNode.setRecordContains("100 CHARACTERS");

        DataItemNode record = new DataItemNode(1, "CUSTOMER-RECORD");
        // No picture = group
        
        DataItemNode keyField = new DataItemNode(5, "CUSTOMER-ID");
        keyField.setPicture("9(6)");
        record.addChild(keyField);
        
        fdNode.addRecord(record);

        fileSectionNode.addFileDescription(fdNode);

        // Act
        List<FileDefinition> result = converter.convertFileSection(fileSectionNode);

        // Assert
        FileDefinition fileDef = result.get(0);
        assertEquals("INDEXED-FILE", fileDef.getFileName());
        assertTrue(fileDef.isIndexed());
        assertFalse(fileDef.isSequential());
        assertEquals("INDEXED", fileDef.getOrganization());
        assertEquals("DYNAMIC", fileDef.getAccessMode());
        assertEquals("CUSTOMER-ID", fileDef.getRecordKey());
        assertEquals("WS-FILE-STATUS", fileDef.getFileStatus());
    }

    @Test
    @DisplayName("Parse BLOCK CONTAINS en records")
    void parsesBlockContainsRecords() {
        // Arrange
        FileDescriptionNode fdNode = new FileDescriptionNode("BLOCKED-FILE");
        fdNode.setBlockContains("10 RECORDS");

        fileSectionNode.addFileDescription(fdNode);

        // Act
        List<FileDefinition> result = converter.convertFileSection(fileSectionNode);

        // Assert
        FileDefinition fileDef = result.get(0);
        assertEquals(Integer.valueOf(10), fileDef.getBlockSizeRecords());
        assertNull(fileDef.getBlockSizeBytes());
    }

    @Test
    @DisplayName("Parse BLOCK CONTAINS en caractères")
    void parsesBlockContainsCharacters() {
        // Arrange
        FileDescriptionNode fdNode = new FileDescriptionNode("BLOCKED-FILE");
        fdNode.setBlockContains("8000 CHARACTERS");

        fileSectionNode.addFileDescription(fdNode);

        // Act
        List<FileDefinition> result = converter.convertFileSection(fileSectionNode);

        // Assert
        FileDefinition fileDef = result.get(0);
        assertEquals(Integer.valueOf(8000), fileDef.getBlockSizeBytes());
        assertNull(fileDef.getBlockSizeRecords());
    }

    @Test
    @DisplayName("Infère INDEXED organization quand RECORD KEY est présent")
    void infersIndexedOrganizationFromRecordKey() {
        // Arrange
        FileDescriptionNode fdNode = new FileDescriptionNode("AUTO-INDEXED");
        fdNode.setRecordKey("KEY-FIELD");
        // organization not explicitly set

        fileSectionNode.addFileDescription(fdNode);

        // Act
        List<FileDefinition> result = converter.convertFileSection(fileSectionNode);

        // Assert
        FileDefinition fileDef = result.get(0);
        assertEquals("INDEXED", fileDef.getOrganization());
        assertTrue(fileDef.isIndexed());
    }

    @Test
    @DisplayName("Infère LINE SEQUENTIAL pour fichiers avec LINAGE")
    void infersLineSequentialFromLinage() {
        // Arrange
        FileDescriptionNode fdNode = new FileDescriptionNode("REPORT-FILE");
        fdNode.setLinageClause("LINAGE IS 60 LINES");

        fileSectionNode.addFileDescription(fdNode);

        // Act
        List<FileDefinition> result = converter.convertFileSection(fileSectionNode);

        // Assert
        FileDefinition fileDef = result.get(0);
        assertEquals("LINE SEQUENTIAL", fileDef.getOrganization());
    }

    @Test
    @DisplayName("Stocke les métadonnées additionnelles")
    void storesAdditionalMetadata() {
        // Arrange
        FileDescriptionNode fdNode = new FileDescriptionNode("META-FILE");
        fdNode.setValueOfClause("ID IS 'CUSTFILE'");
        fdNode.setDataRecords("CUSTOMER-RECORD");

        fileSectionNode.addFileDescription(fdNode);

        // Act
        List<FileDefinition> result = converter.convertFileSection(fileSectionNode);

        // Assert
        FileDefinition fileDef = result.get(0);
        assertEquals("ID IS 'CUSTFILE'", fileDef.getMetadata("VALUE_OF"));
        assertEquals("CUSTOMER-RECORD", fileDef.getMetadata("DATA_RECORDS"));
    }

    @Test
    @DisplayName("Convertit plusieurs fichiers dans FILE SECTION")
    void convertsMultipleFiles() {
        // Arrange
        FileDescriptionNode fd1 = new FileDescriptionNode("INPUT-FILE");
        fd1.setOrganization("SEQUENTIAL");
        fd1.setRecordContains("80 CHARACTERS");

        FileDescriptionNode fd2 = new FileDescriptionNode("OUTPUT-FILE");
        fd2.setOrganization("SEQUENTIAL");
        fd2.setRecordContains("132 CHARACTERS");

        fileSectionNode.addFileDescription(fd1);
        fileSectionNode.addFileDescription(fd2);

        // Act
        List<FileDefinition> result = converter.convertFileSection(fileSectionNode);

        // Assert
        assertEquals(2, result.size());
        assertEquals("INPUT-FILE", result.get(0).getFileName());
        assertEquals("OUTPUT-FILE", result.get(1).getFileName());
        assertEquals(Integer.valueOf(80), result.get(0).getMinRecordLength());
        assertEquals(Integer.valueOf(132), result.get(1).getMinRecordLength());
    }

    @Test
    @DisplayName("Valide FileDefinition avec avertissements")
    void validatesFileDefinitionWithWarnings() {
        // Arrange
        FileDescriptionNode fdNode = new FileDescriptionNode("INCOMPLETE-FILE");
        // No organization, no record layout
        fileSectionNode.addFileDescription(fdNode);

        List<FileDefinition> result = converter.convertFileSection(fileSectionNode);
        FileDefinition fileDef = result.get(0);

        // Act & Assert - should not throw, just log warnings
        assertDoesNotThrow(() -> converter.validate(fileDef));
        
        // Organization should default to SEQUENTIAL after validation
        assertEquals("SEQUENTIAL", fileDef.getOrganization());
    }

    @Test
    @DisplayName("Enrichit avec données Environment Division")
    void enrichesWithEnvironmentData() {
        // Arrange
        FileDescriptionNode fdNode = new FileDescriptionNode("CUSTOMER-FILE");
        fileSectionNode.addFileDescription(fdNode);
        List<FileDefinition> result = converter.convertFileSection(fileSectionNode);
        FileDefinition fileDef = result.get(0);

        // Create Environment Division node
        EnvironmentDivisionNode envNode = new EnvironmentDivisionNode();
        FileDescriptionNode envFd = new FileDescriptionNode("CUSTOMER-FILE");
        envFd.setOrganization("INDEXED");
        envFd.setAccessMode("RANDOM");
        envFd.setRecordKey("CUST-ID");
        envFd.setFileStatus("FILE-STATUS-01");
        envNode.addFileDescription(envFd);

        // Act
        converter.enrichWithEnvironmentData(fileDef, envNode);

        // Assert
        assertEquals("INDEXED", fileDef.getOrganization());
        assertEquals("RANDOM", fileDef.getAccessMode());
        assertEquals("CUST-ID", fileDef.getRecordKey());
        assertEquals("FILE-STATUS-01", fileDef.getFileStatus());
    }

    @Test
    @DisplayName("Convertit record layout avec hiérarchie de DataItems")
    void convertsRecordLayoutHierarchy() {
        // Arrange
        FileDescriptionNode fdNode = new FileDescriptionNode("CUSTOMER-FILE");
        
        DataItemNode record = new DataItemNode(1, "CUSTOMER-RECORD");
        // No picture = group
        
        DataItemNode field1 = new DataItemNode(5, "CUST-ID");
        field1.setPicture("9(6)");
        
        DataItemNode field2 = new DataItemNode(5, "CUST-NAME");
        field2.setPicture("X(30)");
        
        record.addChild(field1);
        record.addChild(field2);
        fdNode.addRecord(record);
        
        fileSectionNode.addFileDescription(fdNode);

        // Act
        List<FileDefinition> result = converter.convertFileSection(fileSectionNode);

        // Assert
        FileDefinition fileDef = result.get(0);
        assertNotNull(fileDef.getRecordLayout());
        
        DataItem recordLayout = fileDef.getRecordLayout();
        assertEquals("CUSTOMER-RECORD", recordLayout.getName());
        assertFalse(recordLayout.isElementary());
        // Note: DataItem doesn't expose children list, would need separate tracking
    }
}
