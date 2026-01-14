package com.cobol.translator.copybook;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for CopybookResolver functionality
 */
class CopybookResolverTest {
    
    @TempDir
    Path tempDir;
    
    private CopybookResolver resolver;
    private Path copybooksDir;
    
    @BeforeEach
    void setUp() throws IOException {
        copybooksDir = tempDir.resolve("copybooks");
        Files.createDirectories(copybooksDir);
        resolver = new CopybookResolver();
        resolver.addSearchPath(copybooksDir);
    }
    
    @Test
    void testSimpleCopyResolution() throws IOException {
        // Create a simple copybook
        Path customerCopy = copybooksDir.resolve("CUSTOMER-RECORD.cpy");
        Files.writeString(customerCopy,
            "       01  CUSTOMER-RECORD.\n" +
            "           05  CUST-ID         PIC 9(10).\n" +
            "           05  CUST-NAME       PIC X(30).\n");
        
        // Source code with COPY statement
        String source = 
            "       DATA DIVISION.\n" +
            "       FILE SECTION.\n" +
            "       FD  CUSTOMER-FILE.\n" +
            "       COPY CUSTOMER-RECORD.\n" +
            "       PROCEDURE DIVISION.\n";
        
        String result = resolver.resolveAllCopybooks(source);
        
        // Verify copybook was expanded
        assertTrue(result.contains("BEGIN COPYBOOK: CUSTOMER-RECORD"));
        assertTrue(result.contains("CUST-ID"));
        assertTrue(result.contains("CUST-NAME"));
        assertTrue(result.contains("END COPYBOOK: CUSTOMER-RECORD"));
        
        // Verify tracking
        Set<String> resolved = resolver.getResolvedCopybooks();
        assertTrue(resolved.contains("CUSTOMER-RECORD"));
    }
    
    @Test
    void testCopyWithReplacing() throws IOException {
        // Create copybook
        Path templateCopy = copybooksDir.resolve("TEMPLATE.cpy");
        Files.writeString(templateCopy,
            "       01  RECORD-NAME.\n" +
            "           05  FIELD-ID        PIC 9(10).\n" +
            "           05  FIELD-NAME      PIC X(30).\n");
        
        // Source with COPY REPLACING
        String source =
            "       DATA DIVISION.\n" +
            "       WORKING-STORAGE SECTION.\n" +
            "       COPY TEMPLATE REPLACING\n" +
            "           ==RECORD-NAME== BY ==CUSTOMER-DATA==\n" +
            "           ==FIELD-== BY ==CUST-==.\n";
        
        String result = resolver.resolveAllCopybooks(source);
        
        // Verify replacements
        assertTrue(result.contains("CUSTOMER-DATA"));
        assertTrue(result.contains("CUST-ID"));
        assertTrue(result.contains("CUST-NAME"));
        assertFalse(result.contains("RECORD-NAME"));
        assertFalse(result.contains("FIELD-ID"));
    }
    
    @Test
    void testNestedCopybooks() throws IOException {
        // Create nested copybooks
        Path addressCopy = copybooksDir.resolve("ADDRESS.cpy");
        Files.writeString(addressCopy,
            "       05  ADDRESS.\n" +
            "           10  STREET      PIC X(40).\n" +
            "           10  CITY        PIC X(30).\n");
        
        Path customerCopy = copybooksDir.resolve("CUSTOMER.cpy");
        Files.writeString(customerCopy,
            "       01  CUSTOMER.\n" +
            "           05  CUST-ID     PIC 9(10).\n" +
            "       COPY ADDRESS.\n");
        
        String source = "       COPY CUSTOMER.\n";
        
        String result = resolver.resolveAllCopybooks(source);
        
        // Verify both copybooks were expanded
        assertTrue(result.contains("BEGIN COPYBOOK: CUSTOMER"));
        assertTrue(result.contains("BEGIN COPYBOOK: ADDRESS"));
        assertTrue(result.contains("CUST-ID"));
        assertTrue(result.contains("STREET"));
        assertTrue(result.contains("CITY"));
        
        Set<String> resolved = resolver.getResolvedCopybooks();
        assertEquals(2, resolved.size());
        assertTrue(resolved.contains("CUSTOMER"));
        assertTrue(resolved.contains("ADDRESS"));
    }
    
    @Test
    void testCircularCopyDetection() throws IOException {
        // Create circular references
        Path copy1 = copybooksDir.resolve("COPY1.cpy");
        Files.writeString(copy1, "       COPY COPY2.\n");
        
        Path copy2 = copybooksDir.resolve("COPY2.cpy");
        Files.writeString(copy2, "       COPY COPY1.\n");
        
        String source = "       COPY COPY1.\n";
        
        String result = resolver.resolveAllCopybooks(source);
        
        // Should contain error message
        assertTrue(result.contains("ERROR: Circular COPY reference"));
    }
    
    @Test
    void testCopybookNotFound() {
        String source = "       COPY NONEXISTENT.\n";
        
        String result = resolver.resolveAllCopybooks(source);
        
        // Should contain error message
        assertTrue(result.contains("ERROR: COPYBOOK NOT FOUND: NONEXISTENT"));
    }
    
    @Test
    void testMultipleCopyStatements() throws IOException {
        // Create multiple copybooks
        Path copy1 = copybooksDir.resolve("COPY1.cpy");
        Files.writeString(copy1, "       01  DATA1    PIC X(10).\n");
        
        Path copy2 = copybooksDir.resolve("COPY2.cpy");
        Files.writeString(copy2, "       01  DATA2    PIC 9(10).\n");
        
        String source = 
            "       COPY COPY1.\n" +
            "       COPY COPY2.\n";
        
        String result = resolver.resolveAllCopybooks(source);
        
        assertTrue(result.contains("DATA1"));
        assertTrue(result.contains("DATA2"));
        assertEquals(2, resolver.getResolvedCopybooks().size());
    }
    
    @Test
    void testCopybookCache() throws IOException {
        Path customerCopy = copybooksDir.resolve("CUSTOMER.cpy");
        Files.writeString(customerCopy, "       01  CUSTOMER PIC X(10).\n");
        
        // First resolution
        String source1 = "       COPY CUSTOMER.\n";
        resolver.resolveAllCopybooks(source1);
        
        // Second resolution - should use cache
        String source2 = "       COPY CUSTOMER.\n";
        String result = resolver.resolveAllCopybooks(source2);
        
        assertTrue(result.contains("CUSTOMER"));
    }
    
    @Test
    void testClearCache() throws IOException {
        Path customerCopy = copybooksDir.resolve("CUSTOMER.cpy");
        Files.writeString(customerCopy, "       01  CUSTOMER PIC X(10).\n");
        
        String source = "       COPY CUSTOMER.\n";
        resolver.resolveAllCopybooks(source);
        
        assertEquals(1, resolver.getResolvedCopybooks().size());
        
        resolver.clearCache();
        
        assertEquals(0, resolver.getResolvedCopybooks().size());
    }
    
    @Test
    void testCopybookExists() throws IOException {
        Path customerCopy = copybooksDir.resolve("CUSTOMER.cpy");
        Files.writeString(customerCopy, "       01  CUSTOMER PIC X(10).\n");
        
        assertTrue(resolver.copybookExists("CUSTOMER"));
        assertFalse(resolver.copybookExists("NONEXISTENT"));
    }
}
