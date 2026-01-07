package com.cobol.translator.vsam;

import com.cobol.translator.config.TranslationConfig;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.model.FileDefinition;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for VSAM file analyzer and mapper
 */
class VsamSupportTest {
    
    private VsamFileAnalyzer analyzer;
    private VsamToJdbcMapper mapper;
    private TranslationConfig config;
    
    @BeforeEach
    void setUp() {
        analyzer = new VsamFileAnalyzer();
        config = TranslationConfig.builder()
            .sourceFile("test.cob") // Required but not used in mapper
            .outputPackage("com.example.test")
            .build();
        mapper = new VsamToJdbcMapper(config);
    }
    
    @Test
    void testDetectKSDSFile() {
        FileDefinition fileDef = new FileDefinition();
        fileDef.setFileName("CUSTOMER-FILE");
        fileDef.setOrganization("INDEXED");
        
        String source = 
            "       SELECT CUSTOMER-FILE\n" +
            "           ORGANIZATION IS INDEXED\n" +
            "           ACCESS MODE IS DYNAMIC\n" +
            "           RECORD KEY IS CUST-ID.\n";
        
        VsamFileInfo info = analyzer.analyzeVsamFile(fileDef, source);
        
        assertEquals("CUSTOMER-FILE", info.getFileName());
        assertEquals(VsamFileAnalyzer.VsamType.KSDS, info.getVsamType());
        assertEquals("DYNAMIC", info.getAccessMode());
        assertEquals("CUST-ID", info.getPrimaryKey());
        assertTrue(info.isIndexed());
    }
    
    @Test
    void testDetectESDSFile() {
        FileDefinition fileDef = new FileDefinition();
        fileDef.setFileName("TRANSACTION-FILE");
        fileDef.setOrganization("SEQUENTIAL");
        
        String source = 
            "       SELECT TRANSACTION-FILE\n" +
            "           ORGANIZATION IS SEQUENTIAL\n" +
            "           ACCESS MODE IS SEQUENTIAL.\n";
        
        VsamFileInfo info = analyzer.analyzeVsamFile(fileDef, source);
        
        assertEquals(VsamFileAnalyzer.VsamType.ESDS, info.getVsamType());
        assertEquals("SEQUENTIAL", info.getAccessMode());
        assertFalse(info.isIndexed());
    }
    
    @Test
    void testDetectRRDSFile() {
        FileDefinition fileDef = new FileDefinition();
        fileDef.setFileName("RELATIVE-FILE");
        fileDef.setOrganization("RELATIVE");
        
        String source = 
            "       SELECT RELATIVE-FILE\n" +
            "           ORGANIZATION IS RELATIVE\n" +
            "           ACCESS MODE IS RANDOM.\n";
        
        VsamFileInfo info = analyzer.analyzeVsamFile(fileDef, source);
        
        assertEquals(VsamFileAnalyzer.VsamType.RRDS, info.getVsamType());
        assertEquals("RANDOM", info.getAccessMode());
    }
    
    @Test
    void testExtractAlternateKeys() {
        FileDefinition fileDef = new FileDefinition();
        fileDef.setFileName("CUSTOMER-FILE");
        fileDef.setOrganization("INDEXED");
        
        String source = 
            "       SELECT CUSTOMER-FILE\n" +
            "           ORGANIZATION IS INDEXED\n" +
            "           RECORD KEY IS CUST-ID\n" +
            "           ALTERNATE RECORD KEY IS CUST-EMAIL\n" +
            "               WITH DUPLICATES\n" +
            "           ALTERNATE RECORD KEY IS CUST-PHONE.\n";
        
        VsamFileInfo info = analyzer.analyzeVsamFile(fileDef, source);
        
        assertEquals("CUST-ID", info.getPrimaryKey());
        assertTrue(info.hasAlternateKeys());
        assertEquals(2, info.getAlternateKeys().size());
        
        AlternateKeyInfo emailKey = info.getAlternateKeys().get(0);
        assertEquals("CUST-EMAIL", emailKey.getKeyName());
        assertTrue(emailKey.isAllowDuplicates());
        
        AlternateKeyInfo phoneKey = info.getAlternateKeys().get(1);
        assertEquals("CUST-PHONE", phoneKey.getKeyName());
        assertFalse(phoneKey.isAllowDuplicates());
    }
    
    @Test
    void testGenerateJpaEntityForKSDS() throws IOException {
        // Setup VSAM info
        VsamFileInfo vsamInfo = new VsamFileInfo();
        vsamInfo.setFileName("CUSTOMER-FILE");
        vsamInfo.setVsamType(VsamFileAnalyzer.VsamType.KSDS);
        vsamInfo.setAccessMode("DYNAMIC");
        vsamInfo.setPrimaryKey("CUST-ID");
        vsamInfo.addAlternateKey(new AlternateKeyInfo("CUST-EMAIL", false));
        
        // Setup record layout - simplified without children
        DataItem custId = new DataItem();
        custId.setName("CUST-ID");
        custId.setLevel(5);
        custId.setPictureClause("9(10)");
        
        // Generate entity  
        String javaCode = mapper.generateJpaEntity(vsamInfo, custId);
        
        // Verify generated code
        assertTrue(javaCode.contains("package com.example.test.model"));
        assertTrue(javaCode.contains("@Entity"));
        assertTrue(javaCode.contains("@Table(name = \"customer_file\""));
        assertTrue(javaCode.contains("class CustomerFileEntity"));
        
        // Verify primary key
        assertTrue(javaCode.contains("@Id"));
        assertTrue(javaCode.contains("private Long custId"));
        
        // Verify alternate key index
        assertTrue(javaCode.contains("@Index"));
        assertTrue(javaCode.contains("idx_customer_file_custEmail"));
        assertTrue(javaCode.contains("unique = true")); // No duplicates
        
        // Verify getters/setters
        assertTrue(javaCode.contains("public Long getCustId()"));
        assertTrue(javaCode.contains("public void setCustId(Long custId)"));
    }
    
    @Test
    void testGenerateJpaEntityForESDSWithAutoId() throws IOException {
        // ESDS files have no primary key, should generate auto-increment ID
        VsamFileInfo vsamInfo = new VsamFileInfo();
        vsamInfo.setFileName("TRANSACTION-FILE");
        vsamInfo.setVsamType(VsamFileAnalyzer.VsamType.ESDS);
        vsamInfo.setAccessMode("SEQUENTIAL");
        vsamInfo.setPrimaryKey(null); // No primary key
        
        DataItem txnDate = new DataItem();
        txnDate.setName("TXN-DATE");
        txnDate.setLevel(5);
        txnDate.setPictureClause("X(10)");
        
        String javaCode = mapper.generateJpaEntity(vsamInfo, txnDate);
        
        // Should have auto-generated ID
        assertTrue(javaCode.contains("@Id"));
        assertTrue(javaCode.contains("@GeneratedValue(strategy = GenerationType.IDENTITY)"));
        assertTrue(javaCode.contains("private Long id"));
    }
    
    @Test
    void testAlternateKeyWithDuplicates() {
        VsamFileInfo vsamInfo = new VsamFileInfo();
        vsamInfo.addAlternateKey(new AlternateKeyInfo("STATUS", true));
        vsamInfo.addAlternateKey(new AlternateKeyInfo("ID", false));
        
        AlternateKeyInfo statusKey = vsamInfo.getAlternateKeys().get(0);
        assertTrue(statusKey.isAllowDuplicates());
        assertEquals("STATUS (with duplicates)", statusKey.toString());
        
        AlternateKeyInfo idKey = vsamInfo.getAlternateKeys().get(1);
        assertFalse(idKey.isAllowDuplicates());
        assertEquals("ID (unique)", idKey.toString());
    }
    
    @Test
    void testNumericFieldMapping() throws IOException {
        VsamFileInfo vsamInfo = new VsamFileInfo();
        vsamInfo.setFileName("TEST-FILE");
        vsamInfo.setVsamType(VsamFileAnalyzer.VsamType.KSDS);
        vsamInfo.setPrimaryKey("ID");
        
        // Integer field
        DataItem id = new DataItem();
        id.setName("ID");
        id.setPictureClause("9(5)");
        
        String javaCode = mapper.generateJpaEntity(vsamInfo, id);
        
        // Verify type mappings
        assertTrue(javaCode.contains("private Long id")); // Primary key is Long
    }
}
