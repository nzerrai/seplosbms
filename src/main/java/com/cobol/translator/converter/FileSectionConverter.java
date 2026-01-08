package com.cobol.translator.converter;

import com.cobol.translator.ast.*;
import com.cobol.translator.model.DataItem;
import com.cobol.translator.model.FileDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Converts FILE SECTION AST nodes to FileDefinition models with complete metadata.
 * Extracts all FD clauses and enriches FileDefinition for optimal code generation.
 */
public class FileSectionConverter {

    private static final Logger logger = LoggerFactory.getLogger(FileSectionConverter.class);

    // Patterns for parsing RECORD CONTAINS clause
    private static final Pattern RECORD_FIXED = Pattern.compile("(\\d+)\\s+CHARACTERS?", Pattern.CASE_INSENSITIVE);
    private static final Pattern RECORD_VARIABLE = Pattern.compile("(\\d+)\\s+TO\\s+(\\d+)\\s+CHARACTERS?", Pattern.CASE_INSENSITIVE);
    
    // Pattern for BLOCK CONTAINS
    private static final Pattern BLOCK_PATTERN = Pattern.compile("(\\d+)\\s+(RECORDS?|CHARACTERS?)", Pattern.CASE_INSENSITIVE);

    /**
     * Convert a FileSectionNode to a list of FileDefinitions
     */
    public List<FileDefinition> convertFileSection(FileSectionNode fileSectionNode) {
        logger.info("Converting FILE SECTION with {} file descriptions", 
                    fileSectionNode.getFileDescriptions().size());

        List<FileDefinition> fileDefinitions = new ArrayList<>();

        for (FileDescriptionNode fdNode : fileSectionNode.getFileDescriptions()) {
            FileDefinition fileDef = convertFileDescription(fdNode);
            fileDefinitions.add(fileDef);
            
            logger.debug("Converted file: {} (organization: {}, format: {})",
                        fileDef.getFileName(),
                        fileDef.getOrganization(),
                        fileDef.isFixedLength() ? "FIXED" : "VARIABLE");
        }

        return fileDefinitions;
    }

    /**
     * Convert a single FileDescriptionNode to FileDefinition
     */
    private FileDefinition convertFileDescription(FileDescriptionNode fdNode) {
        FileDefinition fileDef = new FileDefinition(fdNode.getFileName());

        // Transfer all metadata from AST to model
        fileDef.setOrganization(fdNode.getOrganization());
        fileDef.setAccessMode(fdNode.getAccessMode());
        fileDef.setRecordKey(fdNode.getRecordKey());
        fileDef.setFileStatus(fdNode.getFileStatus());

        // Parse RECORD CONTAINS clause
        parseRecordContains(fdNode.getRecordContains(), fileDef);

        // Parse BLOCK CONTAINS clause
        parseBlockContains(fdNode.getBlockContains(), fileDef);

        // Process LABEL RECORDS
        if (fdNode.getLabelRecords() != null) {
            fileDef.setLabelRecords(fdNode.getLabelRecords());
        }

        // Process record layouts (01-level data items)
        if (!fdNode.getRecords().isEmpty()) {
            DataItemNode firstRecord = fdNode.getRecords().get(0);
            DataItem recordLayout = convertDataItemNode(firstRecord);
            fileDef.setRecordLayout(recordLayout);
        }

        // Additional clauses
        if (fdNode.getValueOfClause() != null) {
            fileDef.addMetadata("VALUE_OF", fdNode.getValueOfClause());
        }
        if (fdNode.getDataRecords() != null) {
            fileDef.addMetadata("DATA_RECORDS", fdNode.getDataRecords());
        }
        if (fdNode.getLinageClause() != null) {
            fileDef.addMetadata("LINAGE", fdNode.getLinageClause());
        }

        // Infer organization if not explicitly set
        if (fileDef.getOrganization() == null) {
            fileDef.setOrganization(inferOrganization(fdNode));
        }

        return fileDef;
    }

    /**
     * Parse RECORD CONTAINS clause to extract min/max record lengths
     */
    private void parseRecordContains(String recordContains, FileDefinition fileDef) {
        if (recordContains == null || recordContains.trim().isEmpty()) {
            return;
        }

        // Try variable length format: "n TO m CHARACTERS"
        Matcher varMatcher = RECORD_VARIABLE.matcher(recordContains);
        if (varMatcher.find()) {
            int min = Integer.parseInt(varMatcher.group(1));
            int max = Integer.parseInt(varMatcher.group(2));
            fileDef.setMinRecordLength(min);
            fileDef.setMaxRecordLength(max);
            fileDef.setFixedLength(false);
            logger.debug("Variable length record: {} to {} characters", min, max);
            return;
        }

        // Try fixed length format: "n CHARACTERS"
        Matcher fixedMatcher = RECORD_FIXED.matcher(recordContains);
        if (fixedMatcher.find()) {
            int length = Integer.parseInt(fixedMatcher.group(1));
            fileDef.setMinRecordLength(length);
            fileDef.setMaxRecordLength(length);
            fileDef.setFixedLength(true);
            logger.debug("Fixed length record: {} characters", length);
        }
    }

    /**
     * Parse BLOCK CONTAINS clause
     */
    private void parseBlockContains(String blockContains, FileDefinition fileDef) {
        if (blockContains == null || blockContains.trim().isEmpty()) {
            return;
        }

        Matcher matcher = BLOCK_PATTERN.matcher(blockContains);
        if (matcher.find()) {
            int size = Integer.parseInt(matcher.group(1));
            String unit = matcher.group(2).toUpperCase();

            if (unit.startsWith("RECORD")) {
                fileDef.setBlockSizeRecords(size);
                logger.debug("Block size: {} records", size);
            } else {
                fileDef.setBlockSizeBytes(size);
                logger.debug("Block size: {} characters/bytes", size);
            }
        }
    }

    /**
     * Infer file organization from available metadata
     */
    private String inferOrganization(FileDescriptionNode fdNode) {
        // If RECORD KEY is present, it's INDEXED
        if (fdNode.getRecordKey() != null && !fdNode.getRecordKey().trim().isEmpty()) {
            return "INDEXED";
        }

        // If LINAGE clause is present, it's for reports (line sequential)
        if (fdNode.getLinageClause() != null) {
            return "LINE SEQUENTIAL";
        }

        // Default to SEQUENTIAL
        return "SEQUENTIAL";
    }

    /**
     * Convert DataItemNode to DataItem (simplified version)
     * For full conversion, delegate to DataItemConverter
     */
    private DataItem convertDataItemNode(DataItemNode node) {
        DataItem item = new DataItem(node.getLevel(), node.getName());
        item.setPictureClause(node.getPicture());
        
        // Group if no PICTURE clause
        boolean hasChildren = !node.getChildren().isEmpty();
        item.setGroup(hasChildren || node.getPicture() == null);
        
        // Check if FILLER
        boolean isFiller = "FILLER".equalsIgnoreCase(node.getName());
        item.setFiller(isFiller);

        // Convert children recursively
        for (ASTNode child : node.getChildren()) {
            if (child instanceof DataItemNode) {
                DataItem childItem = convertDataItemNode((DataItemNode) child);
                childItem.setParent(item);  // Set parent reference
            }
        }

        return item;
    }

    /**
     * Enriches FileDefinition with Environment Division metadata
     * (to be called after parsing SELECT/ASSIGN clauses)
     */
    public void enrichWithEnvironmentData(FileDefinition fileDef, EnvironmentDivisionNode envNode) {
        if (envNode == null) {
            return;
        }

        // Search for SELECT clause matching this file
        for (FileDescriptionNode envFd : envNode.getFileDescriptions()) {
            if (envFd.getFileName().equalsIgnoreCase(fileDef.getFileName())) {
                // Transfer organization, access mode, etc.
                if (envFd.getOrganization() != null) {
                    fileDef.setOrganization(envFd.getOrganization());
                }
                if (envFd.getAccessMode() != null) {
                    fileDef.setAccessMode(envFd.getAccessMode());
                }
                if (envFd.getRecordKey() != null) {
                    fileDef.setRecordKey(envFd.getRecordKey());
                }
                if (envFd.getFileStatus() != null) {
                    fileDef.setFileStatus(envFd.getFileStatus());
                }
                
                logger.debug("Enriched {} with Environment Division metadata", fileDef.getFileName());
                break;
            }
        }
    }

    /**
     * Validate FileDefinition completeness
     */
    public void validate(FileDefinition fileDef) {
        List<String> warnings = new ArrayList<>();

        if (fileDef.getOrganization() == null) {
            warnings.add("Organization not specified, assuming SEQUENTIAL");
            fileDef.setOrganization("SEQUENTIAL");
        }

        if ("INDEXED".equalsIgnoreCase(fileDef.getOrganization()) && 
            fileDef.getRecordKey() == null) {
            warnings.add("INDEXED file without RECORD KEY");
        }

        if (fileDef.getRecordLayout() == null) {
            warnings.add("No record layout (01-level) found");
        }

        if (!warnings.isEmpty()) {
            logger.warn("File {} validation warnings: {}", 
                       fileDef.getFileName(), String.join(", ", warnings));
        }
    }
}
