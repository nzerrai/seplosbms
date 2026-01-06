package com.cobol.translator.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a parsed COBOL program.
 */
public class CobolProgram {

    private String programName;
    private String author;
    private int totalLines;
    private String sourceFile;
    private String pattern;  // Detected program pattern (e.g., FILE_PROCESSING, TABLE_SEARCH)

    private List<DataItem> dataItems = new ArrayList<>();
    private List<FileDefinition> files = new ArrayList<>();
    private List<Statement> statements = new ArrayList<>();
    private List<String> copybooks = new ArrayList<>();
    private List<Paragraph> paragraphs = new ArrayList<>();

    // Identification Division
    private String programId;
    private String dateWritten;
    private String dateCompiled;

    // Environment Division
    private List<FileAssignment> fileAssignments = new ArrayList<>();

    // Working Storage
    private List<DataItem> workingStorageItems = new ArrayList<>();

    // File Section
    private List<FileDefinition> fileDefinitions = new ArrayList<>();

    public CobolProgram() {
    }

    public CobolProgram(String programName) {
        this.programName = programName;
    }

    // Builder methods
    public CobolProgram addDataItem(DataItem item) {
        this.dataItems.add(item);
        return this;
    }

    public CobolProgram addFile(FileDefinition file) {
        this.files.add(file);
        return this;
    }

    public CobolProgram addStatement(Statement statement) {
        this.statements.add(statement);
        return this;
    }

    public CobolProgram addCopybook(String copybook) {
        this.copybooks.add(copybook);
        return this;
    }

    public CobolProgram addParagraph(Paragraph paragraph) {
        this.paragraphs.add(paragraph);
        return this;
    }

    // Getters and Setters
    public String getProgramName() { return programName; }
    public void setProgramName(String programName) { this.programName = programName; }

    public String getAuthor() { return author; }
    public void setAuthor(String author) { this.author = author; }

    public int getTotalLines() { return totalLines; }
    public void setTotalLines(int totalLines) { this.totalLines = totalLines; }

    public String getSourceFile() { return sourceFile; }
    public void setSourceFile(String sourceFile) { this.sourceFile = sourceFile; }

    public String getPattern() { return pattern; }
    public void setPattern(String pattern) { this.pattern = pattern; }

    public List<DataItem> getDataItems() { return dataItems; }
    public void setDataItems(List<DataItem> dataItems) { this.dataItems = dataItems; }

    public List<FileDefinition> getFiles() { return files; }
    public void setFiles(List<FileDefinition> files) { this.files = files; }

    public List<Statement> getStatements() { return statements; }
    public void setStatements(List<Statement> statements) { this.statements = statements; }

    public List<String> getCopybooks() { return copybooks; }
    public void setCopybooks(List<String> copybooks) { this.copybooks = copybooks; }

    public String getProgramId() { return programId; }
    public void setProgramId(String programId) { this.programId = programId; }

    public String getDateWritten() { return dateWritten; }
    public void setDateWritten(String dateWritten) { this.dateWritten = dateWritten; }

    public String getDateCompiled() { return dateCompiled; }
    public void setDateCompiled(String dateCompiled) { this.dateCompiled = dateCompiled; }

    public List<FileAssignment> getFileAssignments() { return fileAssignments; }
    public void setFileAssignments(List<FileAssignment> fileAssignments) {
        this.fileAssignments = fileAssignments;
    }

    public List<DataItem> getWorkingStorageItems() { return workingStorageItems; }
    public void setWorkingStorageItems(List<DataItem> workingStorageItems) {
        this.workingStorageItems = workingStorageItems;
    }

    public List<FileDefinition> getFileDefinitions() { return fileDefinitions; }
    public void setFileDefinitions(List<FileDefinition> fileDefinitions) {
        this.fileDefinitions = fileDefinitions;
    }

    public List<Paragraph> getParagraphs() { return paragraphs; }
    public void setParagraphs(List<Paragraph> paragraphs) { this.paragraphs = paragraphs; }

    @Override
    public String toString() {
        return "CobolProgram{" +
                "programName='" + programName + '\'' +
                ", dataItems=" + dataItems.size() +
                ", files=" + files.size() +
                ", statements=" + statements.size() +
                '}';
    }

    /**
     * Represents a file assignment in the ENVIRONMENT DIVISION.
     */
    public static class FileAssignment {
        private String logicalName;
        private String physicalName;

        public FileAssignment(String logicalName, String physicalName) {
            this.logicalName = logicalName;
            this.physicalName = physicalName;
        }

        public String getLogicalName() { return logicalName; }
        public String getPhysicalName() { return physicalName; }
    }
}
