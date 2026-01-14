package com.cobol.translator.service;

import com.cobol.translator.report.ConversionReport;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Result of a conversion operation including the project path and conversion reports per file
 */
public class ConversionResult {
    private final Path projectPath;
    private final List<FileConversionReport> fileReports;
    
    public ConversionResult(Path projectPath, List<FileConversionReport> fileReports) {
        this.projectPath = projectPath;
        this.fileReports = fileReports != null ? fileReports : new ArrayList<>();
    }
    
    // Constructor for backward compatibility
    public ConversionResult(Path projectPath, ConversionReport report) {
        this.projectPath = projectPath;
        this.fileReports = new ArrayList<>();
        if (report != null) {
            this.fileReports.add(new FileConversionReport("unknown", report));
        }
    }
    
    public Path getProjectPath() {
        return projectPath;
    }
    
    public List<FileConversionReport> getFileReports() {
        return fileReports;
    }
    
    // Get the main report (for backward compatibility)
    public ConversionReport getReport() {
        return fileReports.isEmpty() ? null : fileReports.get(0).getReport();
    }
    
    /**
     * Report for a single file conversion
     */
    public static class FileConversionReport {
        private final String fileName;
        private final ConversionReport report;
        
        public FileConversionReport(String fileName, ConversionReport report) {
            this.fileName = fileName;
            this.report = report;
        }
        
        public String getFileName() {
            return fileName;
        }
        
        public ConversionReport getReport() {
            return report;
        }
    }
}
