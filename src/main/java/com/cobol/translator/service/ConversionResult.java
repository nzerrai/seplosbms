package com.cobol.translator.service;

import com.cobol.translator.report.ConversionReport;

import java.nio.file.Path;

/**
 * Result of a conversion operation including the project path and conversion report
 */
public class ConversionResult {
    private final Path projectPath;
    private final ConversionReport report;
    
    public ConversionResult(Path projectPath, ConversionReport report) {
        this.projectPath = projectPath;
        this.report = report;
    }
    
    public Path getProjectPath() {
        return projectPath;
    }
    
    public ConversionReport getReport() {
        return report;
    }
}
