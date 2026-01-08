package com.cobol.translator.controller;

import com.cobol.translator.report.ConversionReport;
import com.cobol.translator.service.ConversionResult;

import java.util.ArrayList;
import java.util.List;

/**
 * Response object for conversion endpoint including conversion reports per file
 */
public class ConversionResponse {
    private boolean success;
    private String message;
    private String projectName;
    private String downloadUrl;
    private String zipFileBase64;
    private List<ConversionReportSummary> reports;

    public ConversionResponse() {}

    public ConversionResponse(boolean success, String message) {
        this.success = success;
        this.message = message;
        this.reports = new ArrayList<>();
    }

    public static ConversionResponse success(String message, String projectName, ConversionResult result) {
        ConversionResponse response = new ConversionResponse(true, message);
        response.setProjectName(projectName);
        if (result != null && result.getFileReports() != null) {
            List<ConversionReportSummary> summaries = new ArrayList<>();
            for (ConversionResult.FileConversionReport fileReport : result.getFileReports()) {
                summaries.add(ConversionReportSummary.from(fileReport.getFileName(), fileReport.getReport()));
            }
            response.setReports(summaries);
        }
        return response;
    }

    public static ConversionResponse error(String message) {
        return new ConversionResponse(false, message);
    }

    // Getters and setters
    public boolean isSuccess() { return success; }
    public void setSuccess(boolean success) { this.success = success; }
    
    public String getMessage() { return message; }
    public void setMessage(String message) { this.message = message; }
    
    public String getProjectName() { return projectName; }
    public void setProjectName(String projectName) { this.projectName = projectName; }
    
    public String getDownloadUrl() { return downloadUrl; }
    public void setDownloadUrl(String downloadUrl) { this.downloadUrl = downloadUrl; }
    
    public String getZipFileBase64() { return zipFileBase64; }
    public void setZipFileBase64(String zipFileBase64) { this.zipFileBase64 = zipFileBase64; }
    
    public List<ConversionReportSummary> getReports() { return reports; }
    public void setReports(List<ConversionReportSummary> reports) { this.reports = reports; }

    /**
     * Summary of conversion report for web display (per file)
     */
    public static class ConversionReportSummary {
        private String fileName;
        private String programName;
        private int totalStatements;
        private int convertedStatements;
        private int partiallyConvertedStatements;
        private int unconvertedStatements;
        private double conversionPercentage;
        private double partialConversionPercentage;
        private double failurePercentage;
        private String confidenceLevel;
        private String confidenceIcon;
        private String confidenceDescription;
        private int totalDataItems;
        private int convertedDataItems;
        private int unconvertedDataItems;

        public static ConversionReportSummary from(String fileName, ConversionReport report) {
            ConversionReportSummary summary = new ConversionReportSummary();
            summary.setFileName(fileName);
            summary.setProgramName(report.getProgramName());
            summary.setTotalStatements(report.getTotalStatements());
            summary.setConvertedStatements(report.getConvertedStatements());
            summary.setPartiallyConvertedStatements(report.getPartiallyConvertedStatements());
            summary.setUnconvertedStatements(report.getUnconvertedStatements());
            summary.setConversionPercentage(report.getConversionPercentage());
            summary.setPartialConversionPercentage(report.getPartialConversionPercentage());
            summary.setFailurePercentage(report.getFailurePercentage());
            
            if (report.getOverallConfidence() != null) {
                summary.setConfidenceLevel(report.getOverallConfidence().getLabel());
                summary.setConfidenceIcon(report.getOverallConfidence().getIcon());
                summary.setConfidenceDescription(report.getOverallConfidence().getDescription());
            }
            
            summary.setTotalDataItems(report.getTotalDataItems());
            summary.setConvertedDataItems(report.getConvertedDataItems());
            summary.setUnconvertedDataItems(report.getUnconvertedDataItems());
            
            return summary;
        }

        // Getters and setters
        public String getFileName() { return fileName; }
        public void setFileName(String fileName) { this.fileName = fileName; }
        
        public String getProgramName() { return programName; }
        public void setProgramName(String programName) { this.programName = programName; }
        
        public int getTotalStatements() { return totalStatements; }
        public void setTotalStatements(int totalStatements) { this.totalStatements = totalStatements; }
        
        public int getConvertedStatements() { return convertedStatements; }
        public void setConvertedStatements(int convertedStatements) { this.convertedStatements = convertedStatements; }
        
        public int getPartiallyConvertedStatements() { return partiallyConvertedStatements; }
        public void setPartiallyConvertedStatements(int partiallyConvertedStatements) { 
            this.partiallyConvertedStatements = partiallyConvertedStatements; 
        }
        
        public int getUnconvertedStatements() { return unconvertedStatements; }
        public void setUnconvertedStatements(int unconvertedStatements) { 
            this.unconvertedStatements = unconvertedStatements; 
        }
        
        public double getConversionPercentage() { return conversionPercentage; }
        public void setConversionPercentage(double conversionPercentage) { 
            this.conversionPercentage = conversionPercentage; 
        }
        
        public double getPartialConversionPercentage() { return partialConversionPercentage; }
        public void setPartialConversionPercentage(double partialConversionPercentage) { 
            this.partialConversionPercentage = partialConversionPercentage; 
        }
        
        public double getFailurePercentage() { return failurePercentage; }
        public void setFailurePercentage(double failurePercentage) { 
            this.failurePercentage = failurePercentage; 
        }
        
        public String getConfidenceLevel() { return confidenceLevel; }
        public void setConfidenceLevel(String confidenceLevel) { this.confidenceLevel = confidenceLevel; }
        
        public String getConfidenceIcon() { return confidenceIcon; }
        public void setConfidenceIcon(String confidenceIcon) { this.confidenceIcon = confidenceIcon; }
        
        public String getConfidenceDescription() { return confidenceDescription; }
        public void setConfidenceDescription(String confidenceDescription) { 
            this.confidenceDescription = confidenceDescription; 
        }
        
        public int getTotalDataItems() { return totalDataItems; }
        public void setTotalDataItems(int totalDataItems) { this.totalDataItems = totalDataItems; }
        
        public int getConvertedDataItems() { return convertedDataItems; }
        public void setConvertedDataItems(int convertedDataItems) { this.convertedDataItems = convertedDataItems; }
        
        public int getUnconvertedDataItems() { return unconvertedDataItems; }
        public void setUnconvertedDataItems(int unconvertedDataItems) { 
            this.unconvertedDataItems = unconvertedDataItems; 
        }
    }
}
