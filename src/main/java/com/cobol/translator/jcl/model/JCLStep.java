package com.cobol.translator.jcl.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a JCL Step
 */
public class JCLStep {

    private String stepName;
    private String programName;
    private String procedureName;
    private List<JCLParameter> execParameters;
    private List<DDStatement> ddStatements;

    public JCLStep() {
        this.execParameters = new ArrayList<>();
        this.ddStatements = new ArrayList<>();
    }

    public JCLStep(String stepName) {
        this();
        this.stepName = stepName;
    }

    public void addExecParameter(JCLParameter parameter) {
        this.execParameters.add(parameter);
    }

    public void addDDStatement(DDStatement ddStatement) {
        this.ddStatements.add(ddStatement);
    }

    public DDStatement getDDStatement(String ddName) {
        return ddStatements.stream()
                .filter(dd -> dd.getDdName().equalsIgnoreCase(ddName))
                .findFirst()
                .orElse(null);
    }

    // Getters and Setters

    public String getStepName() {
        return stepName;
    }

    public void setStepName(String stepName) {
        this.stepName = stepName;
    }

    public String getProgramName() {
        return programName;
    }

    public void setProgramName(String programName) {
        this.programName = programName;
    }

    public String getProcedureName() {
        return procedureName;
    }

    public void setProcedureName(String procedureName) {
        this.procedureName = procedureName;
    }

    public List<JCLParameter> getExecParameters() {
        return execParameters;
    }

    public void setExecParameters(List<JCLParameter> execParameters) {
        this.execParameters = execParameters;
    }

    public List<DDStatement> getDdStatements() {
        return ddStatements;
    }

    public void setDdStatements(List<DDStatement> ddStatements) {
        this.ddStatements = ddStatements;
    }

    @Override
    public String toString() {
        return "JCLStep{" +
                "stepName='" + stepName + '\'' +
                ", programName='" + programName + '\'' +
                ", ddStatements=" + ddStatements.size() +
                '}';
    }
}
