package com.cobol.translator.jcl.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a JCL Job
 */
public class JCLJob {

    private String jobName;
    private String accountInfo;
    private List<JCLParameter> jobParameters;
    private List<JCLStep> steps;

    public JCLJob() {
        this.jobParameters = new ArrayList<>();
        this.steps = new ArrayList<>();
    }

    public JCLJob(String jobName) {
        this();
        this.jobName = jobName;
    }

    public void addStep(JCLStep step) {
        this.steps.add(step);
    }

    public void addJobParameter(JCLParameter parameter) {
        this.jobParameters.add(parameter);
    }

    // Getters and Setters

    public String getJobName() {
        return jobName;
    }

    public void setJobName(String jobName) {
        this.jobName = jobName;
    }

    public String getAccountInfo() {
        return accountInfo;
    }

    public void setAccountInfo(String accountInfo) {
        this.accountInfo = accountInfo;
    }

    public List<JCLParameter> getJobParameters() {
        return jobParameters;
    }

    public void setJobParameters(List<JCLParameter> jobParameters) {
        this.jobParameters = jobParameters;
    }

    public List<JCLStep> getSteps() {
        return steps;
    }

    public void setSteps(List<JCLStep> steps) {
        this.steps = steps;
    }

    @Override
    public String toString() {
        return "JCLJob{" +
                "jobName='" + jobName + '\'' +
                ", accountInfo='" + accountInfo + '\'' +
                ", steps=" + steps.size() +
                '}';
    }
}
