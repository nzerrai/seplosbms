package com.cobol.translator.jcl.translator;

import com.cobol.translator.jcl.model.JCLStep;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Translates JCL conditional execution (IF/THEN/ELSE) to Spring Batch decision logic.
 *
 * JCL Conditions:
 * - IF (condition) THEN
 * - IF RC = 0 THEN
 * - IF ABEND THEN
 * - IF NOT condition THEN
 *
 * Spring Batch Equivalent:
 * - JobExecutionDecider
 * - Step execution status checks
 * - Conditional flow configuration
 */
public class JCLConditionTranslator {

    private static final Logger logger = LoggerFactory.getLogger(JCLConditionTranslator.class);

    /**
     * Represents a JCL conditional block
     */
    public static class ConditionalBlock {
        private String condition;
        private List<JCLStep> thenSteps = new ArrayList<>();
        private List<JCLStep> elseSteps = new ArrayList<>();
        private ConditionType type;

        public enum ConditionType {
            RETURN_CODE,    // IF RC = 0
            ABEND_CHECK,    // IF ABEND
            STEP_SUCCESS,   // IF stepname.RC = 0
            NOT_CONDITION   // IF NOT ...
        }

        public String getCondition() { return condition; }
        public void setCondition(String condition) { this.condition = condition; }

        public List<JCLStep> getThenSteps() { return thenSteps; }
        public void setThenSteps(List<JCLStep> thenSteps) { this.thenSteps = thenSteps; }

        public List<JCLStep> getElseSteps() { return elseSteps; }
        public void setElseSteps(List<JCLStep> elseSteps) { this.elseSteps = elseSteps; }

        public ConditionType getType() { return type; }
        public void setType(ConditionType type) { this.type = type; }
    }

    /**
     * Parses JCL condition into structured form
     */
    public ConditionalBlock parseCondition(String jclCondition) {
        ConditionalBlock block = new ConditionalBlock();
        block.setCondition(jclCondition);

        String upperCondition = jclCondition.toUpperCase().trim();

        // Determine condition type
        if (upperCondition.contains("RC")) {
            block.setType(ConditionalBlock.ConditionType.RETURN_CODE);
        } else if (upperCondition.contains("ABEND")) {
            block.setType(ConditionalBlock.ConditionType.ABEND_CHECK);
        } else if (upperCondition.startsWith("NOT")) {
            block.setType(ConditionalBlock.ConditionType.NOT_CONDITION);
        } else if (upperCondition.contains(".RC")) {
            block.setType(ConditionalBlock.ConditionType.STEP_SUCCESS);
        } else {
            block.setType(ConditionalBlock.ConditionType.RETURN_CODE);
        }

        logger.debug("Parsed JCL condition: {} -> {}", jclCondition, block.getType());
        return block;
    }

    /**
     * Generates Spring Batch JobExecutionDecider for JCL conditional logic
     */
    public String generateDeciderCode(ConditionalBlock block, String packageName) {
        StringBuilder code = new StringBuilder();

        String deciderName = sanitizeConditionName(block.getCondition()) + "Decider";

        code.append("package ").append(packageName).append(";\n\n");
        code.append("import org.springframework.batch.core.JobExecution;\n");
        code.append("import org.springframework.batch.core.StepExecution;\n");
        code.append("import org.springframework.batch.core.job.flow.FlowExecutionStatus;\n");
        code.append("import org.springframework.batch.core.job.flow.JobExecutionDecider;\n");
        code.append("import org.springframework.stereotype.Component;\n");
        code.append("import org.slf4j.Logger;\n");
        code.append("import org.slf4j.LoggerFactory;\n\n");

        code.append("/**\n");
        code.append(" * Decider for JCL condition: ").append(block.getCondition()).append("\n");
        code.append(" * Type: ").append(block.getType()).append("\n");
        code.append(" */\n");
        code.append("@Component\n");
        code.append("public class ").append(deciderName).append(" implements JobExecutionDecider {\n\n");

        code.append("    private static final Logger logger = LoggerFactory.getLogger(")
            .append(deciderName).append(".class);\n\n");

        code.append("    @Override\n");
        code.append("    public FlowExecutionStatus decide(JobExecution jobExecution, StepExecution stepExecution) {\n");
        code.append("        logger.debug(\"Evaluating JCL condition: ").append(block.getCondition()).append("\");\n\n");

        // Generate condition logic based on type
        switch (block.getType()) {
            case RETURN_CODE:
                code.append(generateReturnCodeCheck(block));
                break;
            case ABEND_CHECK:
                code.append(generateAbendCheck());
                break;
            case STEP_SUCCESS:
                code.append(generateStepSuccessCheck(block));
                break;
            case NOT_CONDITION:
                code.append(generateNotConditionCheck(block));
                break;
        }

        code.append("    }\n");
        code.append("}\n");

        return code.toString();
    }

    private String generateReturnCodeCheck(ConditionalBlock block) {
        StringBuilder code = new StringBuilder();

        code.append("        // JCL: ").append(block.getCondition()).append("\n");
        code.append("        if (stepExecution != null) {\n");
        code.append("            String exitCode = stepExecution.getExitStatus().getExitCode();\n");
        code.append("            logger.debug(\"Step exit code: {}\", exitCode);\n\n");

        // Extract expected return code from condition
        String condition = block.getCondition().toUpperCase();
        if (condition.contains("RC = 0") || condition.contains("RC=0")) {
            code.append("            if (\"COMPLETED\".equals(exitCode)) {\n");
            code.append("                logger.debug(\"Condition satisfied: step completed successfully\");\n");
            code.append("                return new FlowExecutionStatus(\"THEN\");\n");
            code.append("            }\n");
        } else if (condition.contains("RC > 0")) {
            code.append("            if (!\"COMPLETED\".equals(exitCode)) {\n");
            code.append("                logger.debug(\"Condition satisfied: step failed\");\n");
            code.append("                return new FlowExecutionStatus(\"THEN\");\n");
            code.append("            }\n");
        }

        code.append("        }\n\n");
        code.append("        logger.debug(\"Condition not satisfied\");\n");
        code.append("        return new FlowExecutionStatus(\"ELSE\");\n");

        return code.toString();
    }

    private String generateAbendCheck() {
        StringBuilder code = new StringBuilder();

        code.append("        // Check if previous step abended\n");
        code.append("        if (stepExecution != null) {\n");
        code.append("            String exitCode = stepExecution.getExitStatus().getExitCode();\n");
        code.append("            if (\"FAILED\".equals(exitCode) || exitCode.startsWith(\"ABEND\")) {\n");
        code.append("                logger.warn(\"Abend detected: {}\", exitCode);\n");
        code.append("                return new FlowExecutionStatus(\"THEN\");\n");
        code.append("            }\n");
        code.append("        }\n\n");
        code.append("        return new FlowExecutionStatus(\"ELSE\");\n");

        return code.toString();
    }

    private String generateStepSuccessCheck(ConditionalBlock block) {
        StringBuilder code = new StringBuilder();

        // Extract step name from condition like "STEP01.RC = 0"
        String stepName = extractStepName(block.getCondition());

        code.append("        // Check if step '").append(stepName).append("' succeeded\n");
        code.append("        if (jobExecution != null) {\n");
        code.append("            for (StepExecution se : jobExecution.getStepExecutions()) {\n");
        code.append("                if (\"").append(stepName).append("\".equals(se.getStepName())) {\n");
        code.append("                    String exitCode = se.getExitStatus().getExitCode();\n");
        code.append("                    if (\"COMPLETED\".equals(exitCode)) {\n");
        code.append("                        logger.debug(\"Step {} completed successfully\", \"").append(stepName).append("\");\n");
        code.append("                        return new FlowExecutionStatus(\"THEN\");\n");
        code.append("                    }\n");
        code.append("                }\n");
        code.append("            }\n");
        code.append("        }\n\n");
        code.append("        return new FlowExecutionStatus(\"ELSE\");\n");

        return code.toString();
    }

    private String generateNotConditionCheck(ConditionalBlock block) {
        StringBuilder code = new StringBuilder();

        code.append("        // NOT condition - invert result\n");
        code.append("        if (stepExecution != null) {\n");
        code.append("            String exitCode = stepExecution.getExitStatus().getExitCode();\n");
        code.append("            if (\"COMPLETED\".equals(exitCode)) {\n");
        code.append("                return new FlowExecutionStatus(\"ELSE\");\n");
        code.append("            }\n");
        code.append("        }\n\n");
        code.append("        return new FlowExecutionStatus(\"THEN\");\n");

        return code.toString();
    }

    /**
     * Generates Spring Batch flow configuration with conditional branches
     */
    public String generateConditionalFlowConfig(ConditionalBlock block, String jobName) {
        StringBuilder code = new StringBuilder();

        code.append("        // JCL Conditional: ").append(block.getCondition()).append("\n");
        code.append("        .start(").append(sanitizeStepName(block.getThenSteps().get(0).getStepName())).append("Step)\n");
        code.append("        .next(").append(sanitizeConditionName(block.getCondition())).append("Decider)\n");
        code.append("        .on(\"THEN\")\n");

        // THEN branch
        if (!block.getThenSteps().isEmpty()) {
            for (int i = 0; i < block.getThenSteps().size(); i++) {
                JCLStep step = block.getThenSteps().get(i);
                if (i == 0) {
                    code.append("            .to(").append(sanitizeStepName(step.getStepName())).append("Step)\n");
                } else {
                    code.append("            .next(").append(sanitizeStepName(step.getStepName())).append("Step)\n");
                }
            }
        }

        // ELSE branch
        if (!block.getElseSteps().isEmpty()) {
            code.append("        .from(").append(sanitizeConditionName(block.getCondition())).append("Decider)\n");
            code.append("        .on(\"ELSE\")\n");
            for (int i = 0; i < block.getElseSteps().size(); i++) {
                JCLStep step = block.getElseSteps().get(i);
                if (i == 0) {
                    code.append("            .to(").append(sanitizeStepName(step.getStepName())).append("Step)\n");
                } else {
                    code.append("            .next(").append(sanitizeStepName(step.getStepName())).append("Step)\n");
                }
            }
        }

        code.append("        .end()\n");

        return code.toString();
    }

    private String sanitizeConditionName(String condition) {
        return condition.replaceAll("[^a-zA-Z0-9]", "");
    }

    private String sanitizeStepName(String stepName) {
        return stepName.toLowerCase().replaceAll("[^a-zA-Z0-9]", "");
    }

    private String extractStepName(String condition) {
        // Extract step name from "STEP01.RC = 0"
        int dotIndex = condition.indexOf('.');
        if (dotIndex > 0) {
            return condition.substring(0, dotIndex).trim();
        }
        return "UNKNOWN";
    }
}
