package com.cobol.translator.jcl.translator;

import com.cobol.translator.jcl.model.JCLStep;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

/**
 * Handles JCL PROC (procedures) translation to Spring Batch reusable configurations.
 *
 * JCL PROC Features:
 * - Reusable step sequences
 * - Symbolic parameters (&PARM substitution)
 * - PROC libraries (JCLLIB, PROCLIB)
 * - Override parameters at invocation
 *
 * Spring Batch Equivalent:
 * - @Configuration classes with @Bean factory methods
 * - Parameterized Step/Job definitions
 * - Job/Step inheritance
 */
public class JCLProcedureHandler {

    private static final Logger logger = LoggerFactory.getLogger(JCLProcedureHandler.class);

    /**
     * Represents a JCL PROC definition
     */
    public static class ProcDefinition {
        private String procName;
        private Map<String, String> symbolicParameters = new LinkedHashMap<>();
        private List<JCLStep> steps = new ArrayList<>();
        private String library;

        public String getProcName() { return procName; }
        public void setProcName(String procName) { this.procName = procName; }

        public Map<String, String> getSymbolicParameters() { return symbolicParameters; }
        public void setSymbolicParameters(Map<String, String> symbolicParameters) {
            this.symbolicParameters = symbolicParameters;
        }

        public List<JCLStep> getSteps() { return steps; }
        public void setSteps(List<JCLStep> steps) { this.steps = steps; }

        public String getLibrary() { return library; }
        public void setLibrary(String library) { this.library = library; }

        public void addSymbolicParameter(String name, String defaultValue) {
            symbolicParameters.put(name, defaultValue);
        }

        public void addStep(JCLStep step) {
            steps.add(step);
        }
    }

    /**
     * Represents a PROC invocation with overrides
     */
    public static class ProcInvocation {
        private String procName;
        private Map<String, String> overrideParameters = new HashMap<>();
        private Map<String, String> ddOverrides = new HashMap<>();

        public String getProcName() { return procName; }
        public void setProcName(String procName) { this.procName = procName; }

        public Map<String, String> getOverrideParameters() { return overrideParameters; }
        public void setOverrideParameters(Map<String, String> overrideParameters) {
            this.overrideParameters = overrideParameters;
        }

        public Map<String, String> getDdOverrides() { return ddOverrides; }
        public void setDdOverrides(Map<String, String> ddOverrides) {
            this.ddOverrides = ddOverrides;
        }
    }

    /**
     * Parses PROC definition from JCL
     */
    public ProcDefinition parseProcDefinition(String procName, List<String> jclLines) {
        ProcDefinition proc = new ProcDefinition();
        proc.setProcName(procName);

        logger.debug("Parsing PROC definition: {}", procName);

        for (String line : jclLines) {
            String trimmed = line.trim();

            // Parse symbolic parameters: //PROC01 PROC DSN=&HLQ..DATA
            if (trimmed.contains("PROC") && trimmed.contains("&")) {
                parseSymbolicParameters(trimmed, proc);
            }

            // Parse PROC steps
            if (trimmed.startsWith("//") && !trimmed.startsWith("//*") &&
                trimmed.contains("EXEC")) {
                // This is a step within the PROC
                JCLStep step = parseStepFromLine(trimmed);
                if (step != null) {
                    proc.addStep(step);
                }
            }
        }

        logger.info("Parsed PROC {} with {} symbolic parameters and {} steps",
                   procName, proc.getSymbolicParameters().size(), proc.getSteps().size());

        return proc;
    }

    /**
     * Generates Spring Batch configuration class for a PROC
     */
    public String generateProcConfiguration(ProcDefinition proc, String packageName) {
        StringBuilder code = new StringBuilder();

        String configClassName = toPascalCase(proc.getProcName()) + "ProcConfiguration";

        code.append("package ").append(packageName).append(";\n\n");

        // Imports
        code.append("import org.springframework.batch.core.Step;\n");
        code.append("import org.springframework.batch.core.repository.JobRepository;\n");
        code.append("import org.springframework.batch.core.step.builder.StepBuilder;\n");
        code.append("import org.springframework.context.annotation.Bean;\n");
        code.append("import org.springframework.context.annotation.Configuration;\n");
        code.append("import org.springframework.transaction.PlatformTransactionManager;\n");
        code.append("import org.springframework.beans.factory.annotation.Value;\n");
        code.append("import org.slf4j.Logger;\n");
        code.append("import org.slf4j.LoggerFactory;\n\n");

        code.append("/**\n");
        code.append(" * Spring Batch configuration for JCL PROC: ").append(proc.getProcName()).append("\n");
        code.append(" *\n");
        code.append(" * Symbolic Parameters:\n");
        for (Map.Entry<String, String> param : proc.getSymbolicParameters().entrySet()) {
            code.append(" * - ").append(param.getKey()).append(" (default: ").append(param.getValue()).append(")\n");
        }
        code.append(" *\n");
        code.append(" * Steps: ").append(proc.getSteps().size()).append("\n");
        code.append(" */\n");
        code.append("@Configuration\n");
        code.append("public class ").append(configClassName).append(" {\n\n");

        code.append("    private static final Logger logger = LoggerFactory.getLogger(")
            .append(configClassName).append(".class);\n\n");

        // Generate @Value fields for symbolic parameters
        for (Map.Entry<String, String> param : proc.getSymbolicParameters().entrySet()) {
            String paramName = param.getKey().replace("&", "").toLowerCase();
            String defaultValue = param.getValue() != null ? param.getValue() : "";

            code.append("    @Value(\"${jcl.proc.").append(proc.getProcName().toLowerCase()).append(".")
                .append(paramName).append(":").append(defaultValue).append("}\")\n");
            code.append("    private String ").append(paramName).append(";\n\n");
        }

        // Generate factory method for the PROC
        code.append("    /**\n");
        code.append("     * Creates a parameterized step sequence for PROC ").append(proc.getProcName()).append("\n");
        code.append("     */\n");
        code.append("    public List<Step> create").append(toPascalCase(proc.getProcName())).append("Steps(\n");
        code.append("            JobRepository jobRepository,\n");
        code.append("            PlatformTransactionManager transactionManager");

        // Add overridable parameters
        for (String param : proc.getSymbolicParameters().keySet()) {
            String paramName = param.replace("&", "").toLowerCase();
            code.append(",\n            String ").append(paramName).append("Override");
        }

        code.append(") {\n\n");
        code.append("        List<Step> steps = new ArrayList<>();\n\n");

        // Resolve parameters (use override if provided, else default)
        for (String param : proc.getSymbolicParameters().keySet()) {
            String paramName = param.replace("&", "").toLowerCase();
            code.append("        String resolved").append(toPascalCase(paramName)).append(" = ")
                .append(paramName).append("Override != null ? ")
                .append(paramName).append("Override : this.").append(paramName).append(";\n");
        }
        code.append("\n");

        code.append("        logger.info(\"Creating PROC ").append(proc.getProcName()).append(" steps\");\n\n");

        // Generate each step
        for (int i = 0; i < proc.getSteps().size(); i++) {
            JCLStep step = proc.getSteps().get(i);
            String stepMethodName = sanitizeStepName(step.getStepName()) + "Step";

            code.append("        // PROC Step ").append(i + 1).append(": ").append(step.getStepName()).append("\n");
            code.append("        steps.add(").append(stepMethodName).append("(\n");
            code.append("            jobRepository,\n");
            code.append("            transactionManager");

            // Pass resolved parameters
            for (String param : proc.getSymbolicParameters().keySet()) {
                String paramName = param.replace("&", "").toLowerCase();
                code.append(",\n            resolved").append(toPascalCase(paramName));
            }

            code.append("));\n\n");
        }

        code.append("        return steps;\n");
        code.append("    }\n\n");

        // Generate individual step methods
        for (JCLStep step : proc.getSteps()) {
            code.append(generateProcStepMethod(step, proc));
        }

        code.append("}\n");

        return code.toString();
    }

    private String generateProcStepMethod(JCLStep step, ProcDefinition proc) {
        StringBuilder code = new StringBuilder();

        String stepMethodName = sanitizeStepName(step.getStepName()) + "Step";

        code.append("    /**\n");
        code.append("     * PROC Step: ").append(step.getStepName()).append("\n");
        code.append("     * Program: ").append(step.getProgramName() != null ? step.getProgramName() : "N/A").append("\n");
        code.append("     */\n");
        code.append("    private Step ").append(stepMethodName).append("(\n");
        code.append("            JobRepository jobRepository,\n");
        code.append("            PlatformTransactionManager transactionManager");

        // Add parameters
        for (String param : proc.getSymbolicParameters().keySet()) {
            String paramName = param.replace("&", "").toLowerCase();
            code.append(",\n            String ").append(paramName);
        }

        code.append(") {\n\n");

        code.append("        return new StepBuilder(\"").append(step.getStepName()).append("\", jobRepository)\n");

        if (step.getProgramName() != null) {
            code.append("            .tasklet((contribution, chunkContext) -> {\n");
            code.append("                logger.info(\"Executing PROC step: ").append(step.getStepName()).append("\");\n");
            code.append("                logger.debug(\"Program: ").append(step.getProgramName()).append("\");\n");

            // Log resolved parameters
            for (String param : proc.getSymbolicParameters().keySet()) {
                String paramName = param.replace("&", "").toLowerCase();
                code.append("                logger.debug(\"Parameter ").append(param).append(" = {}\", ")
                    .append(paramName).append(");\n");
            }

            code.append("                // TODO: Implement ").append(step.getProgramName()).append(" logic\n");
            code.append("                return RepeatStatus.FINISHED;\n");
            code.append("            }, transactionManager)\n");
        }

        code.append("            .build();\n");
        code.append("    }\n\n");

        return code.toString();
    }

    /**
     * Generates invocation code for calling a PROC
     */
    public String generateProcInvocation(ProcInvocation invocation, ProcDefinition procDef) {
        StringBuilder code = new StringBuilder();

        code.append("        // JCL PROC invocation: ").append(invocation.getProcName()).append("\n");
        code.append("        List<Step> ").append(sanitizeStepName(invocation.getProcName())).append("Steps = \n");
        code.append("            ").append(toPascalCase(invocation.getProcName())).append("ProcConfiguration.create")
            .append(toPascalCase(invocation.getProcName())).append("Steps(\n");
        code.append("                jobRepository,\n");
        code.append("                transactionManager");

        // Pass override parameters
        for (String param : procDef.getSymbolicParameters().keySet()) {
            String paramName = param.replace("&", "");
            String overrideValue = invocation.getOverrideParameters().get(paramName);

            code.append(",\n                ");
            if (overrideValue != null) {
                code.append("\"").append(overrideValue).append("\"");
            } else {
                code.append("null");
            }
        }

        code.append(");\n\n");

        code.append("        // Add PROC steps to flow\n");
        code.append("        for (Step step : ").append(sanitizeStepName(invocation.getProcName())).append("Steps) {\n");
        code.append("            // Add to job flow\n");
        code.append("        }\n");

        return code.toString();
    }

    private void parseSymbolicParameters(String line, ProcDefinition proc) {
        // Parse symbolic parameters like: DSN=&HLQ..DATA or &REGION=4M
        String[] tokens = line.split("[,\\s]+");

        for (String token : tokens) {
            if (token.contains("=") && token.contains("&")) {
                String[] parts = token.split("=");
                if (parts.length == 2) {
                    String paramName = parts[0].trim();
                    String defaultValue = parts[1].trim();

                    if (paramName.startsWith("&")) {
                        proc.addSymbolicParameter(paramName, defaultValue);
                    } else if (defaultValue.contains("&")) {
                        // DSN=&HLQ format
                        proc.addSymbolicParameter(defaultValue, "");
                    }
                }
            }
        }
    }

    private JCLStep parseStepFromLine(String line) {
        // Simple parsing - in reality would use full JCL parser
        JCLStep step = new JCLStep();

        // Extract step name from //STEP01 EXEC PGM=PROG01
        String[] parts = line.split("\\s+");
        if (parts.length > 0) {
            String stepName = parts[0].replaceAll("^//", "").trim();
            step.setStepName(stepName);
        }

        // Extract program name
        for (String part : parts) {
            if (part.startsWith("PGM=")) {
                String pgm = part.substring(4).trim();
                step.setProgramName(pgm);
                break;
            }
        }

        return step;
    }

    private String toPascalCase(String input) {
        if (input == null || input.isEmpty()) return "";

        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = true;

        for (char c : input.toCharArray()) {
            if (Character.isLetterOrDigit(c)) {
                if (capitalizeNext) {
                    result.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else {
                    result.append(Character.toLowerCase(c));
                }
            } else {
                capitalizeNext = true;
            }
        }

        return result.toString();
    }

    private String sanitizeStepName(String stepName) {
        return stepName.toLowerCase().replaceAll("[^a-z0-9]", "");
    }
}
