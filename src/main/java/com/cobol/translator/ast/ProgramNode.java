package com.cobol.translator.ast;

/**
 * AST node representing a complete COBOL program.
 * Root node of the Abstract Syntax Tree.
 */
public class ProgramNode extends ASTNode {

    private String programName;
    private String sourceFile;
    private IdentificationDivisionNode identificationDivision;
    private EnvironmentDivisionNode environmentDivision;
    private DataDivisionNode dataDivision;
    private ProcedureDivisionNode procedureDivision;

    public ProgramNode(String programName) {
        super();
        this.programName = programName;
    }

    @Override
    public String getNodeType() {
        return "Program";
    }

    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitProgramNode(this);
    }

    // Getters and setters

    public String getProgramName() {
        return programName;
    }

    public void setProgramName(String programName) {
        this.programName = programName;
    }

    public IdentificationDivisionNode getIdentificationDivision() {
        return identificationDivision;
    }

    public void setIdentificationDivision(IdentificationDivisionNode identificationDivision) {
        this.identificationDivision = identificationDivision;
        addChild(identificationDivision);
    }

    public EnvironmentDivisionNode getEnvironmentDivision() {
        return environmentDivision;
    }

    public void setEnvironmentDivision(EnvironmentDivisionNode environmentDivision) {
        this.environmentDivision = environmentDivision;
        addChild(environmentDivision);
    }

    public DataDivisionNode getDataDivision() {
        return dataDivision;
    }

    public void setDataDivision(DataDivisionNode dataDivision) {
        this.dataDivision = dataDivision;
        addChild(dataDivision);
    }

    public ProcedureDivisionNode getProcedureDivision() {
        return procedureDivision;
    }

    public void setProcedureDivision(ProcedureDivisionNode procedureDivision) {
        this.procedureDivision = procedureDivision;
        addChild(procedureDivision);
    }

    public String getSourceFile() {
        return sourceFile;
    }

    public void setSourceFile(String sourceFile) {
        this.sourceFile = sourceFile;
    }

    @Override
    public String toString() {
        return String.format("Program: %s [%s]", programName, getLocation());
    }
}
