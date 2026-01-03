package com.cobol.translator.jcl.parser;

import com.cobol.translator.grammar.JCLBaseVisitor;
import com.cobol.translator.grammar.JCLParser.*;
import com.cobol.translator.jcl.model.*;
import com.cobol.translator.jcl.model.DDStatement.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * ANTLR4 Visitor to build JCL AST model
 */
public class JCLASTBuilder extends JCLBaseVisitor<Object> {

    private static final Logger logger = LoggerFactory.getLogger(JCLASTBuilder.class);

    @Override
    public JCLJob visitJclFile(JclFileContext ctx) {
        JCLJob job = null;

        // Visit all statements
        for (StatementContext stmtCtx : ctx.statement()) {
            if (stmtCtx.jobStatement() != null) {
                job = (JCLJob) visit(stmtCtx.jobStatement());
            } else if (stmtCtx.stepStatement() != null && job != null) {
                JCLStep step = (JCLStep) visit(stmtCtx.stepStatement());
                if (step != null) {
                    job.addStep(step);
                }
            }
            // commentLine statements are ignored (just skip them)
        }

        if (job == null) {
            throw new IllegalStateException("No JOB statement found in JCL file");
        }

        return job;
    }

    @Override
    public JCLJob visitJobStatement(JobStatementContext ctx) {
        String jobName = ctx.jobName().getText();
        JCLJob job = new JCLJob(jobName);

        if (ctx.accountInfo() != null) {
            job.setAccountInfo(ctx.accountInfo().getText());
        }

        // Add job parameters
        if (ctx.jobParameters() != null) {
            for (JobParametersContext paramCtx : ctx.jobParameters()) {
                JCLParameter param = (JCLParameter) visit(paramCtx);
                if (param != null) {
                    job.addJobParameter(param);
                }
            }
        }

        return job;
    }

    @Override
    public JCLStep visitStepStatement(StepStatementContext ctx) {
        String stepName = ctx.stepName().getText();
        JCLStep step = new JCLStep(stepName);

        // Visit PGM or PROC exec
        if (ctx.pgmExec() != null) {
            step.setProgramName(ctx.pgmExec().programName().getText());
        } else if (ctx.procExec() != null) {
            step.setProcedureName(ctx.procExec().procName().getText());
        }

        // Add exec parameters
        if (ctx.execParameter() != null) {
            for (ExecParameterContext paramCtx : ctx.execParameter()) {
                JCLParameter param = (JCLParameter) visit(paramCtx);
                if (param != null) {
                    step.addExecParameter(param);
                }
            }
        }

        // Visit DD statements
        if (ctx.ddStatements() != null) {
            for (DdStatementsContext ddCtx : ctx.ddStatements()) {
                DDStatement dd = (DDStatement) visit(ddCtx);
                if (dd != null) {
                    step.addDDStatement(dd);
                }
            }
        }

        return step;
    }

    @Override
    public DDStatement visitDdStatement(DdStatementContext ctx) {
        String ddName = ctx.ddName().getText();
        DDStatement dd = new DDStatement(ddName);

        if (ctx.ddParameters() != null) {
            visitDdParameters(ctx.ddParameters(), dd);
        }

        return dd;
    }

    @Override
    public DDStatement visitDdDummy(DdDummyContext ctx) {
        String ddName = ctx.ddName().getText();
        DDStatement dd = new DDStatement(ddName);
        dd.setDummy(true);
        return dd;
    }

    private void visitDdParameters(DdParametersContext ctx, DDStatement dd) {
        for (DdParameterContext paramCtx : ctx.ddParameter()) {

            // DSN parameter
            if (paramCtx.dsnParameter() != null) {
                String dsn = paramCtx.dsnParameter().datasetName().getText();
                dd.setDatasetName(stripQuotes(dsn));
            }

            // DISP parameter
            else if (paramCtx.dispParameter() != null) {
                Disposition disp = parseDisposition(paramCtx.dispParameter());
                dd.setDisposition(disp);
            }

            // DCB parameter
            else if (paramCtx.dcbParameter() != null) {
                parseDCB(paramCtx.dcbParameter(), dd.getDcbInfo());
            }

            // SPACE parameter
            else if (paramCtx.spaceParameter() != null) {
                SpaceAllocation space = parseSpace(paramCtx.spaceParameter());
                dd.setSpaceAllocation(space);
            }

            // UNIT parameter
            else if (paramCtx.unitParameter() != null) {
                dd.setUnit(paramCtx.unitParameter().unitValue().getText());
            }

            // SYSOUT parameter
            else if (paramCtx.sysoutParameter() != null) {
                dd.setSysoutClass(paramCtx.sysoutParameter().sysoutClass().getText());
            }

            // RECFM parameter
            else if (paramCtx.recfmParameter() != null) {
                dd.getDcbInfo().setRecfm(paramCtx.recfmParameter().recfmValue().getText());
            }

            // LRECL parameter
            else if (paramCtx.lreclParameter() != null) {
                String lrecl = paramCtx.lreclParameter().number().getText();
                dd.getDcbInfo().setLrecl(Integer.parseInt(lrecl));
            }

            // BLKSIZE parameter
            else if (paramCtx.blksizeParameter() != null) {
                String blksize = paramCtx.blksizeParameter().number().getText();
                dd.getDcbInfo().setBlksize(Integer.parseInt(blksize));
            }

            // Other parameters
            else if (paramCtx.otherParameter() != null) {
                String name = paramCtx.otherParameter().paramName().getText();
                String value = paramCtx.otherParameter().paramValue().getText();
                dd.addOtherParameter(new JCLParameter(name, value));
            }
        }
    }

    private Disposition parseDisposition(DispParameterContext ctx) {
        Disposition disp = new Disposition();

        String status = ctx.disposition().getText();
        disp.setStatus(DispositionStatus.valueOf(status));

        if (ctx.normalDisp() != null) {
            String normalDisp = ctx.normalDisp().getText();
            disp.setNormalEnd(DispositionAction.valueOf(normalDisp));
        }

        if (ctx.abnormalDisp() != null) {
            String abnormalDisp = ctx.abnormalDisp().getText();
            disp.setAbnormalEnd(DispositionAction.valueOf(abnormalDisp));
        }

        return disp;
    }

    private void parseDCB(DcbParameterContext ctx, DCBInfo dcbInfo) {
        for (DcbSubParameterContext subParam : ctx.dcbSubParameter()) {
            if (subParam.RECFM() != null) {
                dcbInfo.setRecfm(subParam.recfmValue().getText());
            } else if (subParam.LRECL() != null) {
                dcbInfo.setLrecl(Integer.parseInt(subParam.number().getText()));
            } else if (subParam.BLKSIZE() != null) {
                dcbInfo.setBlksize(Integer.parseInt(subParam.number().getText()));
            } else if (subParam.DSORG() != null) {
                dcbInfo.setDsorg(subParam.dsorgValue().getText());
            }
        }
    }

    private SpaceAllocation parseSpace(SpaceParameterContext ctx) {
        SpaceAllocation space = new SpaceAllocation();

        space.setUnit(ctx.spaceUnit().getText());
        space.setPrimary(Integer.parseInt(ctx.primary().number().getText()));

        if (ctx.secondary() != null) {
            space.setSecondary(Integer.parseInt(ctx.secondary().number().getText()));
        }

        if (ctx.directory() != null) {
            space.setDirectory(Integer.parseInt(ctx.directory().number().getText()));
        }

        return space;
    }

    @Override
    public JCLParameter visitJobParameters(JobParametersContext ctx) {
        String name = ctx.IDENTIFIER(0).getText();
        String value = getParameterValue(ctx);
        return new JCLParameter(name, value);
    }

    @Override
    public JCLParameter visitExecParameter(ExecParameterContext ctx) {
        String name = ctx.IDENTIFIER(0).getText();
        String value = getParameterValue(ctx);
        return new JCLParameter(name, value);
    }

    private String getParameterValue(Object ctx) {
        if (ctx instanceof JobParametersContext) {
            JobParametersContext jpc = (JobParametersContext) ctx;
            if (jpc.STRING() != null) {
                return stripQuotes(jpc.STRING().getText());
            } else if (jpc.IDENTIFIER(1) != null) {
                return jpc.IDENTIFIER(1).getText();
            } else if (jpc.number() != null) {
                return jpc.number().getText();
            }
        } else if (ctx instanceof ExecParameterContext) {
            ExecParameterContext epc = (ExecParameterContext) ctx;
            if (epc.STRING() != null) {
                return stripQuotes(epc.STRING().getText());
            } else if (epc.IDENTIFIER(1) != null) {
                return epc.IDENTIFIER(1).getText();
            } else if (epc.number() != null) {
                return epc.number().getText();
            }
        }
        return "";
    }

    private String stripQuotes(String str) {
        if (str != null && str.length() >= 2 &&
            str.startsWith("'") && str.endsWith("'")) {
            return str.substring(1, str.length() - 1);
        }
        return str;
    }
}
