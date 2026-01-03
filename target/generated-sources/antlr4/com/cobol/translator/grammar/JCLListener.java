// Generated from com/cobol/translator/grammar/JCL.g4 by ANTLR 4.13.1
package com.cobol.translator.grammar;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link JCLParser}.
 */
public interface JCLListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link JCLParser#jclFile}.
	 * @param ctx the parse tree
	 */
	void enterJclFile(JCLParser.JclFileContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#jclFile}.
	 * @param ctx the parse tree
	 */
	void exitJclFile(JCLParser.JclFileContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#statement}.
	 * @param ctx the parse tree
	 */
	void enterStatement(JCLParser.StatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#statement}.
	 * @param ctx the parse tree
	 */
	void exitStatement(JCLParser.StatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#jobStatement}.
	 * @param ctx the parse tree
	 */
	void enterJobStatement(JCLParser.JobStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#jobStatement}.
	 * @param ctx the parse tree
	 */
	void exitJobStatement(JCLParser.JobStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#stepStatement}.
	 * @param ctx the parse tree
	 */
	void enterStepStatement(JCLParser.StepStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#stepStatement}.
	 * @param ctx the parse tree
	 */
	void exitStepStatement(JCLParser.StepStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#commentLine}.
	 * @param ctx the parse tree
	 */
	void enterCommentLine(JCLParser.CommentLineContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#commentLine}.
	 * @param ctx the parse tree
	 */
	void exitCommentLine(JCLParser.CommentLineContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#pgmExec}.
	 * @param ctx the parse tree
	 */
	void enterPgmExec(JCLParser.PgmExecContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#pgmExec}.
	 * @param ctx the parse tree
	 */
	void exitPgmExec(JCLParser.PgmExecContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#procExec}.
	 * @param ctx the parse tree
	 */
	void enterProcExec(JCLParser.ProcExecContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#procExec}.
	 * @param ctx the parse tree
	 */
	void exitProcExec(JCLParser.ProcExecContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#ddStatements}.
	 * @param ctx the parse tree
	 */
	void enterDdStatements(JCLParser.DdStatementsContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#ddStatements}.
	 * @param ctx the parse tree
	 */
	void exitDdStatements(JCLParser.DdStatementsContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#ddStatement}.
	 * @param ctx the parse tree
	 */
	void enterDdStatement(JCLParser.DdStatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#ddStatement}.
	 * @param ctx the parse tree
	 */
	void exitDdStatement(JCLParser.DdStatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#ddDummy}.
	 * @param ctx the parse tree
	 */
	void enterDdDummy(JCLParser.DdDummyContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#ddDummy}.
	 * @param ctx the parse tree
	 */
	void exitDdDummy(JCLParser.DdDummyContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#ddInline}.
	 * @param ctx the parse tree
	 */
	void enterDdInline(JCLParser.DdInlineContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#ddInline}.
	 * @param ctx the parse tree
	 */
	void exitDdInline(JCLParser.DdInlineContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#ddConcat}.
	 * @param ctx the parse tree
	 */
	void enterDdConcat(JCLParser.DdConcatContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#ddConcat}.
	 * @param ctx the parse tree
	 */
	void exitDdConcat(JCLParser.DdConcatContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#ddParameters}.
	 * @param ctx the parse tree
	 */
	void enterDdParameters(JCLParser.DdParametersContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#ddParameters}.
	 * @param ctx the parse tree
	 */
	void exitDdParameters(JCLParser.DdParametersContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#ddParameter}.
	 * @param ctx the parse tree
	 */
	void enterDdParameter(JCLParser.DdParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#ddParameter}.
	 * @param ctx the parse tree
	 */
	void exitDdParameter(JCLParser.DdParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#dsnParameter}.
	 * @param ctx the parse tree
	 */
	void enterDsnParameter(JCLParser.DsnParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#dsnParameter}.
	 * @param ctx the parse tree
	 */
	void exitDsnParameter(JCLParser.DsnParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#dispParameter}.
	 * @param ctx the parse tree
	 */
	void enterDispParameter(JCLParser.DispParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#dispParameter}.
	 * @param ctx the parse tree
	 */
	void exitDispParameter(JCLParser.DispParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#dcbParameter}.
	 * @param ctx the parse tree
	 */
	void enterDcbParameter(JCLParser.DcbParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#dcbParameter}.
	 * @param ctx the parse tree
	 */
	void exitDcbParameter(JCLParser.DcbParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#dcbSubParameter}.
	 * @param ctx the parse tree
	 */
	void enterDcbSubParameter(JCLParser.DcbSubParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#dcbSubParameter}.
	 * @param ctx the parse tree
	 */
	void exitDcbSubParameter(JCLParser.DcbSubParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#spaceParameter}.
	 * @param ctx the parse tree
	 */
	void enterSpaceParameter(JCLParser.SpaceParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#spaceParameter}.
	 * @param ctx the parse tree
	 */
	void exitSpaceParameter(JCLParser.SpaceParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#unitParameter}.
	 * @param ctx the parse tree
	 */
	void enterUnitParameter(JCLParser.UnitParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#unitParameter}.
	 * @param ctx the parse tree
	 */
	void exitUnitParameter(JCLParser.UnitParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#volserParameter}.
	 * @param ctx the parse tree
	 */
	void enterVolserParameter(JCLParser.VolserParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#volserParameter}.
	 * @param ctx the parse tree
	 */
	void exitVolserParameter(JCLParser.VolserParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#sysoutParameter}.
	 * @param ctx the parse tree
	 */
	void enterSysoutParameter(JCLParser.SysoutParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#sysoutParameter}.
	 * @param ctx the parse tree
	 */
	void exitSysoutParameter(JCLParser.SysoutParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#recfmParameter}.
	 * @param ctx the parse tree
	 */
	void enterRecfmParameter(JCLParser.RecfmParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#recfmParameter}.
	 * @param ctx the parse tree
	 */
	void exitRecfmParameter(JCLParser.RecfmParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#lreclParameter}.
	 * @param ctx the parse tree
	 */
	void enterLreclParameter(JCLParser.LreclParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#lreclParameter}.
	 * @param ctx the parse tree
	 */
	void exitLreclParameter(JCLParser.LreclParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#blksizeParameter}.
	 * @param ctx the parse tree
	 */
	void enterBlksizeParameter(JCLParser.BlksizeParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#blksizeParameter}.
	 * @param ctx the parse tree
	 */
	void exitBlksizeParameter(JCLParser.BlksizeParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#otherParameter}.
	 * @param ctx the parse tree
	 */
	void enterOtherParameter(JCLParser.OtherParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#otherParameter}.
	 * @param ctx the parse tree
	 */
	void exitOtherParameter(JCLParser.OtherParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#accountInfo}.
	 * @param ctx the parse tree
	 */
	void enterAccountInfo(JCLParser.AccountInfoContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#accountInfo}.
	 * @param ctx the parse tree
	 */
	void exitAccountInfo(JCLParser.AccountInfoContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#jobParameters}.
	 * @param ctx the parse tree
	 */
	void enterJobParameters(JCLParser.JobParametersContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#jobParameters}.
	 * @param ctx the parse tree
	 */
	void exitJobParameters(JCLParser.JobParametersContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#execParameter}.
	 * @param ctx the parse tree
	 */
	void enterExecParameter(JCLParser.ExecParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#execParameter}.
	 * @param ctx the parse tree
	 */
	void exitExecParameter(JCLParser.ExecParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#disposition}.
	 * @param ctx the parse tree
	 */
	void enterDisposition(JCLParser.DispositionContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#disposition}.
	 * @param ctx the parse tree
	 */
	void exitDisposition(JCLParser.DispositionContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#normalDisp}.
	 * @param ctx the parse tree
	 */
	void enterNormalDisp(JCLParser.NormalDispContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#normalDisp}.
	 * @param ctx the parse tree
	 */
	void exitNormalDisp(JCLParser.NormalDispContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#abnormalDisp}.
	 * @param ctx the parse tree
	 */
	void enterAbnormalDisp(JCLParser.AbnormalDispContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#abnormalDisp}.
	 * @param ctx the parse tree
	 */
	void exitAbnormalDisp(JCLParser.AbnormalDispContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#spaceUnit}.
	 * @param ctx the parse tree
	 */
	void enterSpaceUnit(JCLParser.SpaceUnitContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#spaceUnit}.
	 * @param ctx the parse tree
	 */
	void exitSpaceUnit(JCLParser.SpaceUnitContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#primary}.
	 * @param ctx the parse tree
	 */
	void enterPrimary(JCLParser.PrimaryContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#primary}.
	 * @param ctx the parse tree
	 */
	void exitPrimary(JCLParser.PrimaryContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#secondary}.
	 * @param ctx the parse tree
	 */
	void enterSecondary(JCLParser.SecondaryContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#secondary}.
	 * @param ctx the parse tree
	 */
	void exitSecondary(JCLParser.SecondaryContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#directory}.
	 * @param ctx the parse tree
	 */
	void enterDirectory(JCLParser.DirectoryContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#directory}.
	 * @param ctx the parse tree
	 */
	void exitDirectory(JCLParser.DirectoryContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#recfmValue}.
	 * @param ctx the parse tree
	 */
	void enterRecfmValue(JCLParser.RecfmValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#recfmValue}.
	 * @param ctx the parse tree
	 */
	void exitRecfmValue(JCLParser.RecfmValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#dsorgValue}.
	 * @param ctx the parse tree
	 */
	void enterDsorgValue(JCLParser.DsorgValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#dsorgValue}.
	 * @param ctx the parse tree
	 */
	void exitDsorgValue(JCLParser.DsorgValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#unitValue}.
	 * @param ctx the parse tree
	 */
	void enterUnitValue(JCLParser.UnitValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#unitValue}.
	 * @param ctx the parse tree
	 */
	void exitUnitValue(JCLParser.UnitValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#sysoutClass}.
	 * @param ctx the parse tree
	 */
	void enterSysoutClass(JCLParser.SysoutClassContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#sysoutClass}.
	 * @param ctx the parse tree
	 */
	void exitSysoutClass(JCLParser.SysoutClassContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#volserValue}.
	 * @param ctx the parse tree
	 */
	void enterVolserValue(JCLParser.VolserValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#volserValue}.
	 * @param ctx the parse tree
	 */
	void exitVolserValue(JCLParser.VolserValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#datasetName}.
	 * @param ctx the parse tree
	 */
	void enterDatasetName(JCLParser.DatasetNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#datasetName}.
	 * @param ctx the parse tree
	 */
	void exitDatasetName(JCLParser.DatasetNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#datasetComponent}.
	 * @param ctx the parse tree
	 */
	void enterDatasetComponent(JCLParser.DatasetComponentContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#datasetComponent}.
	 * @param ctx the parse tree
	 */
	void exitDatasetComponent(JCLParser.DatasetComponentContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#programName}.
	 * @param ctx the parse tree
	 */
	void enterProgramName(JCLParser.ProgramNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#programName}.
	 * @param ctx the parse tree
	 */
	void exitProgramName(JCLParser.ProgramNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#procName}.
	 * @param ctx the parse tree
	 */
	void enterProcName(JCLParser.ProcNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#procName}.
	 * @param ctx the parse tree
	 */
	void exitProcName(JCLParser.ProcNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#jobName}.
	 * @param ctx the parse tree
	 */
	void enterJobName(JCLParser.JobNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#jobName}.
	 * @param ctx the parse tree
	 */
	void exitJobName(JCLParser.JobNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#stepName}.
	 * @param ctx the parse tree
	 */
	void enterStepName(JCLParser.StepNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#stepName}.
	 * @param ctx the parse tree
	 */
	void exitStepName(JCLParser.StepNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#ddName}.
	 * @param ctx the parse tree
	 */
	void enterDdName(JCLParser.DdNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#ddName}.
	 * @param ctx the parse tree
	 */
	void exitDdName(JCLParser.DdNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#paramName}.
	 * @param ctx the parse tree
	 */
	void enterParamName(JCLParser.ParamNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#paramName}.
	 * @param ctx the parse tree
	 */
	void exitParamName(JCLParser.ParamNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#paramValue}.
	 * @param ctx the parse tree
	 */
	void enterParamValue(JCLParser.ParamValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#paramValue}.
	 * @param ctx the parse tree
	 */
	void exitParamValue(JCLParser.ParamValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#number}.
	 * @param ctx the parse tree
	 */
	void enterNumber(JCLParser.NumberContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#number}.
	 * @param ctx the parse tree
	 */
	void exitNumber(JCLParser.NumberContext ctx);
	/**
	 * Enter a parse tree produced by {@link JCLParser#comment}.
	 * @param ctx the parse tree
	 */
	void enterComment(JCLParser.CommentContext ctx);
	/**
	 * Exit a parse tree produced by {@link JCLParser#comment}.
	 * @param ctx the parse tree
	 */
	void exitComment(JCLParser.CommentContext ctx);
}