// Generated from com/cobol/translator/grammar/JCL.g4 by ANTLR 4.13.1
package com.cobol.translator.grammar;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link JCLParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface JCLVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link JCLParser#jclFile}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJclFile(JCLParser.JclFileContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatement(JCLParser.StatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#jobStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJobStatement(JCLParser.JobStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#stepStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStepStatement(JCLParser.StepStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#commentLine}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCommentLine(JCLParser.CommentLineContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#pgmExec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPgmExec(JCLParser.PgmExecContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#procExec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcExec(JCLParser.ProcExecContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#ddStatements}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDdStatements(JCLParser.DdStatementsContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#ddStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDdStatement(JCLParser.DdStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#ddDummy}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDdDummy(JCLParser.DdDummyContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#ddInline}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDdInline(JCLParser.DdInlineContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#ddConcat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDdConcat(JCLParser.DdConcatContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#ddParameters}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDdParameters(JCLParser.DdParametersContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#ddParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDdParameter(JCLParser.DdParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#dsnParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDsnParameter(JCLParser.DsnParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#dispParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDispParameter(JCLParser.DispParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#dcbParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDcbParameter(JCLParser.DcbParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#dcbSubParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDcbSubParameter(JCLParser.DcbSubParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#spaceParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSpaceParameter(JCLParser.SpaceParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#unitParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnitParameter(JCLParser.UnitParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#volserParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVolserParameter(JCLParser.VolserParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#sysoutParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSysoutParameter(JCLParser.SysoutParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#recfmParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRecfmParameter(JCLParser.RecfmParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#lreclParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLreclParameter(JCLParser.LreclParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#blksizeParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlksizeParameter(JCLParser.BlksizeParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#otherParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOtherParameter(JCLParser.OtherParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#accountInfo}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAccountInfo(JCLParser.AccountInfoContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#jobParameters}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJobParameters(JCLParser.JobParametersContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#execParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExecParameter(JCLParser.ExecParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#disposition}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDisposition(JCLParser.DispositionContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#normalDisp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNormalDisp(JCLParser.NormalDispContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#abnormalDisp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAbnormalDisp(JCLParser.AbnormalDispContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#spaceUnit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSpaceUnit(JCLParser.SpaceUnitContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimary(JCLParser.PrimaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#secondary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSecondary(JCLParser.SecondaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#directory}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDirectory(JCLParser.DirectoryContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#recfmValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRecfmValue(JCLParser.RecfmValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#dsorgValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDsorgValue(JCLParser.DsorgValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#unitValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnitValue(JCLParser.UnitValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#sysoutClass}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSysoutClass(JCLParser.SysoutClassContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#volserValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVolserValue(JCLParser.VolserValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#datasetName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDatasetName(JCLParser.DatasetNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#datasetComponent}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDatasetComponent(JCLParser.DatasetComponentContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#programName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgramName(JCLParser.ProgramNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#procName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcName(JCLParser.ProcNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#jobName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJobName(JCLParser.JobNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#stepName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStepName(JCLParser.StepNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#ddName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDdName(JCLParser.DdNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#paramName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParamName(JCLParser.ParamNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#paramValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParamValue(JCLParser.ParamValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#number}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNumber(JCLParser.NumberContext ctx);
	/**
	 * Visit a parse tree produced by {@link JCLParser#comment}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComment(JCLParser.CommentContext ctx);
}