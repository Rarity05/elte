package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.HashSet;

public class Lambda {
	private LexParser lexParser;
	private SyntaxParser syntaxParser;
	
	public Lambda() {		
		this.lexParser = new LexParser();		
		this.syntaxParser = new SyntaxParser();
	}
	
	public String getNormalForm(String input, int maxIterations) throws LexParserException, SyntaxParserException, LambdaNormalizeException {
		if (maxIterations <= 0) {
			throw new LambdaNormalizeException("Incorrect iteration number: " + maxIterations);
		}
		
		ArrayList<LexParserItem> tokens = this.lexParser.parse(input);
		ILambdaExpression expression = this.syntaxParser.parse(tokens);
		
		HashSet<LambdaVariable> freeVariables = expression.getFreeVariables();
		if (!freeVariables.isEmpty()) {
			throw new LambdaNormalizeException("Not a closed expression (free): " + freeVariables.toString());
		}
		
		int reduceCount = 0;
		boolean couldReduce = true;
		while (couldReduce && reduceCount++ < maxIterations) {
			LambdaReduceWrapper wrapper = this.reduce(expression);
			expression = wrapper.getExpression();
			couldReduce = wrapper.couldReduce();
		}
		if (reduceCount >= maxIterations) {
			throw new LambdaNormalizeException("Timeout");
		}
		
		return expression.toString();
	}

	private LambdaReduceWrapper reduce(ILambdaExpression expression) throws LambdaNormalizeException {
		ILambdaExpression reduced = SharedConstants.nConversion(expression); 
		if (!reduced.equals(expression)) {
			return this.reduce(reduced);
		}
		
		if (expression.getClass().equals(LambdaVariable.class)) {
			return new LambdaReduceWrapper(expression, false);
		} else if (expression.getClass().equals(LambdaAbstraction.class)) {
			LambdaAbstraction abstraction = (LambdaAbstraction) expression;
			LambdaVariable var = abstraction.getVariable();
			ILambdaExpression exp = abstraction.getExpression();
			
			LambdaReduceWrapper expReduced = this.reduce(exp);
			LambdaAbstraction retVal = new LambdaAbstraction(var, expReduced.getExpression());
			return new LambdaReduceWrapper(retVal, expReduced.couldReduce());
		} else if (expression.getClass().equals(LambdaApplication.class)) {
			
			LambdaApplication application = (LambdaApplication) expression;
			ILambdaExpression expA = application.getExpressionA();
			ILambdaExpression expB = application.getExpressionB();
			
			if (expA.getClass().equals(LambdaAbstraction.class)) {
				LambdaAbstraction abstraction = (LambdaAbstraction) expA;
				LambdaVariable var = abstraction.getVariable();
				ILambdaExpression exp = abstraction.getExpression();
				if (this.hasLeftRedex(expA)) {
					LambdaReduceWrapper expReduced = this.reduce(exp);
					LambdaAbstraction retValAbs = new LambdaAbstraction(var, expReduced.getExpression());
					LambdaApplication retVal = new LambdaApplication(retValAbs, expB); 
					return new LambdaReduceWrapper(retVal, expReduced.couldReduce());
				} else {
					
					return new LambdaReduceWrapper(exp.Substitute(var, expB), true);
				}
			} else {
				LambdaReduceWrapper expReduced = this.reduce(expA);
				LambdaApplication retVal = new LambdaApplication(expReduced.getExpression(), expB);
				if (!expReduced.couldReduce()) {
					expReduced = this.reduce(expB);
					retVal = new LambdaApplication(expA, expReduced.getExpression());
				}
				 
				return new LambdaReduceWrapper(retVal, expReduced.couldReduce());
			}
			
		} else {
			throw new LambdaNormalizeException("Uncrecognized class: " + expression.getClass());
		}
	}
	
	private boolean hasLeftRedex(ILambdaExpression expression) throws LambdaNormalizeException {
		if (expression.getClass().equals(LambdaVariable.class)) {
			return false;
		} else if (expression.getClass().equals(LambdaAbstraction.class)) {
			return this.hasLeftRedex(((LambdaAbstraction) expression).getExpression());
		} else if (expression.getClass().equals(LambdaApplication.class)) {
			LambdaApplication application = (LambdaApplication) expression;
			ILambdaExpression expA = application.getExpressionA();
			if (expA.getClass().equals(LambdaAbstraction.class)) {
				return true;
			} else {
				return this.hasLeftRedex(expA);
			}
		} else {
			throw new LambdaNormalizeException("Uncrecognized class: " + expression.getClass());
		}
		
	}
}
