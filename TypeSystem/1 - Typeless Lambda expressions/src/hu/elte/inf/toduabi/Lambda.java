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
		System.out.println("Free variables: " + expression.getFreeVariables().toString());
		System.out.println("Bounded variables: " + expression.getBoundedVariables().toString());
		
		HashSet<LambdaVariable> freeVariables = expression.getFreeVariables();
		if (!freeVariables.isEmpty()) {
			throw new LambdaNormalizeException("Not a closed expression (free): " + freeVariables.toString());
		}
		
		int reduceCount = 0;
		ILambdaExpression tmp;
		boolean couldReduce = true;
		while (couldReduce && reduceCount++ < maxIterations) {
			tmp = expression;
			expression = this.reduce(expression);
			couldReduce = !expression.equals(tmp);
		}
		if (reduceCount >= maxIterations) {
			throw new LambdaNormalizeException("Timeout");
		}
		
		return expression.toString();
	}

	private ILambdaExpression reduce(ILambdaExpression expression) throws LambdaNormalizeException {
		ILambdaExpression reduced = SharedConstants.nConversion(expression); 
		if (!reduced.equals(expression)) {
			return this.reduce(reduced);
		}
		
		if (expression.getClass().equals(LambdaVariable.class)) {
			return expression;
		} else if (expression.getClass().equals(LambdaAbstraction.class)) {
			LambdaAbstraction abstraction = (LambdaAbstraction) expression;
			LambdaVariable var = abstraction.getVariable();
			ILambdaExpression exp = abstraction.getExpression();
			return new LambdaAbstraction(var, this.reduce(exp));
		} else if (expression.getClass().equals(LambdaApplication.class)) {
			
			LambdaApplication application = (LambdaApplication) expression;
			ILambdaExpression expA = application.getExpressionA();
			ILambdaExpression expB = application.getExpressionB();
			
			if (expA.getClass().equals(LambdaAbstraction.class)) {
				LambdaAbstraction abstraction = (LambdaAbstraction) expA;
				LambdaVariable var = abstraction.getVariable();
				ILambdaExpression exp = abstraction.getExpression();
				if (this.hasLeftRedex(expA)) {
					// Right?
					return new LambdaAbstraction(var, this.reduce(exp));
				} else {
					
					return exp.Substitute(abstraction.getVariable(), expB);
				}
			} else {
				// Right?
				return new LambdaApplication(this.reduce(expA), expB);
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
