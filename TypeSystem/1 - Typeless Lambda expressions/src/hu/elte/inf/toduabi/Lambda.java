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
		boolean couldReduce = true;
		ILambdaExpression tmp = expression;
		while (couldReduce && reduceCount++ < maxIterations) {
			tmp = this.reduce(expression);
			couldReduce = tmp.equals(expression);
			expression = tmp;
		}
		
		return expression.toString();
	}

	private ILambdaExpression reduce(ILambdaExpression expression) throws LambdaNormalizeException {
		ILambdaExpression nConverted = expression.nConversion();
		if (!nConverted.equals(expression)) {
			return this.reduce(nConverted);
		}
		
		
		
		return null;
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
