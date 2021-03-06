package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.Stack;

public class SyntaxParser {
	
	private final String EXT_NAME = "_";
	private Stack<ILambdaExpression> expressions;
	
	private final String VARIABLE_RULE = SharedConstants.VARIABLE;
	private final String ABSTRACTION_RULE = SharedConstants.LAMBDA + SharedConstants.VARIABLE + SharedConstants.DOT + SharedConstants.EXPRESSION;
	private final String APPLICATION_RULE = SharedConstants.EXPRESSION + SharedConstants.APPLICATION + SharedConstants.EXPRESSION;
	private final String PARENTHESIS_RULE = SharedConstants.OPEN + SharedConstants.EXPRESSION + SharedConstants.CLOSE;
	private final String EXPRESSION_RULE = SharedConstants.EXPRESSION;

	public ILambdaExpression parse(ArrayList<LexParserItem> tokens) throws SyntaxParserException {
		this.expressions = new Stack<ILambdaExpression>();
		Stack<LexParserItem> stack = new Stack<LexParserItem>();
		
		for (int i = 0; i < tokens.size(); i++) {
			LexParserItem token = tokens.get(i);
			String name = tokens.get(i).getName();
			
			String nextName = EXT_NAME;
			if (i+1 < tokens.size()) {
				nextName = tokens.get(i+1).getName();
			}
			
			stack.push(token);
			if (name.equals(SharedConstants.OPEN) || name.equals(SharedConstants.LAMBDA) || name.equals(SharedConstants.DOT)) {
				continue;
			} else {
				checkAndReduce(stack, nextName);
			}
		}
		
		if (stack.size() != 1 || !stack.peek().getName().equals(SharedConstants.EXPRESSION)) {
			throw new SyntaxParserException("eof");
		}
		if (expressions.size() != 1) {
			throw new SyntaxParserException("internal");
		}
		
		return expressions.pop();
	}

	private void checkAndReduce(Stack<LexParserItem> stack, String nextName) throws SyntaxParserException {
		if (stack.isEmpty()) {
			return;
		}
		
		String peekName = stack.peek().getName();
		if (peekName.equals(SharedConstants.OPEN) || peekName.equals(SharedConstants.LAMBDA) || peekName.equals(SharedConstants.DOT) || peekName.equals(SharedConstants.APPLICATION)) {
			return;
		}
		
		Stack<LexParserItem> prefix = new Stack<LexParserItem>();
		String prefixString = "";
		boolean reduced = false;
		
		while (!reduced && !stack.isEmpty()) {
			LexParserItem item = stack.pop();
			prefix.push(item);
			prefixString = item.getName() + prefixString;
			
			if (prefixString.equals(EXPRESSION_RULE) && !stack.isEmpty()) {
				String previousName = stack.peek().getName();
				if (previousName.equals(SharedConstants.DOT) && nextName.equals(SharedConstants.APPLICATION) ) {
					stack.push(item);
					return;
				} else {
					continue;
				}
			}
			
			if (prefixString.equals(VARIABLE_RULE)) {
				
				reduced = true;
				expressions.push(new LambdaVariable(item.getToken()));
				if (nextName.equals(SharedConstants.DOT)) {
					stack.push(item);
				} else {
					stack.push(new LexParserItem('E', SharedConstants.EXPRESSION));
					checkAndReduce(stack, nextName);
				}
				
			} else if (prefixString.equals(ABSTRACTION_RULE)) {
				
				reduced = true;
				stack.push(new LexParserItem('E', SharedConstants.EXPRESSION));
				
				ILambdaExpression expression = expressions.pop();
				ILambdaExpression _variable = expressions.pop();
				if (!_variable.getClass().equals(LambdaVariable.class)) {
					throw new SyntaxParserException("invalid abstraction rule found");
				}
				LambdaVariable variable = (LambdaVariable) _variable;
				expressions.push(new LambdaAbstraction(variable, expression));
				
				checkAndReduce(stack, nextName);
				
			} else if (prefixString.equals(APPLICATION_RULE)) {
				
				reduced = true;
				stack.push(new LexParserItem('E', SharedConstants.EXPRESSION));
				
				ILambdaExpression expressionB = expressions.pop();
				ILambdaExpression expressionA = expressions.pop();
				expressions.push(new LambdaApplication(expressionA, expressionB));
				
				checkAndReduce(stack, nextName);
				
			} else if (prefixString.equals(PARENTHESIS_RULE)) {
				
				reduced = true;
				stack.push(new LexParserItem('E', SharedConstants.EXPRESSION));
				
				checkAndReduce(stack, nextName);
				
			} else {
				 continue;
			}
		}
		
		if (!reduced) {
			while (!prefix.isEmpty()) {
				stack.push(prefix.pop());
			}
		}
	}
	

}
