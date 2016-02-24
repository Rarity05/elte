package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.Stack;

public class SyntaxParser {
	
	private final String EXT_NAME = "_";
	private Stack<LambdaExpression> expressions;
	
	private final String VARIABLE_RULE = SharedConstants.VARIABLE;
	private final String ABSTRACTION_RULE = SharedConstants.LAMBDA + SharedConstants.VARIABLE + SharedConstants.DOT + SharedConstants.EXPRESSION;
	private final String APPLICATION_RULE = SharedConstants.EXPRESSION + SharedConstants.APPLICATION + SharedConstants.EXPRESSION;
	private final String PARENTHESIS_RULE = SharedConstants.OPEN + SharedConstants.EXPRESSION + SharedConstants.CLOSE;

	public SyntaxTree parse(ArrayList<LexParserItem> tokens) throws SyntaxParserException {
		this.expressions = new Stack<LambdaExpression>();
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
		if (!stack.isEmpty()) {
			throw new SyntaxParserException("eof");
		}
		return null;
	}

	private void checkAndReduce(Stack<LexParserItem> stack, String nextName) throws SyntaxParserException {
		if (stack.isEmpty()) {
			return;
		}
		
		Stack<LexParserItem> prefix = new Stack<LexParserItem>();
		String prefixString = "";
		boolean reduced = false;
		
		while (!reduced || stack.size() != 0) {
			LexParserItem item = stack.pop();
			prefix.push(item);
			prefixString = prefixString + item.getName();
			
			if (prefix.equals(VARIABLE_RULE)) {
				
				reduced = true;
				expressions.push(new LambdaVariable(item.getToken()));
				if (nextName.equals(SharedConstants.DOT)) {
					stack.push(item);
				} else {
					stack.push(new LexParserItem('E', SharedConstants.EXPRESSION));
					checkAndReduce(stack, nextName);
				}
				
			} else if (prefix.equals(ABSTRACTION_RULE)) {
				
				reduced = true;
				stack.push(new LexParserItem('E', SharedConstants.EXPRESSION));
				
				LambdaExpression expression = expressions.pop();
				LambdaExpression _variable = expressions.pop();
				if (!_variable.getClass().equals(LambdaVariable.class)) {
					throw new SyntaxParserException("invalid abstraction rule found");
				}
				LambdaVariable variable = (LambdaVariable) _variable;
				expressions.push(new LambdaAbstraction(variable.getVariable(), expression));
				
				checkAndReduce(stack, nextName);
				
			} else if (prefix.equals(APPLICATION_RULE)) {
				
			} else if (prefix.equals(PARENTHESIS_RULE)) {
				
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
