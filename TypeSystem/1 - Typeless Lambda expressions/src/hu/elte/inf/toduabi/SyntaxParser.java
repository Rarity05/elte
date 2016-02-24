package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.Stack;

public class SyntaxParser {
	
	private final String EXT_NAME = "_";
	private Stack<LambdaExpression> expressions;

	public SyntaxTree parse(ArrayList<LexParserItem> tokens) throws SyntaxParserException {
		this.expressions = new Stack<LambdaExpression>();
		Stack<Character> stack = new Stack<Character>();
		
		for (int i = 0; i < tokens.size(); i++) {
			String name = tokens.get(i).getName();
			char token = tokens.get(i).getToken();
			
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

	private void checkAndReduce(Stack<Character> stack, String nextName) {
		// TODO Auto-generated method stub
		
	}

}
