package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 
 * @author I321357
 *
 * @param <T> The lexical elements
 * @param <K> The type which we can compare lexical elements
 * @param <R> The return type
 */
public class SyntaxParser<T extends ILexicalItem<K>, K, R> {
	
	private Stack<R> expressions;
	private ISyntaxCallback<T, K, R> callback;
	private HashMap<ArrayList<K>, String> rules;
	
	public enum ReturnType {
		CONTINUE, RETURN, CHECK
	}
	public static class ReturnWrapper {
		private ReturnType type;
		private int index;
		
		public ReturnWrapper(ReturnType type, int index) {
			this.type = type;
			this.index = index;
		}
		
		public ReturnType getType() {
			return this.type;
		}
		
		public int getIndex() {
			return this.index;
		}
	}
	
	public SyntaxParser(HashMap<ArrayList<K>, String> rules, ISyntaxCallback<T, K, R> callback) {
		this.callback = callback;
		this.rules = rules;
	}

	public <T extends ILexicalItem<K>> R parse(ArrayList<T> tokens) throws SyntaxParserException {
		this.expressions = new Stack<R>();
		Stack<T> stack = new Stack<T>();
		
		for (int i = 0; i < tokens.size(); /*no-operation*/) {			
			stack.push(tokens.get(i));
			K nextType = (i+1 < tokens.size()) ? tokens.get(i+1).getType() : null;
			ArrayList<T> remainingTokens = (nextType == null) ? new ArrayList<T>() : new ArrayList<T>(tokens.subList(i+1, tokens.size()-1));
			i += checkAndReduce(stack, remainingTokens, nextType);
		}
		
		if (!stack.isEmpty()) {
			throw new SyntaxParserException("eof");
		}
		
		if (expressions.size() != 1) {
			throw new SyntaxParserException("internal");
		}
		
		return expressions.pop();
	}

	private <T extends ILexicalItem<K>> int checkAndReduce(Stack<T> stack, ArrayList<T> remainingTokens, K nextType) throws SyntaxParserException {
		
		Stack<T> prefixStack = new Stack<T>();		
		/**
		 * We iterate the 'sub-stack' for rules
		 */
		while (!stack.isEmpty()) {
			T item = stack.pop();
			prefixStack.push(item);
			
			ArrayList<T> prefixList = new ArrayList<T>(prefixStack);
			String rule = getRule(prefixList);
			if (rule != null) {
				ReturnWrapper retVal = callback.foundRule(rule, prefixList, stack, remainingTokens, nextType, this.expressions);
				switch (retVal.getType()) {
					case CONTINUE: continue;
					case RETURN: return retVal.getIndex();
					case CHECK: return checkAndReduce(stack, remainingTokens, nextType);
					default: break;
				}
			}
		}
		
		/**
		 * If no rules apply for the 'sub-stack' then we put everything back
		 */
		while (!prefixStack.isEmpty()) {
			stack.push(prefixStack.pop());
		}
		
		return 1;
	}

	/**
	 * Gets a rule from the rules
	 * @param prefixList
	 * @return
	 */
	private <T extends ILexicalItem<K>> String getRule(ArrayList<T> prefixList) {
		// Rules are contained in the format of List<R>, so we have to map our parameter to match
		Stream<T> prefixStream = prefixList.stream();
		Stream<K> mappedStream = prefixStream.map(lexicalItem -> lexicalItem.getType());
		ArrayList<K> mappedList = new ArrayList<K>(mappedStream.collect(Collectors.toList()));
		
		// Select the rule if any
		if (this.rules.containsKey(mappedList)) {
			return this.rules.get(mappedList);
		} else {
			return null;
		}
	}
	

}
