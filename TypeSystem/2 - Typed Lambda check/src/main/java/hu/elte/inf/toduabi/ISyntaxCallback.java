package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.Stack;

public interface ISyntaxCallback<T extends ILexicalItem<K>, K, R> {

	/**
	 * 
	 * @param rule the String rule which was recognized by the SyntaxParser
	 * @param prefixList the sub-stack in list format, for which the recognized rule applies
	 * @param stack the whole sub-stack
	 * @param remainingTokens the remaining tokens to be examined
	 * @param nextType the next lexical element's type (outside the sub-stack)
	 * @param expressions container for instantiated types (must contain a single element after when the nextType is null)
	 * @return
	 * @throws SyntaxParserException 
	 */
	<T extends ILexicalItem<K>> SyntaxParser.ReturnWrapper foundRule(String rule, ArrayList<T> prefixList, Stack<T> stack, ArrayList<T> remainingTokens, K nextType, Stack<R> expressions) throws SyntaxParserException;

}
