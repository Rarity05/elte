package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.Stack;

public interface ISyntaxCallback<T, K, R> {

	/**
	 * 
	 * @param rule the String rule which was recognized by the SyntaxParser
	 * @param prefixList the sub-stack in list format, for which the recognized rule applies
	 * @param stack the whole sub-stack
	 * @param nextType the next lexical element's type (outside the sub-stack)
	 * @param expressions container for instantiated types (must contain a single element after when the nextType is null)
	 * @return
	 */
	<T extends ILexicalItem<K>> SyntaxParser.ReturnType foundRule(String rule, ArrayList<T> prefixList, Stack<T> stack, K nextType, Stack<R> expressions);

}
