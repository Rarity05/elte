package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.Stack;

public interface ISyntaxCallback<T, K, R> {

	<T extends ILexicalItem<K>> SyntaxParser.ReturnType foundRule(String rule, ArrayList<T> prefixList, Stack<T> stack, K nextType, Stack<R> expressions);

}
