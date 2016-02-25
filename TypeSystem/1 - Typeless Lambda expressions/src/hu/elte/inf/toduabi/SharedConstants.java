package hu.elte.inf.toduabi;

import java.util.HashSet;

public class SharedConstants {
	public static final String LAMBDA = "Lambda";
	public static final String VARIABLE = "Variable";
	public static final String APPLICATION = "Application";
	public static final String ABSTRACTION = "Abstraction";
	public static final String EXPRESSION = "Expression";
	public static final String DOT = "Dot";
	public static final String OPEN = "Open";
	public static final String CLOSE = "Close";
	public static LambdaVariable getNonConflictVariable(HashSet<LambdaVariable> free) throws LambdaNormalizeException {
		if (free == null) {
			throw new LambdaNormalizeException("Null set in alpha conversion");
		}
		
		int i = 'a';
		LambdaVariable retVal;
		
		do {
			retVal = new LambdaVariable(Character.toChars(i)[0]);
		} while (free.contains(retVal) && ++i <= 'z');
		
		if (i > 'z') {
			throw new LambdaNormalizeException("Run out of variable names");
		}
		
		return retVal;
	}
}
