package hu.elte.inf.toduabi;

import java.util.HashSet;

public interface ILambdaExpression {
	public HashSet<LambdaVariable> getFreeVariables();
	public HashSet<LambdaVariable> getBoundedVariables();
	public ILambdaExpression Substitute(LambdaVariable variable, ILambdaExpression expression) throws LambdaNormalizeException;
}
