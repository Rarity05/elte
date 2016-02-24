package hu.elte.inf.toduabi;

import java.util.HashSet;

public class LambdaVariable implements ILambdaExpression {
	private char variable;
	
	public LambdaVariable(char variable) {
		this.variable = variable;
	}
	
	public char getVariable() {
		return this.variable;
	}
	
	@Override
	public boolean equals(Object _other) {
		if (!(_other instanceof LambdaVariable)) {
			return false;
		}
		
		LambdaVariable other = (LambdaVariable) _other; 
		return this.variable == other.getVariable();
	}
	
	@Override
	public String toString() {
		return Character.toString(this.variable);
	}

	public HashSet<LambdaVariable> getFreeVariables() {
		HashSet<LambdaVariable> retVal = new HashSet<LambdaVariable>();
		retVal.add(this);
		return retVal;
	}

	public HashSet<LambdaVariable> getBoundedVariables() {
		HashSet<LambdaVariable> retVal = new HashSet<LambdaVariable>();
		return retVal;
	}
}
