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
	public String toString() {
		return Character.toString(this.variable);
	}

	public HashSet<LambdaVariable> getFreeVariables() {
		// TODO Auto-generated method stub
		return null;
	}

	public HashSet<LambdaVariable> getBoundedVariables() {
		// TODO Auto-generated method stub
		return null;
	}
}
