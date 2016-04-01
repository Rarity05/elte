package hu.elte.inf.toduabi;

import java.util.HashSet;

public class TypeContext {

	private HashSet<LambdaVariable> variables;
	
	public TypeContext(HashSet<LambdaVariable> variables) {
		this.variables = variables;
	}
	
	public HashSet<LambdaVariable> getVariables() {
		return this.variables;
	}
	
}
