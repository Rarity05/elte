package hu.elte.inf.toduabi;

import java.util.HashMap;
import java.util.HashSet;
import java.util.stream.Stream;

public class TypeContext {

	private HashSet<Character> variables;
	private HashSet<LambdaVariable> variableSet;
	private HashMap<Character, IType> types;
	
	public TypeContext(HashSet<LambdaVariable> variables) {
		this.variables = new HashSet<Character>();
		this.types = new HashMap<Character, IType>();
		this.variableSet = new HashSet<LambdaVariable>(variables);
		
		Stream<LambdaVariable> variableStream = variables.stream();
		variableStream.forEach(var -> {
			this.variables.add(var.getVariable());
			this.types.put(var.getVariable(), var.getType());
		});
	}
	
	public HashSet<Character> getVariables() {
		return this.variables;
	}
	
	public HashSet<LambdaVariable> getSet() {
		return this.variableSet;
	}
	
	public IType getTypeForVariable(char variable) {
		if (this.types.containsKey(variable)) {
			return this.types.get(variable);
		} else {
			return null;
		}
	}
	
}
