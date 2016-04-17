package hu.elte.inf.toduabi;

import java.util.HashSet;

public class LambdaVariable implements ILambdaExpression {
	private char variable;
	private IType type;
	
	public LambdaVariable(char variable, IType type) {
		this.variable = variable;
		this.type = type;
	}
	
	public char getVariable() {
		return this.variable;
	}
	
	public IType getType() {
		return this.type;
	}
	
	@Override
	public int hashCode(){
        return this.variable + this.type.hashCode();
    }
	
	@Override
	public boolean equals(Object _other) {
		if (!(_other instanceof LambdaVariable)) {
			return false;
		}
		
		LambdaVariable other = (LambdaVariable) _other; 
		return this.variable == other.getVariable() && ((this.type == null && other.getType() == null) || this.type.equals(other.getType()));
	}
	
	@Override
	public String toString() {
		String retVal = Character.toString(this.variable);
		if (this.type != null) {
			retVal += ":" + this.type.toString();
		}
		return retVal;
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
