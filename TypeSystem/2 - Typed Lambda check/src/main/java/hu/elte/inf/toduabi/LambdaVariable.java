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

	@Override
	public ILambdaExpression Substitute(LambdaVariable variable, ILambdaExpression expression) {
		return (this.equals(variable)) ? expression : this;
	}

	@Override
	public IType deductType(TypeContext typeContext) {
		return (this.type == null) ? typeContext.getTypeForVariable(this.variable) : this.type;
	}
}
