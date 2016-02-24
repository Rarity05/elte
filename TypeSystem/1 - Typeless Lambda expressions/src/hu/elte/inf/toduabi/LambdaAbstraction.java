package hu.elte.inf.toduabi;

import java.util.HashSet;

public class LambdaAbstraction implements ILambdaExpression {
	private LambdaVariable variable;
	private ILambdaExpression expression;
	
	public LambdaAbstraction(LambdaVariable variable, ILambdaExpression expression) {
		this.variable = variable;
		this.expression = expression;
	}
	
	public ILambdaExpression getExpression() {
		return this.expression;
	}
	public LambdaVariable getVariable() {
		return this.variable;
	}
	
	@Override
	public int hashCode(){
        return this.variable.hashCode()*3 + this.expression.hashCode()*5;
    }
	
	@Override
	public boolean equals(Object _other) {
		if (!(_other instanceof LambdaAbstraction)) {
			return false;
		}
		
		LambdaAbstraction other = (LambdaAbstraction) _other;
		return this.variable.equals(other.getVariable()) && this.expression.equals(other.getExpression());
	}

	@Override
	public String toString() {
		return "(" + "\\" + this.variable.toString() + "." + this.expression.toString() + ")";
	}

	public HashSet<LambdaVariable> getFreeVariables() {
		HashSet<LambdaVariable> retVal = this.expression.getFreeVariables();
		retVal.remove(this.variable);
		return retVal;
	}

	public HashSet<LambdaVariable> getBoundedVariables() {
		HashSet<LambdaVariable> retVal = this.expression.getBoundedVariables();
		retVal.add(this.variable);
		return retVal;
	}
}
