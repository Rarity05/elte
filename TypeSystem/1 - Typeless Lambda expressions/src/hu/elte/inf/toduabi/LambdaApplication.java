package hu.elte.inf.toduabi;

import java.util.HashSet;

public class LambdaApplication implements ILambdaExpression {
	private ILambdaExpression expressionA;
	private ILambdaExpression expressionB;
	
	public LambdaApplication(ILambdaExpression expressionA, ILambdaExpression expressionB) {
		this.expressionA = expressionA;
		this.expressionB = expressionB;
	}
	
	public ILambdaExpression getExpressionA() {
		return this.expressionA;
	}
	public ILambdaExpression getExpressionB() {
		return this.expressionB;
	}
	
	@Override
	public int hashCode(){
        return this.expressionA.hashCode()*3 + this.expressionB.hashCode()*5;
    }
	
	@Override
	public boolean equals(Object _other) {
		if (!(_other instanceof LambdaApplication)) {
			return false;
		}
		
		LambdaApplication other = (LambdaApplication) _other; 
		return this.expressionA.equals(other.getExpressionA()) && this.expressionB.equals(other.getExpressionB());
	}
	
	@Override
	public String toString() {
		return "(" + this.expressionA.toString() + " " + this.expressionB.toString() + ")";
	}

	public HashSet<LambdaVariable> getFreeVariables() {
		HashSet<LambdaVariable> retVal = new HashSet<LambdaVariable>();
		retVal.addAll(this.expressionA.getFreeVariables());
		retVal.addAll(this.expressionB.getFreeVariables());
		return retVal;
	}

	public HashSet<LambdaVariable> getBoundedVariables() {
		HashSet<LambdaVariable> retVal = new HashSet<LambdaVariable>();
		retVal.addAll(this.expressionA.getBoundedVariables());
		retVal.addAll(this.expressionB.getBoundedVariables());
		return retVal;
	}

	@Override
	public ILambdaExpression Substitute(LambdaVariable variable, ILambdaExpression expression)
			throws LambdaNormalizeException {
		ILambdaExpression expA = this.expressionA.Substitute(variable, expression);
		ILambdaExpression expB = this.expressionB.Substitute(variable, expression);
		
		return new LambdaApplication(expA, expB);
	}
}
