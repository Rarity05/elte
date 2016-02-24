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

	@Override
	public String toString() {
		return "(" + "\\" + this.variable.toString() + "." + this.expression.toString() + ")";
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
