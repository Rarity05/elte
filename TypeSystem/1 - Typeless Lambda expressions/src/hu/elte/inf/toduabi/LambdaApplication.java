package hu.elte.inf.toduabi;

public class LambdaApplication extends LambdaExpression {
	private LambdaExpression expressionA;
	private LambdaExpression expressionB;
	
	public LambdaApplication(LambdaExpression expressionA, LambdaExpression expressionB) {
		this.expressionA = expressionA;
		this.expressionB = expressionB;
	}
	
	public LambdaExpression getExpressionA() {
		return this.expressionA;
	}
	public LambdaExpression getExpressionB() {
		return this.expressionB;
	}
}
