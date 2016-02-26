package hu.elte.inf.toduabi;

public class LambdaReduceWrapper {
	private ILambdaExpression expression;
	private boolean couldReduce;
	
	public LambdaReduceWrapper(ILambdaExpression expression, boolean couldReduce) {
		this.expression = expression;
		this.couldReduce = couldReduce;
	}
	
	public ILambdaExpression getExpression() {
		return this.expression;
	}
	public boolean couldReduce() {
		return this.couldReduce;
	}
}
