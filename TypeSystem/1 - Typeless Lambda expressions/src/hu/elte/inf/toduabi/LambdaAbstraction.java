package hu.elte.inf.toduabi;

public class LambdaAbstraction extends LambdaVariable {
	private LambdaExpression expression;
	
	public LambdaAbstraction(char variable, LambdaExpression expression) {
		super(variable);
		this.expression = expression;
	}
	
	public LambdaExpression getExpression() {
		return this.expression;
	}

	@Override
	public String toString() {
		return "(" + "\\" + super.toString() + "." + this.expression.toString() + ")";
	}
}
