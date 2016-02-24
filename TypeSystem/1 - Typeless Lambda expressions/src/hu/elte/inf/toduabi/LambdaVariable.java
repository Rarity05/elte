package hu.elte.inf.toduabi;

public class LambdaVariable extends LambdaExpression {
	private char variable;
	
	public LambdaVariable(char variable) {
		this.variable = variable;
	}
	
	public char getVariable() {
		return this.variable;
	}
}
