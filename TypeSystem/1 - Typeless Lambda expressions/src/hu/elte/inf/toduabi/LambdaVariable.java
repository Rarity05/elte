package hu.elte.inf.toduabi;

public class LambdaVariable extends LambdaExpression {
	private char variable;
	
	public LambdaVariable(char variable) {
		this.variable = variable;
	}
	
	public char getVariable() {
		return this.variable;
	}
	
	@Override
	public String toString() {
		return Character.toString(this.variable);
	}
}
