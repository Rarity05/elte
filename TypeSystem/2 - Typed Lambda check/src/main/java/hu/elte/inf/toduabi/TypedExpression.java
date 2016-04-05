package hu.elte.inf.toduabi;

public class TypedExpression {
	private TypeContext typeContext;
	private ILambdaExpression expression;
	private IType type;
	
	public TypedExpression(TypeContext typeContext, ILambdaExpression expression, IType type) {
		this.typeContext = typeContext;
		this.expression = expression;
		this.type = type;
	}
	
	public TypeContext getTypeContext() {
		return this.typeContext;
	}
	
	public ILambdaExpression getExpression() {
		return this.expression;
	}
	
	public IType getType() {
		return this.type;
	}
	
	@Override
	public String toString() {
		return this.expression.toString() + ":" + this.type.toString();
	}
}
