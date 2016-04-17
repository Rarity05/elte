package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class Curry {
	private static ArrayList<String> variables = new ArrayList<String>();
	private static int index = 0;
	static {
		for (int i = 'A'; i <= 'Z'; i++) {
			Curry.variables.add(Character.toString(Character.toChars(i)[0]));
		}
	}
	public static String getTypeVariable() {
		try {
			return Curry.variables.get(index++);
		} catch (Exception e) {
			throw new RuntimeException("Out of variables");
		}
	}
	public static List<Restriction> T(TypeContext context, ILambdaExpression expression, IType type) {
		ArrayList<Restriction> retVal = new ArrayList<Restriction>();
		
		Class<?> clazz = expression.getClass();
		if (clazz.equals(LambdaVariable.class)) {
			IType vType = context.getTypeForVariable(((LambdaVariable) expression).getVariable());
			if (vType == null) {
				throw new RuntimeException("Type not found");
			}
			
			retVal.add(new Restriction(type, vType));
			return retVal;
		} else if (clazz.equals(LambdaAbstraction.class)) {
			LambdaAbstraction abstraction = (LambdaAbstraction) expression;
			IType left = new SingleType(getTypeVariable());
			IType right = new SingleType(getTypeVariable());
			IType next = new SingleType(getTypeVariable());
			
			HashSet<LambdaVariable> nContextSet = context.getSet();
			LambdaVariable nVariable = new LambdaVariable(abstraction.getVariable().getVariable(), left);
			nContextSet.add(nVariable);
			TypeContext nContext = new TypeContext(nContextSet);
			
			
			retVal.add(new Restriction(type, new ArrowType(left, right)));
			List<Restriction> subResult = T(nContext, abstraction.getExpression(), next);
			retVal.addAll(subResult);
			
			return retVal;
		} else if (clazz.equals(LambdaApplication.class)) {
			LambdaApplication application = (LambdaApplication) expression;
			
			IType next = new SingleType(getTypeVariable());
			
			List<Restriction> subResA = T(context, application.getExpressionA(), new ArrowType(next, type));
			List<Restriction> subResB = T(context, application.getExpressionB(), next);
			
			retVal.addAll(subResA);
			retVal.addAll(subResB);
			
			return retVal;
		} else {
			throw new RuntimeException("Not handled class in T");
		}
	}
	
	public static IType S(List<Restriction> restrictions) {
		return null;
	}
	
	public static class Restriction {
		private IType left, right;
		
		public Restriction(IType left, IType right) {
			this.left = left;
			this.right = right;
		}
		
		public IType getLeft() {
			return this.left;
		}
		
		public IType getRight() {
			return this.right;
		}
		
		@Override
		public String toString() {
			return this.left.toString() + " = " + this.right.toString();
		}
	}
}
