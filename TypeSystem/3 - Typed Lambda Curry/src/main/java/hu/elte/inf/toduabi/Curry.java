package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Stack;

public class Curry {
	private static ArrayList<String> variables = new ArrayList<String>();
	private static int index = 0;
	static {
		for (int i = 'A'; i <= 'Z'; i++) {
			Curry.variables.add(Character.toString(Character.toChars(i)[0]));
		}
	}
	public static String getTypeVariable() {
		return getTypeVariable(null);
	}
	public static String getTypeVariable(TypeContext context) {
		if (context == null) {
			try {
				return Curry.variables.get(index++);
			} catch (Exception e) {
				throw new RuntimeException("Out of variables");
			}
		} else {
			ArrayList<LambdaVariable> setVariables = new ArrayList<LambdaVariable>();
			context.getSet().stream().forEach(item -> setVariables.add(item));
			
			boolean l = false;
			while (!l) {
				boolean l2 = false;
				for (LambdaVariable v : setVariables) {
					if (v.getType().equals(new SingleType(variables.get(index)))) {
						l2 = true;
					}
				}
				if (l2) index++;
				else l = true;
			}
			
			return variables.get(index++);
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
			IType left = new SingleType(getTypeVariable(context));
			IType right = new SingleType(getTypeVariable(context));
			
			HashSet<LambdaVariable> nContextSet = context.getSet();
			LambdaVariable nVariable = new LambdaVariable(abstraction.getVariable().getVariable(), left);
			nContextSet.add(nVariable);
			TypeContext nContext = new TypeContext(nContextSet);
			
			
			retVal.add(new Restriction(type, new ArrowType(left, right)));
			List<Restriction> subResult = T(nContext, abstraction.getExpression(), right);
			retVal.addAll(subResult);
			
			return retVal;
		} else if (clazz.equals(LambdaApplication.class)) {
			LambdaApplication application = (LambdaApplication) expression;
			
			IType next = new SingleType(getTypeVariable(context));
			
			List<Restriction> subResA = T(context, application.getExpressionA(), new ArrowType(next, type));
			List<Restriction> subResB = T(context, application.getExpressionB(), next);
			
			retVal.addAll(subResA);
			retVal.addAll(subResB);
			
			return retVal;
		} else {
			throw new RuntimeException("Not handled class in T");
		}
	}
	
	public static IType S(List<Restriction> restrictions, List<Substitution> substitutions) {
		Stack<Restriction> rStack = new Stack<Restriction>();
		rStack.addAll(restrictions);
		
		if (rStack.isEmpty()) {
			IType retVal = new SingleType("Z");
			for (Substitution subs : substitutions) {
				retVal = retVal.substitue(subs);
			}
			return retVal;
		}
		
		Restriction r = rStack.pop();
		if (r.getLeft().getClass().equals(SingleType.class) && r.getLeft().equals(r.getRight())) {
			return S(new ArrayList<Restriction>(rStack), substitutions);
		} else if (r.getLeft().getClass().equals(SingleType.class)) {
			if (r.getRight().contains(r.getLeft())) {
				throw new RuntimeException("recursion");
			}
			Substitution subs = new Substitution((SingleType)r.getLeft(), r.getRight());
			for (int i=0; i<rStack.size(); i++) {
				rStack.get(i).substitute(subs);
			}
			ArrayList<Substitution> nSubs = new ArrayList<Substitution>();
			nSubs.addAll(substitutions);
			nSubs.add(subs);
			
			return S(new ArrayList<Restriction>(rStack), nSubs);
		} else if (r.getRight().getClass().equals(SingleType.class)) {
			if (r.getLeft().contains(r.getRight())) {
				throw new RuntimeException("recursion");
			}
			Substitution subs = new Substitution((SingleType)r.getRight(), r.getLeft());
			for (int i=0; i<rStack.size(); i++) {
				rStack.get(i).substitute(subs);
			}
			ArrayList<Substitution> nSubs = new ArrayList<Substitution>();
			nSubs.addAll(substitutions);
			nSubs.add(subs);
			
			return S(new ArrayList<Restriction>(rStack), nSubs);
		} else {
			// T1 -> T2 = T3 -> T4
			ArrayList<Restriction> nRes = new ArrayList<Restriction>(rStack);
			ArrowType a = (ArrowType) r.getLeft();
			ArrowType b = (ArrowType) r.getRight();
			nRes.add(new Restriction(a.getLeft(), b.getLeft()));
			nRes.add(new Restriction(a.getRight(), b.getRight()));
			
			return S(nRes, substitutions);
		}
		
	}
	
	public static class Substitution {

		private SingleType _subs;
		private IType _with;
		
		public Substitution(SingleType subs, IType with) {
			_subs = subs;
			_with = with;
		}

		public Object subs() {
			return _subs;
		}

		public IType with() {
			return _with;
		}
		
		@Override
		public int hashCode() {
			return this._subs.hashCode() + this.with().hashCode();
		}
		
		@Override
		public boolean equals(Object _other) {
			if (!(_other instanceof Substitution)) {
				return false;
			}
			
			Substitution other = (Substitution) _other;
			return this._subs.equals(other.subs()) && this._with.equals(other.with());
		}
		
	}
	
	public static class Restriction {
		private IType left, right;
		
		public Restriction(IType left, IType right) {
			this.left = left;
			this.right = right;
		}
		
		public void substitute(Substitution subs) {
			this.left = this.left.substitue(subs);
			this.right = this.right.substitue(subs);
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
		
		@Override
		public int hashCode() {
			return this.left.hashCode() + this.right.hashCode();
		}
		
		@Override
		public boolean equals(Object _other) {
			if (!(_other instanceof Restriction)) {
				return false;
			}
			
			Restriction other = (Restriction) _other;
			return this.left.equals(other.getLeft()) && this.right.equals(other.getRight());
		}
	}

	public static void reset() {
		index = 0;		
	}
}
