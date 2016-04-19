package hu.elte.inf.toduabi;

public interface IType {

	boolean contains(IType type);

	IType substitue(Curry.Substitution subs);

}
