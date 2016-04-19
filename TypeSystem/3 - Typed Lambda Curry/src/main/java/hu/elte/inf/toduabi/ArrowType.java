package hu.elte.inf.toduabi;

import hu.elte.inf.toduabi.Curry.Substitution;

public class ArrowType implements IType {

	private IType left;
	private IType right;
	
	public ArrowType(IType left, IType right) {
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
    public int hashCode() {
        return this.left.hashCode() + this.right.hashCode();
    }

    @Override
    public boolean equals(Object _other) {
        if (!(_other instanceof ArrowType)) {
            return false;
        }
        
        ArrowType other = (ArrowType) _other; 
        return this.left.equals(other.getLeft()) && this.right.equals(other.getRight());
    }
    
    @Override
    public String toString() {
    	return "(" + this.left.toString() + " -> " + this.right.toString() + ")";
    }

	@Override
	public boolean contains(IType type) {
		return this.left.contains(type) || this.right.contains(type);
	}

	@Override
	public IType substitue(Substitution subs) {
		return new ArrowType(this.left.substitue(subs), this.right.substitue(subs));
	}

}