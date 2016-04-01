package hu.elte.inf.toduabi;

public class SingleType implements IType {
	
	private String type;
	
	public SingleType(String type) {
		this.type = type;
	}
	
	public String getType() {
		return this.type;
	}
	
	@Override
    public int hashCode() {
        int sum = 0;
        for (int i=0; i<this.type.length(); i++) {
            sum += this.type.charAt(i);
        }
        return sum;
    }

    @Override
    public boolean equals(Object _other) {
        if (!(_other instanceof SingleType)) {
            return false;
        }
        
        SingleType other = (SingleType) _other; 
        return this.type.equals(other.getType());
    }
    
    @Override
    public String toString() {
    	return this.type;
    }

}