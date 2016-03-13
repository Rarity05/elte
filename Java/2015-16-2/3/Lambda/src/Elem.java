package gyak3;

import java.util.Objects;

public class Elem {
	private String key;
	private Object value;
	
	public Elem(String key, Object value) {
		this.key = key;
		this.value = value;
	}
	
	public String getKey() {
		return this.key;
	}
	public Object getValue() {
		return this.value;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj == null || !(obj instanceof Elem)) {
			return false;
		}
		Elem other = (Elem) obj;
		return this.key.equals(other.getKey()) && this.value.equals(other.getValue());
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(this.key, this.value);
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("(").append(this.key).append(": ").append(this.value.toString()).append(")");
		return sb.toString();
	}
}
