package hu.elte.inf.robotics.opencv;
/**
 * 
 * This class represents a point in a coordinate system.
 *
 */
public class Point {
	private final static int MAX = 200;
	private int x;
	private int y;
	
	public Point(int x, int y) {
		this.x = x;
		this.y = y;
	}
	
	public int getX() {
		return this.x;
	}
	public int getY() {
		return this.y;
	}
	
	@Override
	public int hashCode(){
        return this.x * MAX + this.y;
    }
	
	@Override
	public boolean equals(Object _other) {
		if (!(_other instanceof Point)) {
			return false;
		}
		
		Point other = (Point) _other; 
		return this.x == other.getX() && this.y == other.getY();
	}
	
	@Override
	public String toString() {
		return "( " + this.x + " , " + this.y + " )";
	}
}
