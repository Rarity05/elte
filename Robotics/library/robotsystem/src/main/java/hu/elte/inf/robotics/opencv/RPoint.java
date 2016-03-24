package hu.elte.inf.robotics.opencv;
/**
 * 
 * This class represents a Robot shape in a coordinate system
 * The angle property is the angle (degree) between the relative east and the direction the Robot is facing.
 *
 */
public class RPoint extends Point {
	private int angle;
	
	public RPoint(int x, int y, int angle) {
		super(x, y);
		this.angle = angle;
	}
	
	public int getAngle() {
		return this.angle;
	}
}
