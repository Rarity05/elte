package hu.elte.inf.robotics.controller;
/**
 * 
 * This class represents a single command that the Robot can take.
 *
 */
public class Command {
	private Type type;
	private int value;
	
	public Command(Type type, int value) {
		this.type = type;
		this.value = value;
	}
	
	public Type getType() {
		return this.type;
	}
	
	public int getValue() {
		return this.value;
	}
	
	/**
	 * 
	 * Enumeration type for possible control commands.
	 * TURN - indicates the Robot has to execute a turn maneuver.
	 * FORWARD - the Robot has to accelerate.
	 * BACKWRAEDS - the Robot has to accelerate backwards.
	 * STOP - the Robot has to stop.
	 * COLLISION - indicates that a collision event must be allowed.
	 *
	 */
	public static enum Type {
		TURN, FORWARD, BACKWARDS, STOP, COLLISION
	}
}
