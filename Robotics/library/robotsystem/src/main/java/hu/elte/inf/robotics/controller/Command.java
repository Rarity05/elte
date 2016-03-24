package hu.elte.inf.robotics.controller;
/**
 * 
 * This class represents a single command that the Robot can take.
 *
 */
public class Command {
	private CommandType type;
	private int value;
	
	public Command(CommandType type, int value) {
		this.type = type;
		this.value = value;
	}
	
	public CommandType getType() {
		return this.type;
	}
	
	public int getValue() {
		return this.value;
	}
}
