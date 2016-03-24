package hu.elte.inf.robotics.controller;
/**
 * 
 * Enumeration type for possible control commands.
 * TURN - indicates the Robot has to execute a turn maneuver.
 * FORWARD - the Robot has to accelerate.
 * BACKWRAEDS - the Robot has to accelerate backwards.
 * STOP - the Robot has to stop.
 *
 */
public enum CommandType {
	TURN, FORWARD, BACKWARDS, STOP
}
