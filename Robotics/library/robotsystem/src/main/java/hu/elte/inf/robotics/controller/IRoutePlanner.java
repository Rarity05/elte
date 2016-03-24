package hu.elte.inf.robotics.controller;

import java.util.ArrayList;

import hu.elte.inf.robotics.opencv.ImageData;
/**
 * 
 * RoutePlan interface
 * An implementation of this interface must be able to plan a route based on the informations
 * provided by the OpenCV module (ImageData).
 *
 */
public interface IRoutePlanner {
	/**
	 * getRoutePlan method
	 * Calculates intercepting course from the Robot to a Box.
	 * Calculates course from a Box to a Target.
	 * @param imageData the informations provided by the OpenCV module
	 * @return ArrayList<Command> the route plan in the form of list of commands
	 */
	public ArrayList<Command> getRoutePlan(ImageData imageData);
}
