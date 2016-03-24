package hu.elte.inf.robotics.opencv;

import java.util.ArrayList;
/**
 * 
 * This class represents the output of the OpenCV module.
 * It contains the Robot as a single property (RPoint), and the Boxes and Targets as a list of Points.
 * Also contains the unit / centimeters ratio based on the Robot's hard-coded dimensions.
 * 
 */
public class ImageData {
	private ArrayList<Point> boxes;
	private ArrayList<Point> targets;
	private RPoint robot;
	private int ratio;
	
	public ImageData(RPoint robot, ArrayList<Point> boxes, ArrayList<Point> targets, int ratio) {
		if (boxes == null || targets == null) {
			throw new IllegalArgumentException("Boxes and Targets can't be null.");
		}
		this.robot = robot;
		this.boxes = boxes;
		this.targets = targets;
		this.ratio = ratio;
	}
	
	/**
	 * 
	 * @return RPoint the Robot located in the coordinate system, can be null
	 */
	public RPoint getRobot() {
		return this.robot;
	}
	
	/**
	 * 
	 * @return ArrayList<Point> the list of Boxes located in the coordinate system, can't be null, can be empty.
	 */
	public ArrayList<Point> getBoxes() {
		return this.boxes;
	}
	
	/**
	 * 
	 * @return ArrayList<Point> the list of Targets located in the coordinate system, can't be null, can be empty.
	 */
	public ArrayList<Point> getTargets() {
		return this.targets;
	}
	
	/**
	 * 
	 * @return integer the unit/centimeters ratio based on the hard-coded Robot's dimensions. 
	 */
	public int getRatio() {
		return this.ratio;
	}
}
