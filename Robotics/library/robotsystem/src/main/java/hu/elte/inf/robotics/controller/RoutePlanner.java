package hu.elte.inf.robotics.controller;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Optional;
import java.util.stream.Stream;

import hu.elte.inf.robotics.opencv.*;

/**
 * @author I321357
 *
 */
public class RoutePlanner implements IRoutePlanner {
	
	public RoutePlanner() {
		
	}
	
	/**
	 * getRoutePlan method
	 * Calculates intercepting course from the Robot to a Box.
	 * Calculates course from a Box to a Target.
	 * @param imageData the informations provided by the OpenCV module
	 * @return ArrayList<Command> the route plan in the form of list of commands
	 */
	@Override
	public ArrayList<Command> getRoutePlan(ImageData imageData) {
		if (imageData.getRobot() == null || imageData.getBoxes().size() == 0 || imageData.getTargets().size() == 0) {
			return new ArrayList<Command>();
		}
		
		RPoint _robot = imageData.getRobot();
		Point _target = imageData.getTargets().get(0);
		Point _box = getClosestBoxToTarget(imageData.getBoxes(), _target);
		
		return null;
	}

	/**
	 * getClosestBoxToTarget method
	 * This method determines which box coordinate is the closest to the target coordinate.
	 * @param boxes The list of box coordinates provided by the OpenCV module
	 * @param target The selected target
	 * @return Point The closest box coordinate to the target, can't be null
	 */
	private Point getClosestBoxToTarget(ArrayList<Point> boxes, Point target) {
		if (boxes == null || boxes.isEmpty() || target == null) {
			throw new IllegalArgumentException("Boxes and Target can't be null or empty");
		}
		
		// Create a stream from the Box list
		Stream<Point> boxStream = boxes.stream();
				
		// Measure each Box's distance from the selected Target
		// list {(x,y), (x,y)} -> list {{(x,y), distance}, {(x,y), distance}}
		Stream<PointDistanceWrapper> distanceStream = boxStream.map(point -> {
			int distance = Math.abs(point.getX() - target.getX()) + Math.abs(point.getY() - target.getY());
			return new PointDistanceWrapper(point, distance);
		});
				
		// Select the closest Box to the Target
		final Comparator<PointDistanceWrapper> comparator = (p1, p2) -> Integer.compare( p1.getDistance(), p2.getDistance());
		Optional<PointDistanceWrapper> wrappedOptMinBox = distanceStream.min(comparator);
				
		// Minimum method returns optional, we must unwrap the value
		if (wrappedOptMinBox.isPresent()) {
			PointDistanceWrapper wrappedMinBox = wrappedOptMinBox.get();
			return wrappedMinBox.getPoint();
		} else {
			// Should only happen when the Box stream is empty
			throw new RuntimeException("Optional has no value");
		}
	}

	/**
	 * Private inner wrapper class
	 * Holds a point, and it's distance from an other point.
	 * Used in the getClosestBoxToTarget method
	 *
	 */
	private class PointDistanceWrapper {
		private Point point;
		private int distance;
		
		public PointDistanceWrapper(Point point, int distance) {
			this.point = point;
			this.distance = distance;
		}
		
		public Point getPoint() {
			return this.point;
		}
		
		public int getDistance() {
			return this.distance;
		}

	}
	/**
	 * Wrapper class for SIDE elements
	 * We can approach a target from two different directions.
	 * One is the preferred, other is the optional.
	 *
	 */
	private static class CollisionSideWrapper {
		private SIDE preferred;
		private SIDE optional;
		
		public CollisionSideWrapper(SIDE preferred, SIDE optional) {
			this.preferred = preferred;
			this.optional = optional;
		}
		
		public SIDE getPreferred() {
			return this.preferred;
		}
		
		public SIDE getOptional() {
			return this.optional;
		}
	}
	
	/**
	 * 
	 * Represents a side of a surface
	 *
	 */
	private enum SIDE {
		TOP, RIGHT, BOTTOM, LEFT
	}
}
