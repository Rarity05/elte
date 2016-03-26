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
		if (imageData == null) {
			throw new IllegalArgumentException("ImageData can't be null");
		}
		
		if (imageData.getRobot() == null || imageData.getBoxes().isEmpty() || imageData.getTargets().isEmpty()) {
			return new ArrayList<Command>();
		}
		
		// Get the robot
		RPoint _robot = imageData.getRobot();
		
		// Select the most upper-left target, which is the first element of target array
		Point _target = imageData.getTargets().get(0);
		
		// Select the closest box to the selected target
		Point _box = getClosestBoxToTarget(imageData.getBoxes(), _target);
		
		// Calculate which side of the box to push towards the target first
		CollisionSideWrapper collisionToTarget = getCollisionSides(_box, _target);
		
		// Calculate which side of the box is more accessible to the robot (considering the target) and
		// calculate which direction to approach to it
		CollisionSideWrapper collisionToBox = CollisionSideWrapper.mirror(getCollisionSides(_box, _robot));
		CollisionSideWrapper directionsToBox = directionsFromCollisionSides(collisionToBox, collisionToTarget);
		
		// Create the ArrayList which will hold the RoutePlan as the return value of this method
		ArrayList<Command> retVal = new ArrayList<Command>();
		
		// Calculate the initial turn command, which will face the Robot in the correct direction
		Command initialTurn = getTurnCommand(_robot, directionsToBox.getOptional());
		retVal.add(initialTurn);
		
		return null;
	}
	
	/**
	 * Returns the Point located on the given Point's side.
	 * @param point
	 * @param side
	 * @return the Point located on the given Point's side.
	 */
	private Point getSidePoint(Point point, SIDE side) {
		switch (side) {
			case TOP: return new Point(point.getX()-1, point.getY());
			case RIGHT: return new Point(point.getX(), point.getY()+1);
			case BOTTOM: return new Point(point.getX()+1, point.getY());
			case LEFT: return new Point(point.getX(), point.getY()-1);
			default: /* not possible */ throw new RuntimeException("Undefined SIDE");
		}
	}

	/**
	 * Creates a Turn Command into the given SIDE direction
	 * @param robot
	 * @param direction
	 * @return Turn Command containing the degree of turn
	 */
	private Command getTurnCommand(RPoint robot, SIDE direction) {
		int from = robot.getAngle(), to = SIDE.toDegree(direction);
		boolean turnRight = ((from-to) % 360 >= 180);
		int turnDegree = turnRight ?  (to-from) % 360 : (from-to) % 360 * -1;
		
		Command.Type type = Command.Type.TURN;
		return new Command(type, turnDegree);
	}
	
	/**
	 * Creates a Turn Command between two SIDE directions
	 * @param from
	 * @param to
	 * @return Turn Command containing the degree of turn
	 */
	private Command getTurnCommand(SIDE from, SIDE to) {				
		return getTurnCommand(new RPoint(0, 0, SIDE.toDegree(from)), to);
	}

	/**
	 * Calculates which side to collide with the target, and which direction should the object move first. 
	 * @param object
	 * @param target
	 * @return the wrapper containing the collision side as the Preferred SIDE, and the direction as the Optional SIDE
	 */
	private CollisionSideWrapper directionsFromCollisionSides(CollisionSideWrapper object, CollisionSideWrapper target) {
		SIDE targetPref = target.getPreferred(), targetOpt = target.getOptional();
		SIDE objectPref = object.getPreferred(), objectOpt = object.getOptional();
		
		if (targetPref.equals(objectPref) || targetOpt.equals(objectPref)) {
			return new CollisionSideWrapper(objectPref, SIDE.mirror(objectOpt));
		} else if (targetPref.equals(objectOpt) || targetOpt.equals(objectOpt)) {
			return new CollisionSideWrapper(objectOpt, SIDE.mirror(objectPref));
		} else {
			if (objectPref.equals(objectOpt) && objectPref.equals(SIDE.mirror(targetPref))) {
				return new CollisionSideWrapper(targetOpt, targetOpt);
			} else {
				return new CollisionSideWrapper(targetPref, targetPref);
			}
		}
	}

	/**
	 * Calculates which side to collide with the object in order to reach the target.
	 * @param object
	 * @param target
	 * @return
	 */
	private CollisionSideWrapper getCollisionSides(Point object, Point target) {
		int o_x = object.getX(), o_y = object.getY();
		int t_x = target.getX(), t_y = target.getY();
		
		// Target is above
		if (o_x  < t_x) {
			// Target is in the same column
			if (o_y == t_y) {
				return new CollisionSideWrapper(SIDE.BOTTOM, SIDE.BOTTOM);
			}
			// Target is in the left hand side
			else if (o_y < t_y) {
				return (Math.abs(o_x - t_x) >= Math.abs(o_y - t_y)) ? new CollisionSideWrapper(SIDE.BOTTOM, SIDE.LEFT) : new CollisionSideWrapper(SIDE.LEFT, SIDE.BOTTOM);
			}
			// Target is in the right hand side
			else {
				return (Math.abs(o_x - t_x) >= Math.abs(o_y - t_y)) ? new CollisionSideWrapper(SIDE.BOTTOM, SIDE.RIGHT) : new CollisionSideWrapper(SIDE.RIGHT, SIDE.BOTTOM);
			}
		}
		// Target is below
		else if (o_x > t_x) {
			// Target is in the same column
			if (o_y == t_y) {
				return new CollisionSideWrapper(SIDE.TOP, SIDE.TOP);
			}
			// Target is in the left hand side
			else if (o_y < t_y) {
				return (Math.abs(o_x - t_x) >= Math.abs(o_y - t_y)) ? new CollisionSideWrapper(SIDE.TOP, SIDE.LEFT) : new CollisionSideWrapper(SIDE.LEFT, SIDE.TOP);
			}
			// Target is in the right hand side
			else {
				return (Math.abs(o_x - t_x) >= Math.abs(o_y - t_y)) ? new CollisionSideWrapper(SIDE.TOP, SIDE.RIGHT) : new CollisionSideWrapper(SIDE.RIGHT, SIDE.TOP);
			}
		}
		// Target is in the same row
		else {
			// Target is in the same column
			if (o_y == t_y) {
				// Not possible
				throw new RuntimeException("Object is on Target");
			}
			// Target is in the left hand side
			else if (o_y < t_y) {
				return new CollisionSideWrapper(SIDE.RIGHT, SIDE.RIGHT);
			}
			// Target is in the right hand side
			else {
				return new CollisionSideWrapper(SIDE.LEFT, SIDE.LEFT);
			}
			
		}
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
		
		/**
		 * Mirrors the SIDE elements inside the wrapper
		 * @param wrapper
		 * @return the mirrored wrapper
		 */
		public static CollisionSideWrapper mirror(CollisionSideWrapper wrapper) {
			if (wrapper == null) {
				throw new IllegalArgumentException("Wrapper can't be null");
			}
			
			SIDE preferred = SIDE.mirror(wrapper.getOptional());
			SIDE optional = SIDE.mirror(wrapper.getPreferred());
			
			return new CollisionSideWrapper(preferred, optional);
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
		TOP, RIGHT, BOTTOM, LEFT;
		
		public static SIDE mirror(SIDE side) {
			switch (side) {
				case TOP: return BOTTOM;
				case RIGHT: return LEFT;
				case BOTTOM: return TOP;
				case LEFT: return RIGHT;
				default : /* should never happen */ throw new RuntimeException("Undefined SIDE element");
			}
		}
		
		/**
		 * Create a SIDE element from discrete degree
		 * @param degree
		 * @return SIDE element representing the degree
		 */
		public static SIDE fromDiscrateDegree(int degree) {
			int _degree = Math.abs(degree);
			if (_degree != 0 || _degree != 90 || _degree != 180 || _degree != 270 || _degree != 360) {
				throw new IllegalArgumentException("Not a descrete value");
			}
			
			switch (degree) {
				case 0:
				case 360:
				case -360: return TOP;
				case 90:
				case -270: return RIGHT;
				case 180:
				case -180: return BOTTOM;
				case 270:
				case -90: return LEFT;
				default: /* not possible */ throw new RuntimeException("Undefined DEGREE");
			}
		}
		public static int toDegree(SIDE side) {
			switch (side) {
				case TOP: return 360;
				case RIGHT: return 90;
				case BOTTOM: return 180;
				case LEFT: return 270;
				default: /* not possible */ throw new RuntimeException("Undefined SIDE");
			}
		}
	}
}
