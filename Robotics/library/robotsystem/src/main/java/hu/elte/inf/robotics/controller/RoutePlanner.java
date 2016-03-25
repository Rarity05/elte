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
	
	/**
	 * Helper class that manages a matrix from CollisionSideWrappers
	 * With this class we can get CollisionSideWrappers in linear time, once the matrix is built.
	 * @author I321357
	 *
	 */
	private static class CollisionSideMatrixHandler {
		volatile private static CollisionSideWrapper[][] matrix;
		private static int dimension = 0;
		
		synchronized public static void buildMatrix(int n) {
			if (inTreshold(dimension - n)) {
				return;
			}
			dimension = n;
			
			// Build the 4 partial matrixes
			CollisionSideWrapper[][] upperLeftMatrix = buildPartialMatrix(new CollisionSideWrapper(SIDE.RIGHT, SIDE.BOTTOM), new CollisionSideWrapper(SIDE.BOTTOM, SIDE.RIGHT));
			CollisionSideWrapper[][] upperRightMatrix = mirrorMatrix(buildPartialMatrix(new CollisionSideWrapper(SIDE.BOTTOM, SIDE.LEFT), new CollisionSideWrapper(SIDE.LEFT, SIDE.BOTTOM)));
			CollisionSideWrapper[][] lowerLeftMatrix = mirrorMatrix(buildPartialMatrix(new CollisionSideWrapper(SIDE.RIGHT, SIDE.TOP), new CollisionSideWrapper(SIDE.TOP, SIDE.RIGHT)));
			CollisionSideWrapper[][] lowerRightMatrix = buildPartialMatrix(new CollisionSideWrapper(SIDE.TOP, SIDE.LEFT), new CollisionSideWrapper(SIDE.LEFT, SIDE.TOP));
			
			// Merges the partial matrixes into a big one
			CollisionSideWrapper[][] partialBigMatrix = mergeMatrixes(upperLeftMatrix, upperRightMatrix, lowerLeftMatrix, lowerRightMatrix);
			
			// Fill the axis and store the complete matrix
			matrix = fillAxis(partialBigMatrix);
		}
		
		/**
		 * getCollisionSides method
		 * gets the CollisionSideWrapper from the matrix
		 * @param x relative x coordinate to center
		 * @param y relative y coordinate to center
		 * @return CollisionSideWrapper the CollisionSideWrapper at the specified coordinate
		 */
		public static CollisionSideWrapper getCollisionSides(int x, int y) {
			if (dimension == 0 || Math.abs(x) >= dimension/2 || Math.abs(y) >= dimension/2) {
				return null;
			}
			
			return matrix[x][y];
		}
		
		/**
		 * inTreshold method
		 * building the matrix is a resource intensive task, so we don't want to perform it each time the build method gets called,
		 * only when there is a significant change in dimension
		 * @param x the change in dimension
		 * @return boolean whether or not we need to rebuild the matrix
		 */
		private static boolean inTreshold(int x) {
			return Math.abs(x) < 2;
		}
		
		/**
		 * Fills the empty axis fields on a matrix
		 * @param partialBigMatrix
		 * @return the complete matrix
		 */
		private static CollisionSideWrapper[][] fillAxis(CollisionSideWrapper[][] partialBigMatrix) {
			CollisionSideWrapper[][] retVal = copyMatrix(partialBigMatrix);
			
			CollisionSideWrapper top = new CollisionSideWrapper(SIDE.TOP, SIDE.TOP);
			CollisionSideWrapper right = new CollisionSideWrapper(SIDE.RIGHT, SIDE.RIGHT);
			CollisionSideWrapper bottom = new CollisionSideWrapper(SIDE.BOTTOM, SIDE.BOTTOM);
			CollisionSideWrapper left = new CollisionSideWrapper(SIDE.LEFT, SIDE.LEFT);
			
			int center = dimension/2;
			for (int i=0; i<dimension; i++) {
				retVal[i][center] = (i < center) ? bottom : top;
				retVal[center][i] = (i < center) ? right : left;
			}
			
			return partialBigMatrix;
		}
		
		/**
		 * Merges 4 partial matrixes into a new one, leaving the axis empty.
		 * @param upperLeftMatrix
		 * @param upperRightMatrix
		 * @param lowerLeftMatrix
		 * @param lowerRightMatrix
		 * @return
		 */
		private static CollisionSideWrapper[][] mergeMatrixes(CollisionSideWrapper[][] upperLeftMatrix,
				CollisionSideWrapper[][] upperRightMatrix, CollisionSideWrapper[][] lowerLeftMatrix,
				CollisionSideWrapper[][] lowerRightMatrix) {
		
			
			return null;
		}
		
		/**
		 * Builds a partial matrix with triangle distribution.
		 * @param lowerMatrixElement
		 * @param upperMatrixElement
		 * @return the partial matrix
		 */
		private static CollisionSideWrapper[][] buildPartialMatrix(CollisionSideWrapper lowerMatrixElement, CollisionSideWrapper upperMatrixElement) {
			int size = dimension/2;
			CollisionSideWrapper[][] retVal = new CollisionSideWrapper[size][size];
			
			for (int i=0; i<size; i++) {
				for (int j=0; j<size; j++) {
					retVal[i][j] = (i <= j) ? upperMatrixElement : lowerMatrixElement;
				}
			}
			
			return retVal;
		}
		
		/**
		 * Mirrors a matrix into a new one, along the X axis (vertical)
		 * @param matrix
		 * @return the mirrored matrix
		 */
		private static CollisionSideWrapper[][] mirrorMatrix(CollisionSideWrapper[][] matrix) {
			int size = matrix.length;
			CollisionSideWrapper[][] retVal = new CollisionSideWrapper[size][size];
			
			for (int i=0; i<size; i++) {
				for (int j=0; j<size; j++) {
					retVal[i][j] = matrix[i][size-j];
				}
			}
			
			return retVal;
		}
		
		/**
		 * Copies a matrix into a new one 
		 * @param matrix
		 * @return the copied matrix
		 */
		private static CollisionSideWrapper[][] copyMatrix(CollisionSideWrapper[][] matrix) {
			int size = matrix.length;
			CollisionSideWrapper[][] retVal = new CollisionSideWrapper[size][size];
			
			for (int i=0; i<size; i++) {
				for (int j=0; j<size; j++) {
					retVal[i][j] = matrix[i][j];
				}
			}
			
			return retVal;
		}
	}
}
