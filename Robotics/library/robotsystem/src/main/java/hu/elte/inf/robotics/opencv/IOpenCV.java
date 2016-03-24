package hu.elte.inf.robotics.opencv;

import java.awt.Image;
/**
 * 
 * OpenCV interface
 * An implementation of this interface must be able to process an image,
 * and provide the necessary shape informations.
 *
 */
public interface IOpenCV {
	/**
	 * Image processing method
	 * @param image the provided image
	 * @return ImageData information about the Robot's, Boxes' and Targets' location
	 */
	public ImageData processImage(Image image);
}
