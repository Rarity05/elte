package hu.elte.inf.robotics;

import java.awt.Image;

/**
 * Image provider interface
 * An implementation of this interface must be able to provide
 * an image from it's world.
 */
public interface IImageProvider {
	/**
	 * Image provider method
	 * @return Image the Image which represents the world
	 */
	public Image provideImage();
}
