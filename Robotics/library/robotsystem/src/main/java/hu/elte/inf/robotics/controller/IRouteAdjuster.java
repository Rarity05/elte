package hu.elte.inf.robotics.controller;

import java.util.ArrayList;
/**
 * 
 * RouteAdjuster interface
 * An implementation of this interface is Robot specific.
 * The implementation must know the Robot capabilities and adjust the default route-plan as such.
 *
 */
public interface IRouteAdjuster {
	/**
	 * getAdjustedRoutePlan method
	 * Calculates the new, adjusted route plan based on the Robot capabilities.
	 * @param routePlan the default route plan
	 * @return ArrayList<Command> the new, adjusted route plan
	 */
	public ArrayList<Command> getAdjustedRoutePlan(ArrayList<Command> routePlan);
}
