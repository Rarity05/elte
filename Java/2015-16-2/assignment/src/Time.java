
public class Time {
	
	/**
	 * Creates a Signal that emits a value every X milliseconds
	 * @param delay
	 * @param multiplier
	 * @return
	 */
	public static Signal<Integer> every(int delay, Times multiplier) {
		Signal<Integer> signal = new Signal<Integer>();
		signal.setValue(0);
		final int timeout = delay * multiplier.getValue();
		
		Thread myThread = new Thread(new Runnable() {

			@Override
			public void run() {
				boolean l = true;
				while (l) {
					signal.setValue(signal.getValue());
					try {
						Thread.sleep(timeout);
					} catch (InterruptedException e) {
						l = false;
						e.printStackTrace();
					}
				}
			}
			
		});
		myThread.start();
		
		return signal;
	}
}
