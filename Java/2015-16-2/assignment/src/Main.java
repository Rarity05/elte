public class Main {

	public static void main(String... args) {		
		// Timer signal
	    Signal<Integer> timeSignal = Time.every(1, Times.SECONDS);
	    
	    // Time counter signal
	    Signal<Integer> timeFromStart = timeSignal.accumulate((counter, value) -> counter + 1, 0);
	    
	    // Console reader signal
	    Signal<String> consoleSignal = SignalBuilder.consoleSignal();
	    
	    // Writing console input to output
	    consoleSignal.subscribe(new ISignalAction<String>() {

			@Override
			public void onSignalChanged(String oldValue, String newValue) {
				System.out.println(newValue);
			}
	    	
	    });
	    
	    // Writing time from start
	    timeFromStart.subscribe(new ISignalAction<Integer>() {

			@Override
			public void onSignalChanged(Integer oldValue, Integer newValue) {
				System.out.println(newValue + " seconds");				
			}
	    	
	    });
	}

}
