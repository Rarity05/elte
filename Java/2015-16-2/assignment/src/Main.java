import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Main {

	public static void main(String... args) {
		final BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		
		// Timer signal
	    Signal<Integer> timeSignal = Time.every(1, Times.SECONDS);
	    
	    // Time counter signal
	    Signal<Integer> timeFromStart = timeSignal.accumulate((counter, value) -> counter + 1, 0);
	    
	    // Console reader signal
	    final Signal<String> consoleSignal = new Signal<String>();
	    Thread consoleReader = new Thread(new Runnable() {

			@Override
			public void run() {
				String input;
				boolean l = true;
				while (l) {
					try {
						if ((input = in.readLine()) != null && input.length() != 0) {
							consoleSignal.setValue(input);
						}
		 	    	} catch (Exception e) {
		 	    		l = false;
		 	    		e.printStackTrace();
					}
				}
			}
	    	
	    });
	    consoleReader.start();
	    
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
