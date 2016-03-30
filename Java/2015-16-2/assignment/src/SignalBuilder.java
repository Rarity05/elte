import java.io.BufferedReader;
import java.io.InputStreamReader;

public class SignalBuilder {

	public static Signal<String> consoleSignal() {
		final BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
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
	    return consoleSignal;
	}

}
