package hu.elte.inf.toduabi;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Main {

	public static void main(String[] args) throws IOException {
		
	    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
	    String input;
	    while ((input = in.readLine()) != null && input.length() != 0) {
		    try {
		    	IType type = TypeCheck.parseAndCheck(input);
		    	System.out.println("OK: " + type.toString());
		    } catch (Exception e) {
		    	System.out.println("ERROR: " + input);
		    }
	    }
	}

}
