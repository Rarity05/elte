package hu.elte.inf.toduabi;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Main {

	private static final int MAX_ITERATIONS = 100;
	public static void main(String[] args) throws IOException {
		int maxIterations = MAX_ITERATIONS;
		if (args.length == 1) {
			try {
				maxIterations = Integer.parseInt(args[0]);
			} catch (NumberFormatException e) {
				maxIterations = MAX_ITERATIONS;
			}
		}
		
	    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
	    String input;
	    while ((input = in.readLine()) != null && input.length() != 0) {
	    	Lambda lambda = new Lambda();
		    try {
		    	System.out.println("OK: " + lambda.getNormalForm(input, maxIterations));
		    } catch (LexParserException | SyntaxParserException | LambdaNormalizeException e) {
		    	System.out.println("ERROR: " + input);
		    }
	    }
	}

}
