package hu.elte.inf.toduabi;

import java.util.Scanner;

public class Main {

	private static final int MAX_ITERATIONS = 100;
	public static void main(String[] args) {
		int maxIterations = MAX_ITERATIONS;
		if (args.length == 1) {
			try {
				maxIterations = Integer.parseInt(args[0]);
			} catch (NumberFormatException e) {
				maxIterations = MAX_ITERATIONS;
			}
		}

	    String input;
	    if (System.console() != null) {
	    	input = System.console().readLine();
	    } else {
	    	Scanner in = new Scanner(System.in);
		    input = in.nextLine();      
		    in.close();
	    }
	    
	    Lambda lambda = new Lambda();
	    String convertedInput = null;
	    try {
	    	convertedInput = SharedConstants.convertConsoleInput(input);
	    	System.out.println("OK: " + lambda.getNormalForm(convertedInput, maxIterations));
	    } catch (LexParserException | SyntaxParserException | LambdaNormalizeException e) {
	    	String errorOutput = (convertedInput == null) ? input : convertedInput;
	    	System.out.println("ERROR: " + errorOutput);
	    }
	}

}
