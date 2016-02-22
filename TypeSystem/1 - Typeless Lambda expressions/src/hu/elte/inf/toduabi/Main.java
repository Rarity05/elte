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
	    try {
	    	System.out.println("OK: " + lambda.getNormalForm(input, maxIterations));
	    } catch (LexParserException | SyntaxParserException e) {
	    	System.out.println("ERROR: " + input);
	    }
	}

}
