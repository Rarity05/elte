package hu.elte.inf.toduabi;

import java.util.ArrayList;

public class Lambda {
	private LexParser lexParser;
	private SyntaxParser syntaxParser;
	
	public Lambda() {		
		this.lexParser = new LexParser();		
		this.syntaxParser = new SyntaxParser();
	}
	
	public String getNormalForm(String input, int maxIterations) throws LexParserException, SyntaxParserException {
		ArrayList<LexParserItem> tokens = this.lexParser.parse(input);
		ILambdaExpression expression = this.syntaxParser.parse(tokens);
		return expression.toString();
	}
	public String getNormalForm(String input) throws LexParserException, SyntaxParserException {
		return this.getNormalForm(input, 0);
	}
}
