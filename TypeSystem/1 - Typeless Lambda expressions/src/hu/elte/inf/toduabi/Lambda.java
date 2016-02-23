package hu.elte.inf.toduabi;

import java.util.ArrayList;

public class Lambda {
	private LexParser lexParser;
	
	public Lambda() {		
		this.lexParser = new LexParser();
		this.lexParser.addItem(new LexParserItem('\\', SharedConstants.LAMBDA));
		this.lexParser.addItem(new LexParserItem('x', SharedConstants.VARIABLE));
		this.lexParser.addItem(new LexParserItem('y', SharedConstants.VARIABLE));
		this.lexParser.addItem(new LexParserItem('z', SharedConstants.VARIABLE));
		this.lexParser.addItem(new LexParserItem(' ', SharedConstants.APPLICATION));
		this.lexParser.addItem(new LexParserItem('.', SharedConstants.DOT));
		this.lexParser.addItem(new LexParserItem('(', SharedConstants.OPEN));
		this.lexParser.addItem(new LexParserItem(')', SharedConstants.CLOSE));
	}
	
	public String getNormalForm(String input, int maxIterations) throws LexParserException, SyntaxParserException {
		ArrayList<LexParserItem> tokens = this.lexParser.parse(input);
		return tokens.toString();
	}
	public String getNormalForm(String input) throws LexParserException, SyntaxParserException {
		return this.getNormalForm(input, 0);
	}
}
