package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.List;

public class LexParser {
	private List<LexParserItem> items;
	
	public LexParser() {
		this.items = new ArrayList<LexParserItem>();
		this.items.add(new LexParserItem('\\', SharedConstants.LAMBDA));
		for (int i = 'a'; i <= 'z'; i++) {
			this.items.add(new LexParserItem(Character.toChars(i)[0], SharedConstants.VARIABLE));
		}
		this.items.add(new LexParserItem(' ', SharedConstants.APPLICATION));
		this.items.add(new LexParserItem('.', SharedConstants.DOT));
		this.items.add(new LexParserItem('(', SharedConstants.OPEN));
		this.items.add(new LexParserItem(')', SharedConstants.CLOSE));
	}
	
	public ArrayList<LexParserItem> parse(String input) throws LexParserException {
		ArrayList<LexParserItem> retVal = new ArrayList<LexParserItem>(); 
		for (int i = 0; i < input.length(); i++) {
			LexParserItem token = this.parse(input.charAt(i));
			if (token == null) {
				throw new LexParserException("Not recognized: " + input.charAt(i));
			}
			retVal.add(token);
		}
		return retVal;
	}
	
	private LexParserItem parse(char c) {
		for (LexParserItem item : this.items) {
			if (item.getToken() == c) {
				return item;
			}
		}
		return null;
	}
}
