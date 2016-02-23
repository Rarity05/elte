package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.List;

public class LexParser {
	private List<LexParserItem> items;
	
	public LexParser() {
		this.items = new ArrayList<LexParserItem>();
	}
	public boolean addItem(LexParserItem item) {
		if (item == null || this.items.contains(item)) {
			return false;
		}
		this.items.add(item);
		return true;
	}
	public LexParserItem parse(char c) {
		for (LexParserItem item : this.items) {
			if (item.getToken() == c) {
				return item;
			}
		}
		return null;
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
}
