package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class LexParser {
	public enum Type {
		LAMBDA, VARIABLE, APPLICATION, DOT, OPEN, CLOSE, ARROW, COLON, CONTEXT, TYPE
	}
	private List<Item> items;
	
	public LexParser() {
		this.items = new ArrayList<Item>();
		this.items.add(new Item("\\", Type.LAMBDA));
		for (int i = 'a'; i <= 'z'; i++) {
			this.items.add(new Item(Character.toString(Character.toChars(i)[0]), Type.VARIABLE));
		}
		this.items.add(new Item(" ", Type.APPLICATION));
		this.items.add(new Item(".", Type.DOT));
		this.items.add(new Item("(", Type.OPEN));
		this.items.add(new Item(")", Type.CLOSE));
		
		this.items.add(new Item("->", Type.ARROW));
		this.items.add(new Item(":", Type.COLON));
		this.items.add(new Item("|-", Type.CONTEXT));
		
		this.items.add(new Item("Bool", Type.TYPE));
		this.items.add(new Item("Nat", Type.TYPE));
	}
	
	/**
	 * Parses a String input into Item array
	 * @param input
	 * @return ArrayList<Item> the parsed list
	 * @throws LexParserException
	 */
	public ArrayList<Item> parse(String input) throws LexParserException {
		ArrayList<Item> retVal = new ArrayList<Item>(); 
		for (int i = 0; i < input.length(); /*no-operation*/) {
			String subStr = input.substring(i);
			Item token = this._parse(subStr);
			if (token == null) {
				throw new LexParserException("Not recognized from here: " + subStr);
			}
			retVal.add(token);
			i += token.getToken().length();
		}
		return filterInput(retVal);
	}
	
	/**
	 * Parses the next token from the input
	 * @param input the substring remaining from the input
	 * @return Item, the next recognized token, can be null
	 */
	private Item _parse(String input) {
		String subStr;
		String token;
		for (Item item : this.items) {
			token = item.getToken();
			subStr = input.substring(0, token.length());
			if (token.equals(subStr)) {
				return item;
			}
		}
		return null;
	}
	
	/**
	 * Filters all SPACE characters before and after a FILTER elements
	 * @param input
	 * @return
	 */
	private ArrayList<Item> filterInput(ArrayList<Item> items) {
		HashSet<Type> filters = new HashSet<Type>();
		filters.add(Type.ARROW);
		filters.add(Type.CONTEXT);
		filters.add(Type.COLON);
		
		ArrayList<Item> retVal = new ArrayList<Item>();
		for (int i = 0; i < items.size(); i++) {
			Item item = items.get(i);
			if (item.getType() == Type.APPLICATION) {
				try {
					if (filters.contains(items.get(i-1).getType()) || filters.contains(items.get(i+1).getType())) {
						continue;
					} else {
						retVal.add(item);
					}
				} catch (Exception e) {
					retVal.add(item);
				}
			} else {
				retVal.add(item);
			}
		}
		return retVal;
	}
	
	/**
	 * Inner class, representing a lexical Item
	 * @author I321357
	 *
	 */
	public class Item {
		private String token;
		private Type type;
		
		public Item(String token, Type type) {
			this.token = token;
			this.type = type;
		}
		
		public String getToken() {
			return this.token;
		}
		public Type getType() {
			return this.type;
		}
	}
}
