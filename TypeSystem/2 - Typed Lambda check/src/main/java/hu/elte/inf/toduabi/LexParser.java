package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.List;

public class LexParser<T extends ILexicalItem<K>, K> {
	public enum Type {
		LAMBDA, VARIABLE, APPLICATION, DOT, OPEN, CLOSE, ARROW, COLON, CONTEXT, TYPE
	}
	private List<T> items;
	
	public static  LexParser<LexParser.Item, LexParser.Type> createDefault() {
		LexParser<LexParser.Item, LexParser.Type> parser = new LexParser<LexParser.Item, LexParser.Type>();
		parser.addItem(new Item("\\", Type.LAMBDA));
		for (int i = 'a'; i <= 'z'; i++) {
			parser.addItem(new Item(Character.toString(Character.toChars(i)[0]), Type.VARIABLE));
		}
		parser.addItem(new Item(" ", Type.APPLICATION));
		parser.addItem(new Item(".", Type.DOT));
		parser.addItem(new Item("(", Type.OPEN));
		parser.addItem(new Item(")", Type.CLOSE));
		
		parser.addItem(new Item("->", Type.ARROW));
		parser.addItem(new Item(":", Type.COLON));
		parser.addItem(new Item("|-", Type.CONTEXT));
		
		parser.addItem(new Item("Bool", Type.TYPE));
		parser.addItem(new Item("Nat", Type.TYPE));
		
		return parser;
	}
	
	public LexParser() {
		this.items = new ArrayList<T>();
	}
	
	public void addItem(T item) {
		this.items.add(item);
	}
	
	/**
	 * Parses a String input into Item array
	 * @param input
	 * @return ArrayList<Item> the parsed list
	 * @throws LexParserException
	 */
	public ArrayList<T> parse(String input) throws LexParserException {
		ArrayList<T> retVal = new ArrayList<T>(); 
		for (int i = 0; i < input.length(); /*no-operation*/) {
			String subStr = input.substring(i);
			T token = this._parse(subStr);
			if (token == null) {
				throw new LexParserException("Not recognized from here: " + subStr);
			}
			retVal.add(token);
			i += token.getToken().length();
		}
		return retVal;
	}
	
	/**
	 * Parses the next token from the input
	 * @param input the substring remaining from the input
	 * @return Item, the next recognized token, can be null
	 */
	private T _parse(String input) {
		String subStr;
		String token;
		for (T item : this.items) {
			token = item.getToken();
			subStr = input.substring(0, token.length());
			if (token.equals(subStr)) {
				return item;
			}
		}
		return null;
	}
	
	/**
	 * Inner class, representing a lexical Item
	 * @author I321357
	 *
	 */
	public static class Item implements ILexicalItem<Type> {
		private String token;
		private Type type;
		
         @Override
         public int hashCode() {
             int sum = 0;
             for (int i=0; i<this.token.length(); i++) {
                 sum += this.token.charAt(i);
             }
             return sum;
         }

         @Override
         public boolean equals(Object _other) {
             if (!(_other instanceof Item)) {
                 return false;
             }
             
             Item other = (Item) _other; 
             return this.token.equals(other.getToken()) && this.type.equals(other.getType());
         }

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
