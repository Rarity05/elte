package hu.elte.inf.toduabi;

public class LexParserItem {
	private char token;
	private String name;
	
	public LexParserItem(char token, String name) {
		this.token = token;
		this.name = name;
	}
	
	public char getToken() {
		return this.token;
	}
	public String getName() {
		return this.name;
	}
}
