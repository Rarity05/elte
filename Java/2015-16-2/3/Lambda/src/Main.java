package gyak3;

public class Main {

	public static void main(String[] args) {
		DataHandler dh = new DataHandler();
		
		dh.insert(() -> new Elem("Key", 1));
		dh.transform(e -> e.getKey().equals("a"), e -> e);
	}

}
