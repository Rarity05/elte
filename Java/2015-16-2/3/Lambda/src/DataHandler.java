package gyak3;

import java.util.ArrayList;

public class DataHandler {
	
	private ArrayList<Elem> db = new ArrayList<Elem>();
	
	public void insert(ICreator creator) {
		Elem elem = creator.create();
		// TODO: check if already exists
		db.add(elem);
	}
	
	public ArrayList<Elem> query(ISelector selector) {
		ArrayList<Elem> retVal = new ArrayList<Elem>();
		for (int i = 0; i < db.size(); i++) {
			if (selector.select(db.get(i))) {
				retVal.add(db.get(i));
			}
		}
		return retVal;
	}
	
	public void transform(ISelector selector, ITransformer transformer) {
		ArrayList<Elem> queVal = query(selector);
		for (int i = 0; i < queVal.size(); i++) {
			// equal?
			int idx = db.indexOf(queVal.get(i));
			db.add(idx, transformer.transform(queVal.get(i)));
		}
	}
}
