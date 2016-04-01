package hu.elte.inf.toduabi;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import hu.elte.inf.toduabi.LexParser.Item;
import hu.elte.inf.toduabi.LexParser.Type;

@RunWith(Parameterized.class)
public class TestParsers_LexParser {
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{"x:Bool |- (\\y:Nat.x x): Nat -> Bool"}
       	 });
    }
	private LexParser<LexParser.Item, LexParser.Type> lexParser;
	private String input;
	
	public TestParsers_LexParser(String input) {
		this.input = input;
	}
	
	@Before
	public void setUp() throws Exception {
		this.lexParser = LexParser.createDefault();	
	}

	@Test
	public void testCorrect() {
		ArrayList<LexParser.Item> r1 = new ArrayList<LexParser.Item>();
		r1.add(new LexParser.Item("x", LexParser.Type.VARIABLE));
		r1.add(new LexParser.Item(":", LexParser.Type.COLON));
		r1.add(new LexParser.Item("Bool", LexParser.Type.TYPE));
		r1.add(new LexParser.Item("|-", LexParser.Type.CONTEXT));
		r1.add(new LexParser.Item("(", LexParser.Type.OPEN));
		r1.add(new LexParser.Item("\\", LexParser.Type.LAMBDA));
		r1.add(new LexParser.Item("y", LexParser.Type.VARIABLE));
		r1.add(new LexParser.Item(":", LexParser.Type.COLON));
		r1.add(new LexParser.Item("Nat", LexParser.Type.TYPE));
		r1.add(new LexParser.Item(".", LexParser.Type.DOT));
		r1.add(new LexParser.Item("x", LexParser.Type.VARIABLE));
		r1.add(new LexParser.Item(" ", LexParser.Type.APPLICATION));
		r1.add(new LexParser.Item("x", LexParser.Type.VARIABLE));
		r1.add(new LexParser.Item(")", LexParser.Type.CLOSE));
		r1.add(new LexParser.Item(":", LexParser.Type.COLON));
		r1.add(new LexParser.Item("Nat", LexParser.Type.TYPE));
		r1.add(new LexParser.Item("->", LexParser.Type.ARROW));
		r1.add(new LexParser.Item("Bool", LexParser.Type.TYPE));
		
		try {
			ArrayList<LexParser.Item> tokens = filterInput(this.lexParser.parse(this.input));
			
			Object[] arr1 = r1.toArray();
			Object[] arr2 = tokens.toArray();
			assertTrue(Arrays.equals(arr1, arr2));
		} catch (LexParserException e) {
			fail(e.getLocalizedMessage());
		}
	}
	
	/**
	 * Filters all SPACE characters before and after a FILTER elements
	 * @param input
	 * @return
	 */
	private ArrayList<Item> filterInput(ArrayList<Item> items) {
		HashSet<Type> filters = new HashSet<LexParser.Type>();
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

}
