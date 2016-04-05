package hu.elte.inf.toduabi;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import hu.elte.inf.toduabi.Parsers.LexItem;

@RunWith(Parameterized.class)
public class TestParsers_LexParser {
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{"x:Bool |- (\\y:Nat.x x): Nat -> Bool"}
       	 });
    }
	private String input;
	
	public TestParsers_LexParser(String input) {
		this.input = input;
	}
	
	@Before
	public void setUp() throws Exception {	
	}

	@Test
	public void testCorrect() {
		ArrayList<LexItem> r1 = new ArrayList<LexItem>();
		r1.add(new LexItem("x", Parsers.Type.VARIABLE));
		r1.add(new LexItem(":", Parsers.Type.COLON));
		r1.add(new LexItem("Bool", Parsers.Type.TYPE));
		r1.add(new LexItem("|-", Parsers.Type.CONTEXT));
		r1.add(new LexItem("(", Parsers.Type.OPEN));
		r1.add(new LexItem("\\", Parsers.Type.LAMBDA));
		r1.add(new LexItem("y", Parsers.Type.VARIABLE));
		r1.add(new LexItem(":", Parsers.Type.COLON));
		r1.add(new LexItem("Nat", Parsers.Type.TYPE));
		r1.add(new LexItem(".", Parsers.Type.DOT));
		r1.add(new LexItem("x", Parsers.Type.VARIABLE));
		r1.add(new LexItem(" ", Parsers.Type.APPLICATION));
		r1.add(new LexItem("x", Parsers.Type.VARIABLE));
		r1.add(new LexItem(")", Parsers.Type.CLOSE));
		r1.add(new LexItem(":", Parsers.Type.COLON));
		r1.add(new LexItem("Nat", Parsers.Type.TYPE));
		r1.add(new LexItem("->", Parsers.Type.ARROW));
		r1.add(new LexItem("Bool", Parsers.Type.TYPE));
		
		try {
			ArrayList<LexItem> tokens = Parsers.lexParser.parse(this.input); 
			
			Object[] arr1 = r1.toArray();
			Object[] arr2 = tokens.toArray();
			assertTrue(Arrays.equals(arr1, arr2));
		} catch (LexParserException e) {
			fail(e.getLocalizedMessage());
		}
	}

}
