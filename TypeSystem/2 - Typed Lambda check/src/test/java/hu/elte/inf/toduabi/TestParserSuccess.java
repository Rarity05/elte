package hu.elte.inf.toduabi;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.stream.Stream;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class TestParserSuccess {
	
	/**
	 * We can't create LexParser.Item outside LexParser,
	 * so we need a wrapper class for testing.
	 * @author I321357
	 *
	 */
	private class Wrapper {
		private String token;
		private LexParser.Type type;
		
		public Wrapper(String token, LexParser.Type type) {
			this.token = token;
			this.type = type;
		}
		
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
			if (!(_other instanceof Wrapper)) {
				return false;
			}
			
			Wrapper other = (Wrapper) _other; 
			return this.token.equals(other.getToken()) && this.type.equals(other.getType());
		}
		
		public String getToken() {
			return this.token;
		}
		
		public LexParser.Type getType() {
			return this.type;
		}
	}
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{"x:Bool |- (\\y:Nat.x x): Nat -> Bool"}
       	 });
    }
	private LexParser lexParser;
	private String input;
	
	public TestParserSuccess(String input) {
		this.input = input;
	}
	
	@Before
	public void setUp() throws Exception {
		this.lexParser = new LexParser();	
	}

	@Test
	public void testCorrect() {
		ArrayList<Wrapper> r1 = new ArrayList<Wrapper>();
		r1.add(new Wrapper("x", LexParser.Type.VARIABLE));
		r1.add(new Wrapper(":", LexParser.Type.COLON));
		r1.add(new Wrapper("Bool", LexParser.Type.TYPE));
		r1.add(new Wrapper("|-", LexParser.Type.CONTEXT));
		r1.add(new Wrapper("(", LexParser.Type.OPEN));
		r1.add(new Wrapper("\\", LexParser.Type.LAMBDA));
		r1.add(new Wrapper("y", LexParser.Type.VARIABLE));
		r1.add(new Wrapper(":", LexParser.Type.COLON));
		r1.add(new Wrapper("Nat", LexParser.Type.TYPE));
		r1.add(new Wrapper(".", LexParser.Type.DOT));
		r1.add(new Wrapper("x", LexParser.Type.VARIABLE));
		r1.add(new Wrapper(" ", LexParser.Type.APPLICATION));
		r1.add(new Wrapper("x", LexParser.Type.VARIABLE));
		r1.add(new Wrapper(")", LexParser.Type.CLOSE));
		r1.add(new Wrapper(":", LexParser.Type.COLON));
		r1.add(new Wrapper("Nat", LexParser.Type.TYPE));
		r1.add(new Wrapper("->", LexParser.Type.ARROW));
		r1.add(new Wrapper("Bool", LexParser.Type.TYPE));
		
		try {
			ArrayList<LexParser.Item> tokens = this.lexParser.parse(this.input);
			Stream<LexParser.Item> stream = tokens.stream();
			Stream<Wrapper> mStream = stream.map(item -> {
				return new Wrapper(item.getToken(), item.getType());
			});
			
			Object[] arr1 = r1.toArray();
			Object[] arr2 = mStream.toArray();
			assertTrue(Arrays.equals(arr1, arr2));
		} catch (LexParserException e) {
			fail(e.getLocalizedMessage());
		}
	}

}
