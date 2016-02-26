package hu.elte.inf.toduabi;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class TestNormalizeError {
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{"\\x."},
        	{"\\x.y"},
        	{"(\\x.x x) (\\x.x x)"}
       	 });
    }

	private Lambda lambda;
	private String input;
	
	public TestNormalizeError(String input) {
		this.input = input;
	}
	
	@Before
	public void setUp() throws Exception {
		this.lambda = new Lambda();
	}

	@Test
	public void testCorrect() {
		try {
			String normalized = lambda.getNormalForm(this.input, 100);
			fail("lucky input got through: " + this.input + " -> " + normalized);
		} catch (LexParserException | SyntaxParserException | LambdaNormalizeException e) {
			return; // Success.
		}
	}

}
