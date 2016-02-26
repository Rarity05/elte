package hu.elte.inf.toduabi;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import junit.framework.Assert;

@SuppressWarnings("deprecation")
@RunWith(Parameterized.class)
public class TestNormalizeSuccess {
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{"\\x.x", "(\\x.x)"},
        	{"(\\x.x) (\\x.x)", "(\\x.x)"},
        	{"(\\z.(\\x.\\y.y) z)", "(\\x.(\\y.y))"},
        	{"(\\y.(\\x.\\y.(x y)) y)", "(\\x.x)"},
        	{"(\\f.\\x.f (f x)) (\\f.\\x.f (f x))", "(\\x.(\\y.(x (x (x (x y))))))"}
       	 });
    }
	private Lambda lambda;
	private String input;
	private String output;
	
	public TestNormalizeSuccess(String input, String output) {
		this.input = input;
		this.output = output;
	}
	
	@Before
	public void setUp() throws Exception {
		this.lambda = new Lambda();
	}

	@Test
	public void testCorrect() {
		try {
			String normalized = lambda.getNormalForm(this.input, 100);
			Assert.assertEquals(true, normalized.equals(this.output));
		} catch (LexParserException | SyntaxParserException | LambdaNormalizeException e) {
			fail(e.getMessage());
		}
	}

}
