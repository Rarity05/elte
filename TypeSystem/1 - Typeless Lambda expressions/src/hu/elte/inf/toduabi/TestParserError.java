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

@RunWith(Parameterized.class)
public class TestParserError {
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{"\\"},
        	{"\\."},
        	{"\\x."},
        	{"x:x"},
        	{"\\\\x.x"},
        	{"\\x.\\y."},
        	{"()"},
        	{"x)"},
        	{"((\\x.x)"}
       	 });
    }
	private LexParser lexParser;
	private SyntaxParser syntaxParser;
	private String input;
	
	public TestParserError(String input) {
		this.input = input;
	}
	
	@Before
	public void setUp() throws Exception {
		this.lexParser = new LexParser();
		this.lexParser.addItem(new LexParserItem('\\', SharedConstants.LAMBDA));
		for (int i = 'a'; i <= 'z'; i++) {
			this.lexParser.addItem(new LexParserItem(Character.toChars(i)[0], SharedConstants.VARIABLE));
		}
		this.lexParser.addItem(new LexParserItem(' ', SharedConstants.APPLICATION));
		this.lexParser.addItem(new LexParserItem('.', SharedConstants.DOT));
		this.lexParser.addItem(new LexParserItem('(', SharedConstants.OPEN));
		this.lexParser.addItem(new LexParserItem(')', SharedConstants.CLOSE));
		
		this.syntaxParser = new SyntaxParser();
	}

	@Test
	public void testCorrect() {
		try {
			ArrayList<LexParserItem> tokens = this.lexParser.parse(this.input);
			ILambdaExpression expression = this.syntaxParser.parse(tokens);
			fail("lucky input got through: " + this.input + " -> " + expression.toString());
		} catch (LexParserException | SyntaxParserException e) {
			return; // Success.
		}
	}

}
