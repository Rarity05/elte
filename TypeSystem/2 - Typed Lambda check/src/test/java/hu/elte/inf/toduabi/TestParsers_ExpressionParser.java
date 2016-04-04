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

import hu.elte.inf.toduabi.Parsers.*;

@RunWith(Parameterized.class)
public class TestParsers_ExpressionParser {
	
	private static ArrayList<Parsers.LexItem> T1_raw;
	private static ArrayList<Parsers.LexItem> T2_raw;
	
	private static ILambdaExpression T1_expression;
	private static ILambdaExpression T2_expression;
	
	static {
		T1_raw = new ArrayList<Parsers.LexItem>();
		T1_raw.add(new LexItem("x", Type.VARIABLE));
		T1_raw.add(new LexItem(" ", Type.APPLICATION));
		T1_raw.add(new LexItem("y", Type.VARIABLE));
		T1_expression = new LambdaApplication(new LambdaVariable('x', null), new LambdaVariable('y', null));
		
		T2_raw = new ArrayList<Parsers.LexItem>();
		T2_raw.add(new LexItem("\\", Type.LAMBDA));
		T2_raw.add(new LexItem("x", Type.VARIABLE));
		T2_raw.add(new LexItem(":", Type.COLON));
		T2_raw.add(new LexItem("Nat", Type.TYPE));
		T2_raw.add(new LexItem("->", Type.ARROW));
		T2_raw.add(new LexItem("Bool", Type.TYPE));
		T2_raw.add(new LexItem(".", Type.DOT));
		T2_raw.add(new LexItem("x", Type.VARIABLE));
		T2_raw.add(new LexItem(" ", Type.APPLICATION));
		T2_raw.add(new LexItem("(", Type.OPEN));
		T2_raw.add(new LexItem("x", Type.VARIABLE));
		T2_raw.add(new LexItem(" ", Type.APPLICATION));
		T2_raw.add(new LexItem("y", Type.VARIABLE));
		T2_raw.add(new LexItem(")", Type.CLOSE));
		T2_expression = new LambdaAbstraction(new LambdaVariable('x', new ArrowType(new SingleType("Nat"), new SingleType("Bool"))), new LambdaApplication(new LambdaVariable('x', null),new LambdaApplication(new LambdaVariable('x', null), new LambdaVariable('y', null))));
		
	}
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{T1_raw, T1_expression},
        	{T2_raw, T2_expression}
       	 });
    }
    
	private ArrayList<Parsers.LexItem> input;
	private ILambdaExpression expression;
	
	public TestParsers_ExpressionParser(ArrayList<Parsers.LexItem> input, ILambdaExpression expression) {
		this.input = input;
		this.expression = expression;
	}
	
	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testCorrect() {
		
		try {
			ILambdaExpression rExpression = Parsers.expressionParser.parse(this.input);
			assertTrue(rExpression.equals(this.expression));
		} catch (Exception e) {
			fail(e.getLocalizedMessage());
		}
		
	}
}
