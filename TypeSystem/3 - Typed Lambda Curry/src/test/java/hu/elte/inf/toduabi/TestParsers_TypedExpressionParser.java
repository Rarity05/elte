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

import hu.elte.inf.toduabi.Parsers.*;

@RunWith(Parameterized.class)
public class TestParsers_TypedExpressionParser {
	
	private static ArrayList<Parsers.LexItem> T1_raw;
	private static ArrayList<Parsers.LexItem> T2_raw;
	
	private static TypeContext T1_typeContext;
	private static TypeContext T2_typeContext;
	
	private static ILambdaExpression T1_expression;
	private static ILambdaExpression T2_expression;
	
	private static IType T1_type;
	private static IType T2_type;
	
	static {
		T1_raw = new ArrayList<Parsers.LexItem>();
		T1_raw.add(new LexItem("|-", Type.CONTEXT));
		T1_raw.add(new LexItem("x", Type.VARIABLE));
		T1_raw.add(new LexItem(" ", Type.APPLICATION));
		T1_raw.add(new LexItem("y", Type.VARIABLE));
		T1_raw.add(new LexItem(":", Type.COLON));
		T1_raw.add(new LexItem("Bool", Type.TYPE));
		HashSet<LambdaVariable> T1_typeContextSet = new HashSet<LambdaVariable>();
		T1_typeContext = new TypeContext(T1_typeContextSet);
		T1_expression = new LambdaApplication(new LambdaVariable('x', null), new LambdaVariable('y', null));
		T1_type = new SingleType("Bool");
		
		T2_raw = new ArrayList<Parsers.LexItem>();
		T2_raw.add(new LexItem("x", Type.VARIABLE));
		T2_raw.add(new LexItem(":", Type.COLON));
		T2_raw.add(new LexItem("Bool", Type.TYPE));
		T2_raw.add(new LexItem(",", Type.COMMA));
		T2_raw.add(new LexItem("y", Type.VARIABLE));
		T2_raw.add(new LexItem(":", Type.COLON));
		T2_raw.add(new LexItem("Bool", Type.TYPE));
		T2_raw.add(new LexItem("|-", Type.CONTEXT));
		T2_raw.add(new LexItem("x", Type.VARIABLE));
		T2_raw.add(new LexItem(" ", Type.APPLICATION));
		T2_raw.add(new LexItem("y", Type.VARIABLE));
		T2_raw.add(new LexItem(":", Type.COLON));
		T2_raw.add(new LexItem("Bool", Type.TYPE));
		HashSet<LambdaVariable> T2_typeContextSet = new HashSet<LambdaVariable>();
		T2_typeContextSet.add(new LambdaVariable('x', new SingleType("Bool")));
		T2_typeContextSet.add(new LambdaVariable('y', new SingleType("Bool")));
		T2_typeContext = new TypeContext(T2_typeContextSet);
		T2_expression = new LambdaApplication(new LambdaVariable('x', null), new LambdaVariable('y', null));
		T2_type = new SingleType("Bool");
		
	}
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{T1_raw, T1_typeContext, T1_expression, T1_type},
        	{T2_raw, T2_typeContext, T2_expression, T2_type}
       	 });
    }
    
	private ArrayList<Parsers.LexItem> input;
	private ILambdaExpression expression;
	private TypeContext typeContext;
	private IType type;
	
	public TestParsers_TypedExpressionParser(ArrayList<Parsers.LexItem> input, TypeContext typeContext, ILambdaExpression expression, IType type) {
		this.input = input;
		this.expression = expression;
		this.typeContext = typeContext;
		this.type = type;
	}
	
	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testCorrect() {
		
		try {
			TypedExpression rTypedExpression = Parsers.typedExpressionParser.parse(this.input);
			assertTrue(rTypedExpression.getTypeContext().getVariables().equals(this.typeContext.getVariables()));
			assertTrue(rTypedExpression.getExpression().equals(this.expression));
			assertTrue(rTypedExpression.getType().equals(this.type));
		} catch (Exception e) {
			fail(e.getLocalizedMessage());
		}
		
	}
}
