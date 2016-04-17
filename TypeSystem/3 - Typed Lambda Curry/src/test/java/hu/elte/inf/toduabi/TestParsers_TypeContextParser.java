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
public class TestParsers_TypeContextParser {
	
	private static ArrayList<Parsers.LexItem> T1_raw;
	private static ArrayList<Parsers.LexItem> T2_raw;
	private static ArrayList<Parsers.LexItem> T3_raw;
	
	private static HashSet<LambdaVariable> T1_typeSet;
	private static HashSet<LambdaVariable> T2_typeSet;
	private static HashSet<LambdaVariable> T3_typeSet;
	
	private static TypeContext T1_typeContext;
	private static TypeContext T2_typeContext;
	private static TypeContext T3_typeContext;
	
	static {
		T1_raw = new ArrayList<Parsers.LexItem>();
		T1_raw.add(new LexItem("x", Type.VARIABLE));
		T1_raw.add(new LexItem(":", Type.COLON));
		T1_raw.add(new LexItem("Nat", Type.TYPE));
		T1_typeSet = new HashSet<LambdaVariable>();
		T1_typeSet.add(new LambdaVariable('x', new SingleType("Nat")));
		T1_typeContext = new TypeContext(T1_typeSet);
		
		T2_raw = new ArrayList<Parsers.LexItem>();
		T2_raw.add(new LexItem("x", Type.VARIABLE));
		T2_raw.add(new LexItem(":", Type.COLON));
		T2_raw.add(new LexItem("Nat", Type.TYPE));
		T2_raw.add(new LexItem(",", Type.COMMA));
		T2_raw.add(new LexItem("y", Type.VARIABLE));
		T2_raw.add(new LexItem(":", Type.COLON));
		T2_raw.add(new LexItem("Nat", Type.TYPE));
		T2_raw.add(new LexItem("Arrow", Type.ARROW));
		T2_raw.add(new LexItem("Bool", Type.TYPE));
		T2_typeSet = new HashSet<LambdaVariable>();
		T2_typeSet.add(new LambdaVariable('x', new SingleType("Nat")));
		T2_typeSet.add(new LambdaVariable('y', new ArrowType(new SingleType("Nat"), new SingleType("Bool"))));
		T2_typeContext = new TypeContext(T2_typeSet);
		
		T3_raw = new ArrayList<Parsers.LexItem>();
		T3_typeSet = new HashSet<LambdaVariable>();
		T3_typeContext = new TypeContext(T3_typeSet);
		
	}
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{T1_raw, T1_typeContext},
        	{T2_raw, T2_typeContext},
        	{T3_raw, T3_typeContext}
       	 });
    }
    
	private ArrayList<Parsers.LexItem> input;
	private TypeContext typeContext;
	
	public TestParsers_TypeContextParser(ArrayList<Parsers.LexItem> input, TypeContext typeContext) {
		this.input = input;
		this.typeContext = typeContext;
	}
	
	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testCorrect() {
		
		try {
			TypeContext rTypeContext = Parsers.typeContextParser.parse(this.input);
			assertTrue(rTypeContext.getVariables().equals(this.typeContext.getVariables()));
		} catch (Exception e) {
			fail(e.getLocalizedMessage());
		}
		
	}
}
