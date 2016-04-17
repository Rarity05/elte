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
public class TestParsers_TypeParser {
	
	private static ArrayList<Parsers.LexItem> T1_raw;
	private static ArrayList<Parsers.LexItem> T2_raw;
	private static ArrayList<Parsers.LexItem> T3_raw;
	
	private static IType T1_type;
	private static IType T2_type;
	private static IType T3_type;
	
	static {
		T1_raw = new ArrayList<Parsers.LexItem>();
		T1_raw.add(new LexItem("Nat", Type.TYPE));
		T1_raw.add(new LexItem("Arrow", Type.ARROW));
		T1_raw.add(new LexItem("Nat", Type.TYPE));
		T1_raw.add(new LexItem("Arrow", Type.ARROW));
		T1_raw.add(new LexItem("Bool", Type.TYPE));
		T1_type = new ArrowType(new SingleType("Nat"), new ArrowType(new SingleType("Nat"), new SingleType("Bool")));
		
		T2_raw = new ArrayList<Parsers.LexItem>();
		T2_raw.add(new LexItem("Open", Type.OPEN));
		T2_raw.add(new LexItem("Nat", Type.TYPE));
		T2_raw.add(new LexItem("Arrow", Type.ARROW));
		T2_raw.add(new LexItem("Nat", Type.TYPE));
		T2_raw.add(new LexItem("Close", Type.CLOSE));
		T2_raw.add(new LexItem("Arrow", Type.ARROW));
		T2_raw.add(new LexItem("Bool", Type.TYPE));
		T2_type = new ArrowType(new ArrowType(new SingleType("Nat"), new SingleType("Nat")), new SingleType("Bool"));
		
		T3_raw = new ArrayList<Parsers.LexItem>();
		T3_raw.add(new LexItem("Nat", Type.TYPE));
		T3_raw.add(new LexItem("Arrow", Type.ARROW));
		T3_raw.add(new LexItem("Open", Type.OPEN));
		T3_raw.add(new LexItem("Bool", Type.TYPE));
		T3_raw.add(new LexItem("Arrow", Type.ARROW));
		T3_raw.add(new LexItem("Open", Type.OPEN));
		T3_raw.add(new LexItem("Nat", Type.TYPE));
		T3_raw.add(new LexItem("Arrow", Type.ARROW));
		T3_raw.add(new LexItem("Nat", Type.TYPE));
		T3_raw.add(new LexItem("Close", Type.CLOSE));
		T3_raw.add(new LexItem("Arrow", Type.ARROW));
		T3_raw.add(new LexItem("Nat", Type.TYPE));
		T3_raw.add(new LexItem("Close", Type.CLOSE));
		T3_raw.add(new LexItem("Arrow", Type.ARROW));
		T3_raw.add(new LexItem("Bool", Type.TYPE));
		T3_type = new ArrowType(new SingleType("Nat"), new ArrowType(new ArrowType(new SingleType("Bool"), new ArrowType(new ArrowType(new SingleType("Nat"), new SingleType("Nat")), new SingleType("Nat"))), new SingleType("Bool")));
		
	}
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{T1_raw, T1_type},
        	{T2_raw, T2_type},
        	{T3_raw, T3_type}
       	 });
    }
    
	private ArrayList<Parsers.LexItem> input;
	private IType type;
	
	public TestParsers_TypeParser(ArrayList<Parsers.LexItem> input, IType type) {
		this.input = input;
		this.type = type;
	}
	
	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testCorrect() {
		
		try {
			IType rType = Parsers.typeParser.parse(this.input);
			assertTrue(rType.equals(this.type));
		} catch (Exception e) {
			fail(e.getLocalizedMessage());
		}
		
	}
}
