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
public class TestClass_ITypeToString {
	
	private static IType T1_type;
	private static IType T2_type;
	private static IType T3_type;
	
	static {
		T1_type = new ArrowType(new SingleType("Nat"), new ArrowType(new SingleType("Nat"), new SingleType("Bool")));
		T2_type = new ArrowType(new ArrowType(new SingleType("Nat"), new SingleType("Nat")), new SingleType("Bool"));
		T3_type = new ArrowType(new SingleType("Nat"), new ArrowType(new ArrowType(new SingleType("Bool"), new ArrowType(new ArrowType(new SingleType("Nat"), new SingleType("Nat")), new SingleType("Nat"))), new SingleType("Bool")));
	}
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{T1_type, "(Nat -> (Nat -> Bool))"},
        	{T2_type, "((Nat -> Nat) -> Bool)"},
        	{T3_type, "(Nat -> ((Bool -> ((Nat -> Nat) -> Nat)) -> Bool))"}
       	 });
    }
    
	private IType type;
	private String output;
	
	public TestClass_ITypeToString(IType type, String output) {
		this.type = type;
		this.output = output;
	}
	
	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testCorrect() {
		
		if (this.type.toString().equals(this.output)) {
			assertTrue(true);
		} else {
			fail("Expected: " + this.output + " \nGot: " + this.type.toString());
		}
		
	}
}
