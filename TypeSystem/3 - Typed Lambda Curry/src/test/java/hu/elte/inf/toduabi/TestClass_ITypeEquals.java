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
public class TestClass_ITypeEquals {
	
	private static IType T1_type;
	private static IType T1_type2;
	private static IType T2_type;
	private static IType T2_type2;
	private static IType T3_type;
	private static IType T3_type2;
	
	static {
		T1_type = new ArrowType(new SingleType("Nat"), new ArrowType(new SingleType("Nat"), new SingleType("Bool")));
		T1_type2 = new ArrowType(new SingleType("Nat"), new ArrowType(new SingleType("Nat"), new SingleType("Bool")));
		T2_type = new ArrowType(new ArrowType(new SingleType("Nat"), new SingleType("Nat")), new SingleType("Bool"));
		T2_type2 = new ArrowType(new ArrowType(new SingleType("Nat"), new SingleType("Nat")), new SingleType("Bool"));
		T3_type = new ArrowType(new SingleType("Nat"), new ArrowType(new ArrowType(new SingleType("Bool"), new ArrowType(new ArrowType(new SingleType("Nat"), new SingleType("Nat")), new SingleType("Nat"))), new SingleType("Bool")));
		T3_type2 = new ArrowType(new SingleType("Nat"), new ArrowType(new ArrowType(new SingleType("Bool"), new ArrowType(new ArrowType(new SingleType("Nat"), new SingleType("Nat")), new SingleType("Nat"))), new SingleType("Bool")));
	}
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{T1_type, T1_type2},
        	{T2_type, T2_type2},
        	{T3_type, T3_type2}
       	 });
    }
    
	private IType typeA;
	private IType typeB;
	
	public TestClass_ITypeEquals(IType typeA, IType typeB) {
		this.typeA = typeA;
		this.typeB = typeB;
	}
	
	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testCorrect() {
		
		if (this.typeA.equals(this.typeB)) {
			assertTrue(true);
		} else {
			fail("not equal");
		}
		
	}
}
