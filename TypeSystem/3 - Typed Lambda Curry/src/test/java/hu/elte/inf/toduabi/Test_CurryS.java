package hu.elte.inf.toduabi;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class Test_CurryS {
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{new ArrayList<Curry.Restriction>(){{
        			add(new Curry.Restriction(new SingleType("A"), new ArrowType(new SingleType("B"), new SingleType("C"))));
        			add(new Curry.Restriction(new SingleType("C"), new SingleType("B")));
        		}}, new ArrowType(new SingleType("B"), new SingleType("B"))
        	}
       	 });
    }
    
	private IType rType;
	private List<Curry.Restriction> res;
	
	public Test_CurryS(List<Curry.Restriction> res, IType rType) {
		this.res = res;
		this.rType = rType;
	}
	
	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testCorrect() {
		Curry.reset();
		IType type = Curry.S(this.res, new ArrayList<Curry.Substitution>());
		
		if (type.equals(this.rType)) {
			assertTrue(true);
		} else {
			fail("not equal");
		}
		
	}
}
