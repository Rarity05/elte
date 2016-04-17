package hu.elte.inf.toduabi;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class Test_CurryT {
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{new LambdaAbstraction(new LambdaVariable('x', null), new LambdaVariable('x', null)),
        		new ArrayList<Curry.Restriction>(){{
        			add(new Curry.Restriction(new SingleType("A"), new ArrowType(new SingleType("B"), new SingleType("C"))));
        			add(new Curry.Restriction(new SingleType("C"), new SingleType("B")));
        		}} },
        	{new LambdaAbstraction(new LambdaVariable('x', null), new LambdaAbstraction(new LambdaVariable('y', null), new LambdaVariable('x', null))),
        		new ArrayList<Curry.Restriction>() {{
        			add(new Curry.Restriction(new SingleType("A"), new ArrowType(new SingleType("B"), new SingleType("C"))));
        			add(new Curry.Restriction(new SingleType("C"), new ArrowType(new SingleType("D"), new SingleType("E"))));
        			add(new Curry.Restriction(new SingleType("E"), new SingleType("B")));
        		}}
        			}
       	 });
    }
    
	private ILambdaExpression expression;
	private List<Curry.Restriction> rRestrictions;
	
	public Test_CurryT(ILambdaExpression expression, List<Curry.Restriction> rRestrictions) {
		this.expression = expression;
		this.rRestrictions = rRestrictions;
	}
	
	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testCorrect() {
		
		HashSet<LambdaVariable> contextSet = new HashSet<LambdaVariable>();
		TypeContext context = new TypeContext(contextSet);
		
		Curry.reset();
		List<Curry.Restriction> restrictions = Curry.T(context, this.expression, new SingleType(Curry.getTypeVariable()));
		
		if (Arrays.equals(restrictions.toArray(), this.rRestrictions.toArray())) {
			assertTrue(true);
		} else {
			fail("not equal");
		}
		
	}
}
