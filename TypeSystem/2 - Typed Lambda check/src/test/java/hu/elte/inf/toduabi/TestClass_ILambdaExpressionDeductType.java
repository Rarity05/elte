package hu.elte.inf.toduabi;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class TestClass_ILambdaExpressionDeductType {

	private static ILambdaExpression T1_expression;
	
	private static IType T1_type;
	
	private static TypeContext T1_typeContext;
	
	static {

		HashSet<LambdaVariable> T1_variableSet = new HashSet<LambdaVariable>();
		T1_typeContext = new TypeContext(T1_variableSet);
		T1_type = new ArrowType(new ArrowType(new SingleType("Nat"), new SingleType("Nat")), new ArrowType(new SingleType("Nat"), new SingleType("Nat")));
		T1_expression = new LambdaApplication(new LambdaAbstraction(new LambdaVariable('f', T1_type),new LambdaAbstraction(new LambdaVariable('x', new ArrowType(new SingleType("Nat"), new SingleType("Nat"))),new LambdaApplication(new LambdaVariable('f', null),new LambdaApplication(new LambdaVariable('f', null),new LambdaVariable('x', null))))), new LambdaAbstraction(new LambdaVariable('f', new ArrowType(new SingleType("Nat"), new SingleType("Nat"))),new LambdaAbstraction(new LambdaVariable('x', new SingleType("Nat")),new LambdaApplication(new LambdaVariable('f', null),new LambdaApplication(new LambdaVariable('f', null),new LambdaVariable('x', null))))));
	}
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{T1_expression, T1_type, T1_typeContext}
       	 });
    }
    
	private IType type;
	private TypeContext typeContext;
	private ILambdaExpression expression;
	
	public TestClass_ILambdaExpressionDeductType(ILambdaExpression expression, IType type, TypeContext typeContext) {
		this.type = type;
		this.typeContext = typeContext;
		this.expression = expression;
	}
	
	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testCorrect() {
		
		try {
			IType rType = this.expression.deductType(typeContext);
			assertTrue(rType.equals(this.type));
		} catch (Exception e) {
			fail(e.getLocalizedMessage());
		}
		
	}
}
