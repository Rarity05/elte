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
public class TestClass_TypeCheck {

	private String input;
	private String output;
	
	@Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {     
        	{"|-", "|-"},
        	{"x |-", "x |-"},
        	{"|- x", "|- x"},
        	{"|- x Bool", "|- x Bool"},
        	{"|- \\x.x", "|- \\x.x"},
        	{"|- x : Bool", "x"},
        	{"x:Bool -> Nat, y:Nat |- x y : Nat", "(x y)"},
        	{"|- \\x:Nat.x x : Nat -> Nat", "(x x)"},
        	{"|- \\x:Bool.(\\y:Nat.y) x : Bool -> Bool", "((\\y:Nat.y) x)"},
        	{"x:Bool -> Bool -> Bool |- x : Bool -> Bool -> Bool", "x:(Bool -> (Bool -> Bool))"},
        	{"x:Bool |- \\y:Nat.x : Nat -> Bool", "(\\y:Nat.x):(Nat -> Bool)"},
        	{"|- \\x:Bool -> Nat.\\y:Bool.x y : (Bool -> Nat) -> Bool -> Nat", "(\\x:(Bool -> Nat).(\\y:Bool.(x y))):((Bool -> Nat) -> (Bool -> Nat))"},
        	{"f:Nat -> Bool |- \\x:Nat.f x : Nat -> Bool", "(\\x:Nat.(f x)):(Nat -> Bool)"},
        	{"|- \\x:Nat.\\y:Bool.x : Nat -> Bool -> Nat", "(\\x:Nat.(\\y:Bool.x)):(Nat -> (Bool -> Nat))"},
        	{"|- \\x:Nat.\\y:Bool.x : Nat -> (Bool -> Nat)", "(\\x:Nat.(\\y:Bool.x)):(Nat -> (Bool -> Nat))"},
        	{"|- \\x:Nat.\\y:Bool.x : (Nat -> Bool) -> Nat", "(\\x:Nat.(\\y:Bool.x))"},
        	{"y:Bool |- \\x:Bool -> Nat.x y : (Bool -> Nat) -> Nat", "(\\x:(Bool -> Nat).(x y)):((Bool -> Nat) -> Nat)"},
        	{"|- (\\f:(Nat -> Nat) -> Nat -> Nat.\\x:Nat -> Nat.f (f x)) (\\f:Nat -> Nat.\\x:Nat.f (f x)) : (Nat -> Nat) -> Nat -> Nat", "((\\f:((Nat -> Nat) -> (Nat -> Nat)).(\\x:(Nat -> Nat).(f (f x)))) (\\f:(Nat -> Nat).(\\x:Nat.(f (f x))))):((Nat -> Nat) -> (Nat -> Nat))"}
       	 });
    }

	
	public TestClass_TypeCheck(String input, String output) {
		this.input = input;
		this.output = output;
	}
	
	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testCorrect() {
		
		try {
			TypedExpression expression = TypeCheck.parseAndCheck(this.input);
			assertTrue(expression.toString().equals(this.output));
		} catch (LexParserException | SyntaxParserException e) {
			assertTrue(this.input.equals(this.output));
		} catch (TypeCheckException e) {
			assertTrue(e.getMessage().equals(this.output));
		}		
	}
}
