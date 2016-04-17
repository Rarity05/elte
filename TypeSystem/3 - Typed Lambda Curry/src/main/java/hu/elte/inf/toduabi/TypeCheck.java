package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.List;

public class TypeCheck {
	
	public static IType parseAndCheck(String input) throws LexParserException, SyntaxParserException, TypeCheckException {
		
		// Lexical parser
		ArrayList<Parsers.LexItem> tokens = Parsers.lexParser.parse(input);
		
		// Syntax parser
		TypedExpression typedExpression = Parsers.typedExpressionParser.parse(tokens);
		
		List<Curry.Restriction> restrictions = Curry.T(typedExpression.getTypeContext(), typedExpression.getExpression(), new SingleType(Curry.getTypeVariable()));
		
		return Curry.S(restrictions);
	}

}
