package hu.elte.inf.toduabi;

import java.util.ArrayList;

public class TypeCheck {
	
	public static TypedExpression parseAndCheck(String input) throws LexParserException, SyntaxParserException, TypeCheckException {
		
		// Lexical parser
		ArrayList<Parsers.LexItem> tokens = Parsers.lexParser.parse(input);
		
		// Syntax parser
		TypedExpression typedExpression = Parsers.typedExpressionParser.parse(tokens);
		
		// Deduct the type
		IType type = typedExpression.getExpression().deductType(typedExpression.getTypeContext());
		
		if (!type.equals(typedExpression.getType())) {
			throw new TypeCheckException(typedExpression.getExpression().toString());
		}
		
		return typedExpression;
	}

}
