package hu.elte.inf.toduabi;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import hu.elte.inf.toduabi.LexParser.Item;
import hu.elte.inf.toduabi.SyntaxParser.ReturnType;
import hu.elte.inf.toduabi.SyntaxParser.ReturnWrapper;

public class Parsers {

	/**
	 * The typeParser's syntax callback
	 */
	private static ISyntaxCallback<Parsers.LexItem, Parsers.Type, IType> syntaxCallback = new ISyntaxCallback<Parsers.LexItem, Parsers.Type, IType>() {

		@SuppressWarnings("unchecked")
		@Override
		public <T extends ILexicalItem<Type>> ReturnWrapper foundRule(String rule, ArrayList<T> prefixList,
				Stack<T> stack, ArrayList<T> remainingTokens, Type nextType, Stack<IType> expressions) {
			
			/**
			 * Found a single type in the stack
			 *  - Create a new SingleType in the expressions
			 *  - Push a new RType back to the stack
			 *  : If nextType is null or CLOSE then reduce again
			 *  : Otherwise return +1
			 */
			if (rule.equals("SingleType")) {
				if (prefixList.size() != 1) {
					throw new RuntimeException("SyntaxParser: SingleType internal error");
				}
				
				expressions.push(new SingleType(prefixList.get(0).getToken()));
				stack.push((T) new Parsers.LexItem("R", Parsers.Type.RTYPE));
			}
			/**
			 * Found an arrow type in the stack
			 * - Create new ArrowType (from 2 ITypes from the expressions) in the expressions
			 * - Push a new RType back to the stack
			 * : If nextType is null or CLOSE then reduce again
			 * : Otherwise return +1
			 */
			else if (rule.equals("ArrowType")) {
				if (prefixList.size() != 3 || expressions.size() < 2) {
					throw new RuntimeException("SyntaxParser: ArrowType internal error");
				}
				
				IType right = expressions.pop();
				IType left = expressions.pop();
				
				expressions.push(new ArrowType(left, right));
				stack.push((T) new Parsers.LexItem("R", Parsers.Type.RTYPE));
			}
			/**
			 * Found parenthesis rule
			 * - We don't have to do anything, the parenthesis are removed
			 * : Push back the RTYPE
			 */
			else if (rule.equals("ParenthesisType")) {
				if (prefixList.size() != 3 || expressions.size() < 1) {
					throw new RuntimeException("SyntaxParser: ParenthesisType internal error");
				}
				
				stack.push((T) new Parsers.LexItem("R", Parsers.Type.RTYPE));
			}
			/**
			 * Found a reduced type in the stack
			 * - If we are at the end of the input and the stack is empty,
			 *   then it was the last -> return 1
			 * - Otherwise let the parser continue
			 */
			else if (rule.equals("ReducedType")) {
				if (stack.isEmpty() && nextType == null) {
					return new ReturnWrapper(ReturnType.RETURN, 1);
				} else {
					return new ReturnWrapper(ReturnType.CONTINUE, 0);
				}
			} else {
				throw new RuntimeException("SyntaxParser: undefined TypeParser rule: " + rule);
			}
			
			if (nextType == null || nextType == Parsers.Type.CLOSE) {
				return new ReturnWrapper(ReturnType.CHECK, 0);
			} else {
				return new ReturnWrapper(ReturnType.RETURN, 1);
			}
		}
		
	};
	/**
	 * Rules for the typeParser
	 */
	private final static HashMap<ArrayList<Type>, String> typeParserRules = new HashMap<ArrayList<Type>, String>();
	/**
	 * The typeParser
	 */
	public final static SyntaxParser<LexItem, Type, IType> typeParser = new SyntaxParser<LexItem, Type, IType>(typeParserRules, syntaxCallback);
	
	/**
	 * The lexParser
	 */
	public final static LexParser<LexItem, Type> lexParser = new LexParser<LexItem, Type>();
	
	/**
	 * The typeContextParser
	 *
	 */
	public static class typeContextParser {
		public static TypeContext parse(ArrayList<LexItem> tokens) throws SyntaxParserException {
			
			/**
			 * Split the tokens at the 'COMMA' lexical item
			 */
			ArrayList<ArrayList<LexItem>> splitted = new ArrayList<ArrayList<LexItem>>();
			Stream<LexItem> tokenStream = tokens.stream();
			tokenStream.forEach(token -> {
				if (token.getType() == Type.COMMA) {
					splitted.add(new ArrayList<LexItem>());
				} else {
					if (splitted.size() == 0) {
						splitted.add(new ArrayList<LexItem>());
					}
					splitted.get(splitted.size()-1).add(token);
				}
			});
			
			/**
			 * Create LambdaVariables from the tokens
			 */
			Stream<ArrayList<LexItem>> splittedStream = splitted.stream();
			Stream<LambdaVariable> variableStream = splittedStream.map(splittedTokens -> {
				if (splittedTokens.size() < 3) {
					return null;
				}
				
				LexItem variable = splittedTokens.get(0);
				LexItem colon = splittedTokens.get(1);
				if (!variable.getType().equals(Type.VARIABLE) || !colon.getType().equals(Type.COLON)) {
					return null;
				}
				
				ArrayList<LexItem> typeTokens = new ArrayList<LexItem>(splittedTokens.subList(2, splittedTokens.size()));
				IType type;
				try {
					type = typeParser.parse(typeTokens);
				} catch (Exception e) {
					return null;
				}
				
				return new LambdaVariable(variable.getToken().toCharArray()[0], type);
			});
			
			
			HashSet<LambdaVariable> variableSet = new HashSet<LambdaVariable>(variableStream.collect(Collectors.toSet()));
			if (variableSet.contains(null)) {
				throw new SyntaxParserException("Could not parse type context");
			};
			
			/**
			 * Return with the type context
			 */
			return new TypeContext(variableSet);
		}
	}
	
	/**
	 * The prefix R means that the item has been reduced
	 * @author I321357
	 *
	 */
	public enum Type {
		LAMBDA, VARIABLE, RVARIABLE, APPLICATION, DOT, OPEN, CLOSE, ARROW, COLON, COMMA, CONTEXT, TYPE, RTYPE
	}
	public static class LexItem implements ILexicalItem<Type> {
		private String token;
		private Type type;
		
		public LexItem(String token, Type type) {
			this.token = token;
			this.type = type;
		}
		
		@Override
		public String getToken() {
			return token;
		}

		@Override
		public Type getType() {
			return type;
		}
		
		@Override
        public int hashCode() {
            int sum = 0;
            for (int i=0; i<this.token.length(); i++) {
                sum += this.token.charAt(i);
            }
            return sum;
        }

        @Override
        public boolean equals(Object _other) {
            if (!(_other instanceof Item)) {
                return false;
            }
            
            LexItem other = (LexItem) _other; 
            return this.token.equals(other.getToken()) && this.type.equals(other.getType());
        }
		
	}
	
	static {
		/**
		 * Add lexical rules
		 */
		lexParser.addItem(new LexItem("\\", Type.LAMBDA));
		for (int i = 'a'; i <= 'z'; i++) {
			lexParser.addItem(new LexItem(Character.toString(Character.toChars(i)[0]), Type.VARIABLE));
		}
		lexParser.addItem(new LexItem(" ", Type.APPLICATION));
		lexParser.addItem(new LexItem(".", Type.DOT));
		lexParser.addItem(new LexItem(",", Type.COMMA));
		lexParser.addItem(new LexItem("(", Type.OPEN));
		lexParser.addItem(new LexItem(")", Type.CLOSE));
		
		lexParser.addItem(new LexItem("->", Type.ARROW));
		lexParser.addItem(new LexItem(":", Type.COLON));
		lexParser.addItem(new LexItem("|-", Type.CONTEXT));
		
		lexParser.addItem(new LexItem("Bool", Type.TYPE));
		lexParser.addItem(new LexItem("Nat", Type.TYPE));
		
		/**
		 * Add syntax rules
		 */
		
		/**
		 *  TYPE rules: Type = Type | Type -> Type | ( Type ) 
		 */
		ArrayList<Type> T1 = new ArrayList<Type>();
		T1.add(Type.TYPE);
		typeParserRules.put(T1, "SingleType");
		
		ArrayList<Type> T2 = new ArrayList<Type>();
		T2.add(Type.RTYPE);
		T2.add(Type.ARROW);
		T2.add(Type.RTYPE);
		typeParserRules.put(T2, "ArrowType");
		
		ArrayList<Type> T3 = new ArrayList<Type>();
		T3.add(Type.CLOSE);
		T3.add(Type.RTYPE);
		T3.add(Type.OPEN);
		typeParserRules.put(T3, "ParenthesisType");
		
		ArrayList<Type> T4 = new ArrayList<Type>();
		T4.add(Type.RTYPE);
		typeParserRules.put(T4, "ReducedType");
	}
}
