//
// Generated by JTB 1.2.1
//

package jde.debugger.interpret.visitor;
import jde.debugger.interpret.syntaxtree.*;
import java.util.*;

/**
 * All void visitors must implement this interface.
 */
public interface Visitor {
   //
   // void Auto class visitors
   //
   public void visit(NodeList n);
   public void visit(NodeListOptional n);
   public void visit(NodeOptional n);
   public void visit(NodeSequence n);
   public void visit(NodeToken n);

   //
   // User-generated visitor methods below
   //

   /**
    * <PRE>
    * nodeChoice -> ( PrimitiveType() | Name() )
    * nodeListOptional -> ( "[" "]" )*
    * </PRE>
    */
   public void visit(Type n);

   /**
    * <PRE>
    * nodeChoice -> "boolean"
    *       | "char"
    *       | "byte"
    *       | "short"
    *       | "int"
    *       | "long"
    *       | "float"
    *       | "double"
    * </PRE>
    */
   public void visit(PrimitiveType n);

   /**
    * <PRE>
    * nodeChoice -> "void"
    *       | Type()
    * </PRE>
    */
   public void visit(ResultType n);

   /**
    * <PRE>
    * nodeToken -> &lt;IDENTIFIER&gt;
    * nodeListOptional -> ( "." &lt;IDENTIFIER&gt; )*
    * </PRE>
    */
   public void visit(Name n);

   /**
    * <PRE>
    * name -> Name()
    * nodeListOptional -> ( "," Name() )*
    * </PRE>
    */
   public void visit(NameList n);

   /**
    * <PRE>
    * conditionalExpression -> ConditionalExpression()
    * nodeOptional -> [ AssignmentOperator() Expression() ]
    * </PRE>
    */
   public void visit(Expression n);

   /**
    * <PRE>
    * nodeChoice -> "="
    *       | "*="
    *       | "/="
    *       | "%="
    *       | "+="
    *       | "-="
    *       | "&lt;&lt;="
    *       | "&gt;&gt;="
    *       | "&gt;&gt;&gt;="
    *       | "&="
    *       | "^="
    *       | "|="
    * </PRE>
    */
   public void visit(AssignmentOperator n);

   /**
    * <PRE>
    * conditionalOrExpression -> ConditionalOrExpression()
    * nodeOptional -> [ "?" Expression() ":" ConditionalExpression() ]
    * </PRE>
    */
   public void visit(ConditionalExpression n);

   /**
    * <PRE>
    * conditionalAndExpression -> ConditionalAndExpression()
    * nodeListOptional -> ( "||" ConditionalAndExpression() )*
    * </PRE>
    */
   public void visit(ConditionalOrExpression n);

   /**
    * <PRE>
    * inclusiveOrExpression -> InclusiveOrExpression()
    * nodeListOptional -> ( "&&" InclusiveOrExpression() )*
    * </PRE>
    */
   public void visit(ConditionalAndExpression n);

   /**
    * <PRE>
    * exclusiveOrExpression -> ExclusiveOrExpression()
    * nodeListOptional -> ( "|" ExclusiveOrExpression() )*
    * </PRE>
    */
   public void visit(InclusiveOrExpression n);

   /**
    * <PRE>
    * andExpression -> AndExpression()
    * nodeListOptional -> ( "^" AndExpression() )*
    * </PRE>
    */
   public void visit(ExclusiveOrExpression n);

   /**
    * <PRE>
    * equalityExpression -> EqualityExpression()
    * nodeListOptional -> ( "&" EqualityExpression() )*
    * </PRE>
    */
   public void visit(AndExpression n);

   /**
    * <PRE>
    * instanceOfExpression -> InstanceOfExpression()
    * nodeListOptional -> ( ( "==" | "!=" ) InstanceOfExpression() )*
    * </PRE>
    */
   public void visit(EqualityExpression n);

   /**
    * <PRE>
    * relationalExpression -> RelationalExpression()
    * nodeOptional -> [ "instanceof" Type() ]
    * </PRE>
    */
   public void visit(InstanceOfExpression n);

   /**
    * <PRE>
    * shiftExpression -> ShiftExpression()
    * nodeListOptional -> ( ( "&lt;" | "&gt;" | "&lt;=" | "&gt;=" ) ShiftExpression() )*
    * </PRE>
    */
   public void visit(RelationalExpression n);

   /**
    * <PRE>
    * additiveExpression -> AdditiveExpression()
    * nodeListOptional -> ( ( "&lt;&lt;" | "&gt;&gt;" | "&gt;&gt;&gt;" ) AdditiveExpression() )*
    * </PRE>
    */
   public void visit(ShiftExpression n);

   /**
    * <PRE>
    * multiplicativeExpression -> MultiplicativeExpression()
    * nodeListOptional -> ( ( "+" | "-" ) MultiplicativeExpression() )*
    * </PRE>
    */
   public void visit(AdditiveExpression n);

   /**
    * <PRE>
    * unaryExpression -> UnaryExpression()
    * nodeListOptional -> ( ( "*" | "/" | "%" ) UnaryExpression() )*
    * </PRE>
    */
   public void visit(MultiplicativeExpression n);

   /**
    * <PRE>
    * nodeChoice -> ( "+" | "-" ) UnaryExpression()
    *       | PreIncrementExpression()
    *       | PreDecrementExpression()
    *       | UnaryExpressionNotPlusMinus()
    * </PRE>
    */
   public void visit(UnaryExpression n);

   /**
    * <PRE>
    * nodeToken -> "++"
    * primaryExpression -> PrimaryExpression()
    * </PRE>
    */
   public void visit(PreIncrementExpression n);

   /**
    * <PRE>
    * nodeToken -> "--"
    * primaryExpression -> PrimaryExpression()
    * </PRE>
    */
   public void visit(PreDecrementExpression n);

   /**
    * <PRE>
    * nodeChoice -> ( "~" | "!" ) UnaryExpression()
    *       | CastExpression()
    *       | PostfixExpression()
    * </PRE>
    */
   public void visit(UnaryExpressionNotPlusMinus n);

   /**
    * <PRE>
    * nodeChoice -> "(" PrimitiveType()
    *       | "(" Name() "[" "]"
    *       | "(" Name() ")" ( "~" | "!" | "(" | &lt;IDENTIFIER&gt; | "this" | "super" | "new" | Literal() )
    * </PRE>
    */
   public void visit(CastLookahead n);

   /**
    * <PRE>
    * primaryExpression -> PrimaryExpression()
    * nodeOptional -> [ "++" | "--" ]
    * </PRE>
    */
   public void visit(PostfixExpression n);

   /**
    * <PRE>
    * nodeChoice -> "(" Type() ")" UnaryExpression()
    *       | "(" Type() ")" UnaryExpressionNotPlusMinus()
    * </PRE>
    */
   public void visit(CastExpression n);

   /**
    * <PRE>
    * primaryPrefix -> PrimaryPrefix()
    * nodeListOptional -> ( PrimarySuffix() )*
    * </PRE>
    */
   public void visit(PrimaryExpression n);

   /**
    * <PRE>
    * nodeChoice -> Literal()
    *       | "this"
    *       | "super" "." &lt;IDENTIFIER&gt;
    *       | "(" Expression() ")"
    *       | AllocationExpression()
    *       | ResultType() "." "class"
    *       | Name()
    * </PRE>
    */
   public void visit(PrimaryPrefix n);

   /**
    * <PRE>
    * nodeChoice -> "." "this"
    *       | "." AllocationExpression()
    *       | "[" Expression() "]"
    *       | "." &lt;IDENTIFIER&gt;
    *       | Arguments()
    * </PRE>
    */
   public void visit(PrimarySuffix n);

   /**
    * <PRE>
    * nodeChoice -> &lt;INTEGER_LITERAL&gt;
    *       | &lt;FLOATING_POINT_LITERAL&gt;
    *       | &lt;CHARACTER_LITERAL&gt;
    *       | &lt;STRING_LITERAL&gt;
    *       | BooleanLiteral()
    *       | NullLiteral()
    * </PRE>
    */
   public void visit(Literal n);

   /**
    * <PRE>
    * nodeChoice -> "true"
    *       | "false"
    * </PRE>
    */
   public void visit(BooleanLiteral n);

   /**
    * <PRE>
    * nodeToken -> "null"
    * </PRE>
    */
   public void visit(NullLiteral n);

   /**
    * <PRE>
    * nodeToken -> "("
    * nodeOptional -> [ ArgumentList() ]
    * nodeToken1 -> ")"
    * </PRE>
    */
   public void visit(Arguments n);

   /**
    * <PRE>
    * expression -> Expression()
    * nodeListOptional -> ( "," Expression() )*
    * </PRE>
    */
   public void visit(ArgumentList n);

   /**
    * <PRE>
    * nodeChoice -> "new" PrimitiveType() ArrayDimsAndInits()
    *       | "new" Name() ( ArrayDimsAndInits() | Arguments() )
    * </PRE>
    */
   public void visit(AllocationExpression n);

   /**
    * <PRE>
    * nodeList -> ( "[" Expression() "]" )+
    * nodeListOptional -> ( "[" "]" )*
    * </PRE>
    */
   public void visit(ArrayDimsAndInits n);

}

