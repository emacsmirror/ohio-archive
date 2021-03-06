//
// Generated by JTB 1.2.1
//

package jde.debugger.interpret.visitor;
import jde.debugger.interpret.syntaxtree.*;
import java.util.*;

/**
 * Provides default methods which visit each node in the tree in depth-first
 * order.  Your visitors may extend this class.
 */
public class ObjectDepthFirst implements ObjectVisitor {
   //
   // Auto class visitors--probably don't need to be overridden.
   //
   public Object visit(NodeList n, Object argu) {
      Object _ret=null;
      int _count=0;
      for ( Enumeration e = n.elements(); e.hasMoreElements(); ) {
         ((Node)e.nextElement()).accept(this,argu);
         _count++;
      }
      return _ret;
   }

   public Object visit(NodeListOptional n, Object argu) {
      if ( n.present() ) {
         Object _ret=null;
         int _count=0;
         for ( Enumeration e = n.elements(); e.hasMoreElements(); ) {
            ((Node)e.nextElement()).accept(this,argu);
            _count++;
         }
         return _ret;
      }
      else
         return null;
   }

   public Object visit(NodeOptional n, Object argu) {
      if ( n.present() )
         return n.node.accept(this,argu);
      else
         return null;
   }

   public Object visit(NodeSequence n, Object argu) {
      Object _ret=null;
      int _count=0;
      for ( Enumeration e = n.elements(); e.hasMoreElements(); ) {
         ((Node)e.nextElement()).accept(this,argu);
         _count++;
      }
      return _ret;
   }

   public Object visit(NodeToken n, Object argu) { return null; }

   //
   // User-generated visitor methods below
   //

   /**
    * <PRE>
    * nodeChoice -> ( PrimitiveType() | Name() )
    * nodeListOptional -> ( "[" "]" )*
    * </PRE>
    */
   public Object visit(Type n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

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
   public Object visit(PrimitiveType n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeChoice -> "void"
    *       | Type()
    * </PRE>
    */
   public Object visit(ResultType n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeToken -> &lt;IDENTIFIER&gt;
    * nodeListOptional -> ( "." &lt;IDENTIFIER&gt; )*
    * </PRE>
    */
   public Object visit(Name n, Object argu) {
      Object _ret=null;
      n.nodeToken.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * name -> Name()
    * nodeListOptional -> ( "," Name() )*
    * </PRE>
    */
   public Object visit(NameList n, Object argu) {
      Object _ret=null;
      n.name.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * conditionalExpression -> ConditionalExpression()
    * nodeOptional -> [ AssignmentOperator() Expression() ]
    * </PRE>
    */
   public Object visit(Expression n, Object argu) {
      Object _ret=null;
      n.conditionalExpression.accept(this, argu);
      n.nodeOptional.accept(this, argu);
      return _ret;
   }

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
   public Object visit(AssignmentOperator n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * conditionalOrExpression -> ConditionalOrExpression()
    * nodeOptional -> [ "?" Expression() ":" ConditionalExpression() ]
    * </PRE>
    */
   public Object visit(ConditionalExpression n, Object argu) {
      Object _ret=null;
      n.conditionalOrExpression.accept(this, argu);
      n.nodeOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * conditionalAndExpression -> ConditionalAndExpression()
    * nodeListOptional -> ( "||" ConditionalAndExpression() )*
    * </PRE>
    */
   public Object visit(ConditionalOrExpression n, Object argu) {
      Object _ret=null;
      n.conditionalAndExpression.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * inclusiveOrExpression -> InclusiveOrExpression()
    * nodeListOptional -> ( "&&" InclusiveOrExpression() )*
    * </PRE>
    */
   public Object visit(ConditionalAndExpression n, Object argu) {
      Object _ret=null;
      n.inclusiveOrExpression.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * exclusiveOrExpression -> ExclusiveOrExpression()
    * nodeListOptional -> ( "|" ExclusiveOrExpression() )*
    * </PRE>
    */
   public Object visit(InclusiveOrExpression n, Object argu) {
      Object _ret=null;
      n.exclusiveOrExpression.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * andExpression -> AndExpression()
    * nodeListOptional -> ( "^" AndExpression() )*
    * </PRE>
    */
   public Object visit(ExclusiveOrExpression n, Object argu) {
      Object _ret=null;
      n.andExpression.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * equalityExpression -> EqualityExpression()
    * nodeListOptional -> ( "&" EqualityExpression() )*
    * </PRE>
    */
   public Object visit(AndExpression n, Object argu) {
      Object _ret=null;
      n.equalityExpression.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * instanceOfExpression -> InstanceOfExpression()
    * nodeListOptional -> ( ( "==" | "!=" ) InstanceOfExpression() )*
    * </PRE>
    */
   public Object visit(EqualityExpression n, Object argu) {
      Object _ret=null;
      n.instanceOfExpression.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * relationalExpression -> RelationalExpression()
    * nodeOptional -> [ "instanceof" Type() ]
    * </PRE>
    */
   public Object visit(InstanceOfExpression n, Object argu) {
      Object _ret=null;
      n.relationalExpression.accept(this, argu);
      n.nodeOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * shiftExpression -> ShiftExpression()
    * nodeListOptional -> ( ( "&lt;" | "&gt;" | "&lt;=" | "&gt;=" ) ShiftExpression() )*
    * </PRE>
    */
   public Object visit(RelationalExpression n, Object argu) {
      Object _ret=null;
      n.shiftExpression.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * additiveExpression -> AdditiveExpression()
    * nodeListOptional -> ( ( "&lt;&lt;" | "&gt;&gt;" | "&gt;&gt;&gt;" ) AdditiveExpression() )*
    * </PRE>
    */
   public Object visit(ShiftExpression n, Object argu) {
      Object _ret=null;
      n.additiveExpression.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * multiplicativeExpression -> MultiplicativeExpression()
    * nodeListOptional -> ( ( "+" | "-" ) MultiplicativeExpression() )*
    * </PRE>
    */
   public Object visit(AdditiveExpression n, Object argu) {
      Object _ret=null;
      n.multiplicativeExpression.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * unaryExpression -> UnaryExpression()
    * nodeListOptional -> ( ( "*" | "/" | "%" ) UnaryExpression() )*
    * </PRE>
    */
   public Object visit(MultiplicativeExpression n, Object argu) {
      Object _ret=null;
      n.unaryExpression.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeChoice -> ( "+" | "-" ) UnaryExpression()
    *       | PreIncrementExpression()
    *       | PreDecrementExpression()
    *       | UnaryExpressionNotPlusMinus()
    * </PRE>
    */
   public Object visit(UnaryExpression n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeToken -> "++"
    * primaryExpression -> PrimaryExpression()
    * </PRE>
    */
   public Object visit(PreIncrementExpression n, Object argu) {
      Object _ret=null;
      n.nodeToken.accept(this, argu);
      n.primaryExpression.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeToken -> "--"
    * primaryExpression -> PrimaryExpression()
    * </PRE>
    */
   public Object visit(PreDecrementExpression n, Object argu) {
      Object _ret=null;
      n.nodeToken.accept(this, argu);
      n.primaryExpression.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeChoice -> ( "~" | "!" ) UnaryExpression()
    *       | CastExpression()
    *       | PostfixExpression()
    * </PRE>
    */
   public Object visit(UnaryExpressionNotPlusMinus n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeChoice -> "(" PrimitiveType()
    *       | "(" Name() "[" "]"
    *       | "(" Name() ")" ( "~" | "!" | "(" | &lt;IDENTIFIER&gt; | "this" | "super" | "new" | Literal() )
    * </PRE>
    */
   public Object visit(CastLookahead n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * primaryExpression -> PrimaryExpression()
    * nodeOptional -> [ "++" | "--" ]
    * </PRE>
    */
   public Object visit(PostfixExpression n, Object argu) {
      Object _ret=null;
      n.primaryExpression.accept(this, argu);
      n.nodeOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeChoice -> "(" Type() ")" UnaryExpression()
    *       | "(" Type() ")" UnaryExpressionNotPlusMinus()
    * </PRE>
    */
   public Object visit(CastExpression n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * primaryPrefix -> PrimaryPrefix()
    * nodeListOptional -> ( PrimarySuffix() )*
    * </PRE>
    */
   public Object visit(PrimaryExpression n, Object argu) {
      Object _ret=null;
      n.primaryPrefix.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

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
   public Object visit(PrimaryPrefix n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeChoice -> "." "this"
    *       | "." AllocationExpression()
    *       | "[" Expression() "]"
    *       | "." &lt;IDENTIFIER&gt;
    *       | Arguments()
    * </PRE>
    */
   public Object visit(PrimarySuffix n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      return _ret;
   }

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
   public Object visit(Literal n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeChoice -> "true"
    *       | "false"
    * </PRE>
    */
   public Object visit(BooleanLiteral n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeToken -> "null"
    * </PRE>
    */
   public Object visit(NullLiteral n, Object argu) {
      Object _ret=null;
      n.nodeToken.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeToken -> "("
    * nodeOptional -> [ ArgumentList() ]
    * nodeToken1 -> ")"
    * </PRE>
    */
   public Object visit(Arguments n, Object argu) {
      Object _ret=null;
      n.nodeToken.accept(this, argu);
      n.nodeOptional.accept(this, argu);
      n.nodeToken1.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * expression -> Expression()
    * nodeListOptional -> ( "," Expression() )*
    * </PRE>
    */
   public Object visit(ArgumentList n, Object argu) {
      Object _ret=null;
      n.expression.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeChoice -> "new" PrimitiveType() ArrayDimsAndInits()
    *       | "new" Name() ( ArrayDimsAndInits() | Arguments() )
    * </PRE>
    */
   public Object visit(AllocationExpression n, Object argu) {
      Object _ret=null;
      n.nodeChoice.accept(this, argu);
      return _ret;
   }

   /**
    * <PRE>
    * nodeList -> ( "[" Expression() "]" )+
    * nodeListOptional -> ( "[" "]" )*
    * </PRE>
    */
   public Object visit(ArrayDimsAndInits n, Object argu) {
      Object _ret=null;
      n.nodeList.accept(this, argu);
      n.nodeListOptional.accept(this, argu);
      return _ret;
   }

}
