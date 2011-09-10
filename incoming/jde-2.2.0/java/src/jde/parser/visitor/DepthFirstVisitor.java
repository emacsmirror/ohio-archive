//
// Generated by JTB 1.2.1
//

package jde.parser.visitor;
import jde.parser.syntaxtree.*;
import java.util.*;

/**
 * Provides default methods which visit each node in the tree in depth-first
 * order.  Your visitors may extend this class.
 */
public class DepthFirstVisitor implements Visitor {
   //
   // Auto class visitors--probably don't need to be overridden.
   //
   public void visit(NodeList n) {
      for ( Enumeration e = n.elements(); e.hasMoreElements(); )
         ((Node)e.nextElement()).accept(this);
   }

   public void visit(NodeListOptional n) {
      if ( n.present() )
         for ( Enumeration e = n.elements(); e.hasMoreElements(); )
            ((Node)e.nextElement()).accept(this);
   }

   public void visit(NodeOptional n) {
      if ( n.present() )
         n.node.accept(this);
   }

   public void visit(NodeSequence n) {
      for ( Enumeration e = n.elements(); e.hasMoreElements(); )
         ((Node)e.nextElement()).accept(this);
   }

   public void visit(NodeToken n) { }

   //
   // User-generated visitor methods below
   //

   /**
    * <PRE>
    * nodeOptional -> [ PackageDeclaration() ]
    * nodeListOptional -> ( ImportDeclaration() )*
    * nodeListOptional1 -> ( TypeDeclaration() )*
    * nodeToken -> &lt;EOF&gt;
    * </PRE>
    */
   public void visit(CompilationUnit n) {
      n.nodeOptional.accept(this);
      n.nodeListOptional.accept(this);
      n.nodeListOptional1.accept(this);
      n.nodeToken.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "package"
    * name -> Name()
    * nodeToken1 -> ";"
    * </PRE>
    */
   public void visit(PackageDeclaration n) {
      n.nodeToken.accept(this);
      n.name.accept(this);
      n.nodeToken1.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "import"
    * name -> Name()
    * nodeOptional -> [ "." "*" ]
    * nodeToken1 -> ";"
    * </PRE>
    */
   public void visit(ImportDeclaration n) {
      n.nodeToken.accept(this);
      n.name.accept(this);
      n.nodeOptional.accept(this);
      n.nodeToken1.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> ClassDeclaration()
    *       | InterfaceDeclaration()
    *       | ";"
    * </PRE>
    */
   public void visit(TypeDeclaration n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeListOptional -> ( "abstract" | "final" | "public" )*
    * unmodifiedClassDeclaration -> UnmodifiedClassDeclaration()
    * </PRE>
    */
   public void visit(ClassDeclaration n) {
      n.nodeListOptional.accept(this);
      n.unmodifiedClassDeclaration.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "class"
    * nodeToken1 -> &lt;IDENTIFIER&gt;
    * nodeOptional -> [ "extends" Name() ]
    * nodeOptional1 -> [ "implements" NameList() ]
    * classBody -> ClassBody()
    * </PRE>
    */
   public void visit(UnmodifiedClassDeclaration n) {
      n.nodeToken.accept(this);
      n.nodeToken1.accept(this);
      n.nodeOptional.accept(this);
      n.nodeOptional1.accept(this);
      n.classBody.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "{"
    * nodeListOptional -> ( ClassBodyDeclaration() )*
    * nodeToken1 -> "}"
    * </PRE>
    */
   public void visit(ClassBody n) {
      n.nodeToken.accept(this);
      n.nodeListOptional.accept(this);
      n.nodeToken1.accept(this);
   }

   /**
    * <PRE>
    * nodeListOptional -> ( "static" | "abstract" | "final" | "public" | "protected" | "private" )*
    * unmodifiedClassDeclaration -> UnmodifiedClassDeclaration()
    * </PRE>
    */
   public void visit(NestedClassDeclaration n) {
      n.nodeListOptional.accept(this);
      n.unmodifiedClassDeclaration.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> Initializer()
    *       | NestedClassDeclaration()
    *       | NestedInterfaceDeclaration()
    *       | ConstructorDeclaration()
    *       | MethodDeclaration()
    *       | FieldDeclaration()
    * </PRE>
    */
   public void visit(ClassBodyDeclaration n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeListOptional -> ( "public" | "protected" | "private" | "static" | "abstract" | "final" | "native" | "synchronized" )*
    * resultType -> ResultType()
    * nodeToken -> &lt;IDENTIFIER&gt;
    * nodeToken1 -> "("
    * </PRE>
    */
   public void visit(MethodDeclarationLookahead n) {
      n.nodeListOptional.accept(this);
      n.resultType.accept(this);
      n.nodeToken.accept(this);
      n.nodeToken1.accept(this);
   }

   /**
    * <PRE>
    * nodeListOptional -> ( "abstract" | "public" )*
    * unmodifiedInterfaceDeclaration -> UnmodifiedInterfaceDeclaration()
    * </PRE>
    */
   public void visit(InterfaceDeclaration n) {
      n.nodeListOptional.accept(this);
      n.unmodifiedInterfaceDeclaration.accept(this);
   }

   /**
    * <PRE>
    * nodeListOptional -> ( "static" | "abstract" | "final" | "public" | "protected" | "private" )*
    * unmodifiedInterfaceDeclaration -> UnmodifiedInterfaceDeclaration()
    * </PRE>
    */
   public void visit(NestedInterfaceDeclaration n) {
      n.nodeListOptional.accept(this);
      n.unmodifiedInterfaceDeclaration.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "interface"
    * nodeToken1 -> &lt;IDENTIFIER&gt;
    * nodeOptional -> [ "extends" NameList() ]
    * nodeToken2 -> "{"
    * nodeListOptional -> ( InterfaceMemberDeclaration() )*
    * nodeToken3 -> "}"
    * </PRE>
    */
   public void visit(UnmodifiedInterfaceDeclaration n) {
      n.nodeToken.accept(this);
      n.nodeToken1.accept(this);
      n.nodeOptional.accept(this);
      n.nodeToken2.accept(this);
      n.nodeListOptional.accept(this);
      n.nodeToken3.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> NestedClassDeclaration()
    *       | NestedInterfaceDeclaration()
    *       | MethodDeclaration()
    *       | FieldDeclaration()
    * </PRE>
    */
   public void visit(InterfaceMemberDeclaration n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeListOptional -> ( "public" | "protected" | "private" | "static" | "final" | "transient" | "volatile" )*
    * type -> Type()
    * variableDeclarator -> VariableDeclarator()
    * nodeListOptional1 -> ( "," VariableDeclarator() )*
    * nodeToken -> ";"
    * </PRE>
    */
   public void visit(FieldDeclaration n) {
      n.nodeListOptional.accept(this);
      n.type.accept(this);
      n.variableDeclarator.accept(this);
      n.nodeListOptional1.accept(this);
      n.nodeToken.accept(this);
   }

   /**
    * <PRE>
    * variableDeclaratorId -> VariableDeclaratorId()
    * nodeOptional -> [ "=" VariableInitializer() ]
    * </PRE>
    */
   public void visit(VariableDeclarator n) {
      n.variableDeclaratorId.accept(this);
      n.nodeOptional.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> &lt;IDENTIFIER&gt;
    * nodeListOptional -> ( "[" "]" )*
    * </PRE>
    */
   public void visit(VariableDeclaratorId n) {
      n.nodeToken.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> ArrayInitializer()
    *       | Expression()
    * </PRE>
    */
   public void visit(VariableInitializer n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "{"
    * nodeOptional -> [ VariableInitializer() ( "," VariableInitializer() )* ]
    * nodeOptional1 -> [ "," ]
    * nodeToken1 -> "}"
    * </PRE>
    */
   public void visit(ArrayInitializer n) {
      n.nodeToken.accept(this);
      n.nodeOptional.accept(this);
      n.nodeOptional1.accept(this);
      n.nodeToken1.accept(this);
   }

   /**
    * <PRE>
    * nodeListOptional -> ( "public" | "protected" | "private" | "static" | "abstract" | "final" | "native" | "synchronized" )*
    * resultType -> ResultType()
    * methodDeclarator -> MethodDeclarator()
    * nodeOptional -> [ "throws" NameList() ]
    * nodeChoice -> ( Block() | ";" )
    * </PRE>
    */
   public void visit(MethodDeclaration n) {
      n.nodeListOptional.accept(this);
      n.resultType.accept(this);
      n.methodDeclarator.accept(this);
      n.nodeOptional.accept(this);
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> &lt;IDENTIFIER&gt;
    * formalParameters -> FormalParameters()
    * nodeListOptional -> ( "[" "]" )*
    * </PRE>
    */
   public void visit(MethodDeclarator n) {
      n.nodeToken.accept(this);
      n.formalParameters.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "("
    * nodeOptional -> [ FormalParameter() ( "," FormalParameter() )* ]
    * nodeToken1 -> ")"
    * </PRE>
    */
   public void visit(FormalParameters n) {
      n.nodeToken.accept(this);
      n.nodeOptional.accept(this);
      n.nodeToken1.accept(this);
   }

   /**
    * <PRE>
    * nodeOptional -> [ "final" ]
    * type -> Type()
    * variableDeclaratorId -> VariableDeclaratorId()
    * </PRE>
    */
   public void visit(FormalParameter n) {
      n.nodeOptional.accept(this);
      n.type.accept(this);
      n.variableDeclaratorId.accept(this);
   }

   /**
    * <PRE>
    * nodeOptional -> [ "public" | "protected" | "private" ]
    * nodeToken -> &lt;IDENTIFIER&gt;
    * formalParameters -> FormalParameters()
    * nodeOptional1 -> [ "throws" NameList() ]
    * nodeToken1 -> "{"
    * nodeOptional2 -> [ ExplicitConstructorInvocation() ]
    * nodeListOptional -> ( BlockStatement() )*
    * nodeToken2 -> "}"
    * </PRE>
    */
   public void visit(ConstructorDeclaration n) {
      n.nodeOptional.accept(this);
      n.nodeToken.accept(this);
      n.formalParameters.accept(this);
      n.nodeOptional1.accept(this);
      n.nodeToken1.accept(this);
      n.nodeOptional2.accept(this);
      n.nodeListOptional.accept(this);
      n.nodeToken2.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> "this" Arguments() ";"
    *       | [ PrimaryExpression() "." ] "super" Arguments() ";"
    * </PRE>
    */
   public void visit(ExplicitConstructorInvocation n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeOptional -> [ "static" ]
    * block -> Block()
    * </PRE>
    */
   public void visit(Initializer n) {
      n.nodeOptional.accept(this);
      n.block.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> ( PrimitiveType() | Name() )
    * nodeListOptional -> ( "[" "]" )*
    * </PRE>
    */
   public void visit(Type n) {
      n.nodeChoice.accept(this);
      n.nodeListOptional.accept(this);
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
   public void visit(PrimitiveType n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> "void"
    *       | Type()
    * </PRE>
    */
   public void visit(ResultType n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> &lt;IDENTIFIER&gt;
    * nodeListOptional -> ( "." &lt;IDENTIFIER&gt; )*
    * </PRE>
    */
   public void visit(Name n) {
      n.nodeToken.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * name -> Name()
    * nodeListOptional -> ( "," Name() )*
    * </PRE>
    */
   public void visit(NameList n) {
      n.name.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * conditionalExpression -> ConditionalExpression()
    * nodeOptional -> [ AssignmentOperator() Expression() ]
    * </PRE>
    */
   public void visit(Expression n) {
      n.conditionalExpression.accept(this);
      n.nodeOptional.accept(this);
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
   public void visit(AssignmentOperator n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * conditionalOrExpression -> ConditionalOrExpression()
    * nodeOptional -> [ "?" Expression() ":" ConditionalExpression() ]
    * </PRE>
    */
   public void visit(ConditionalExpression n) {
      n.conditionalOrExpression.accept(this);
      n.nodeOptional.accept(this);
   }

   /**
    * <PRE>
    * conditionalAndExpression -> ConditionalAndExpression()
    * nodeListOptional -> ( "||" ConditionalAndExpression() )*
    * </PRE>
    */
   public void visit(ConditionalOrExpression n) {
      n.conditionalAndExpression.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * inclusiveOrExpression -> InclusiveOrExpression()
    * nodeListOptional -> ( "&&" InclusiveOrExpression() )*
    * </PRE>
    */
   public void visit(ConditionalAndExpression n) {
      n.inclusiveOrExpression.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * exclusiveOrExpression -> ExclusiveOrExpression()
    * nodeListOptional -> ( "|" ExclusiveOrExpression() )*
    * </PRE>
    */
   public void visit(InclusiveOrExpression n) {
      n.exclusiveOrExpression.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * andExpression -> AndExpression()
    * nodeListOptional -> ( "^" AndExpression() )*
    * </PRE>
    */
   public void visit(ExclusiveOrExpression n) {
      n.andExpression.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * equalityExpression -> EqualityExpression()
    * nodeListOptional -> ( "&" EqualityExpression() )*
    * </PRE>
    */
   public void visit(AndExpression n) {
      n.equalityExpression.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * instanceOfExpression -> InstanceOfExpression()
    * nodeListOptional -> ( ( "==" | "!=" ) InstanceOfExpression() )*
    * </PRE>
    */
   public void visit(EqualityExpression n) {
      n.instanceOfExpression.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * relationalExpression -> RelationalExpression()
    * nodeOptional -> [ "instanceof" Type() ]
    * </PRE>
    */
   public void visit(InstanceOfExpression n) {
      n.relationalExpression.accept(this);
      n.nodeOptional.accept(this);
   }

   /**
    * <PRE>
    * shiftExpression -> ShiftExpression()
    * nodeListOptional -> ( ( "&lt;" | "&gt;" | "&lt;=" | "&gt;=" ) ShiftExpression() )*
    * </PRE>
    */
   public void visit(RelationalExpression n) {
      n.shiftExpression.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * additiveExpression -> AdditiveExpression()
    * nodeListOptional -> ( ( "&lt;&lt;" | "&gt;&gt;" | "&gt;&gt;&gt;" ) AdditiveExpression() )*
    * </PRE>
    */
   public void visit(ShiftExpression n) {
      n.additiveExpression.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * multiplicativeExpression -> MultiplicativeExpression()
    * nodeListOptional -> ( ( "+" | "-" ) MultiplicativeExpression() )*
    * </PRE>
    */
   public void visit(AdditiveExpression n) {
      n.multiplicativeExpression.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * unaryExpression -> UnaryExpression()
    * nodeListOptional -> ( ( "*" | "/" | "%" ) UnaryExpression() )*
    * </PRE>
    */
   public void visit(MultiplicativeExpression n) {
      n.unaryExpression.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> ( "+" | "-" ) UnaryExpression()
    *       | PreIncrementExpression()
    *       | PreDecrementExpression()
    *       | UnaryExpressionNotPlusMinus()
    * </PRE>
    */
   public void visit(UnaryExpression n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "++"
    * primaryExpression -> PrimaryExpression()
    * </PRE>
    */
   public void visit(PreIncrementExpression n) {
      n.nodeToken.accept(this);
      n.primaryExpression.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "--"
    * primaryExpression -> PrimaryExpression()
    * </PRE>
    */
   public void visit(PreDecrementExpression n) {
      n.nodeToken.accept(this);
      n.primaryExpression.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> ( "~" | "!" ) UnaryExpression()
    *       | CastExpression()
    *       | PostfixExpression()
    * </PRE>
    */
   public void visit(UnaryExpressionNotPlusMinus n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> "(" PrimitiveType()
    *       | "(" Name() "[" "]"
    *       | "(" Name() ")" ( "~" | "!" | "(" | &lt;IDENTIFIER&gt; | "this" | "super" | "new" | Literal() )
    * </PRE>
    */
   public void visit(CastLookahead n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * primaryExpression -> PrimaryExpression()
    * nodeOptional -> [ "++" | "--" ]
    * </PRE>
    */
   public void visit(PostfixExpression n) {
      n.primaryExpression.accept(this);
      n.nodeOptional.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> "(" Type() ")" UnaryExpression()
    *       | "(" Type() ")" UnaryExpressionNotPlusMinus()
    * </PRE>
    */
   public void visit(CastExpression n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * primaryPrefix -> PrimaryPrefix()
    * nodeListOptional -> ( PrimarySuffix() )*
    * </PRE>
    */
   public void visit(PrimaryExpression n) {
      n.primaryPrefix.accept(this);
      n.nodeListOptional.accept(this);
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
   public void visit(PrimaryPrefix n) {
      n.nodeChoice.accept(this);
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
   public void visit(PrimarySuffix n) {
      n.nodeChoice.accept(this);
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
   public void visit(Literal n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> "true"
    *       | "false"
    * </PRE>
    */
   public void visit(BooleanLiteral n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "null"
    * </PRE>
    */
   public void visit(NullLiteral n) {
      n.nodeToken.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "("
    * nodeOptional -> [ ArgumentList() ]
    * nodeToken1 -> ")"
    * </PRE>
    */
   public void visit(Arguments n) {
      n.nodeToken.accept(this);
      n.nodeOptional.accept(this);
      n.nodeToken1.accept(this);
   }

   /**
    * <PRE>
    * expression -> Expression()
    * nodeListOptional -> ( "," Expression() )*
    * </PRE>
    */
   public void visit(ArgumentList n) {
      n.expression.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> "new" PrimitiveType() ArrayDimsAndInits()
    *       | "new" Name() ( ArrayDimsAndInits() | Arguments() [ ClassBody() ] )
    * </PRE>
    */
   public void visit(AllocationExpression n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> ( "[" Expression() "]" )+ ( "[" "]" )*
    *       | ( "[" "]" )+ ArrayInitializer()
    * </PRE>
    */
   public void visit(ArrayDimsAndInits n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> LabeledStatement()
    *       | Block()
    *       | EmptyStatement()
    *       | StatementExpression() ";"
    *       | SwitchStatement()
    *       | IfStatement()
    *       | WhileStatement()
    *       | DoStatement()
    *       | ForStatement()
    *       | BreakStatement()
    *       | ContinueStatement()
    *       | ReturnStatement()
    *       | ThrowStatement()
    *       | SynchronizedStatement()
    *       | TryStatement()
    * </PRE>
    */
   public void visit(Statement n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> &lt;IDENTIFIER&gt;
    * nodeToken1 -> ":"
    * statement -> Statement()
    * </PRE>
    */
   public void visit(LabeledStatement n) {
      n.nodeToken.accept(this);
      n.nodeToken1.accept(this);
      n.statement.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "{"
    * nodeListOptional -> ( BlockStatement() )*
    * nodeToken1 -> "}"
    * </PRE>
    */
   public void visit(Block n) {
      n.nodeToken.accept(this);
      n.nodeListOptional.accept(this);
      n.nodeToken1.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> LocalVariableDeclaration() ";"
    *       | Statement()
    *       | UnmodifiedClassDeclaration()
    *       | UnmodifiedInterfaceDeclaration()
    * </PRE>
    */
   public void visit(BlockStatement n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeOptional -> [ "final" ]
    * type -> Type()
    * variableDeclarator -> VariableDeclarator()
    * nodeListOptional -> ( "," VariableDeclarator() )*
    * </PRE>
    */
   public void visit(LocalVariableDeclaration n) {
      n.nodeOptional.accept(this);
      n.type.accept(this);
      n.variableDeclarator.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> ";"
    * </PRE>
    */
   public void visit(EmptyStatement n) {
      n.nodeToken.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> PreIncrementExpression()
    *       | PreDecrementExpression()
    *       | PrimaryExpression() [ "++" | "--" | AssignmentOperator() Expression() ]
    * </PRE>
    */
   public void visit(StatementExpression n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "switch"
    * nodeToken1 -> "("
    * expression -> Expression()
    * nodeToken2 -> ")"
    * nodeToken3 -> "{"
    * nodeListOptional -> ( SwitchLabel() ( BlockStatement() )* )*
    * nodeToken4 -> "}"
    * </PRE>
    */
   public void visit(SwitchStatement n) {
      n.nodeToken.accept(this);
      n.nodeToken1.accept(this);
      n.expression.accept(this);
      n.nodeToken2.accept(this);
      n.nodeToken3.accept(this);
      n.nodeListOptional.accept(this);
      n.nodeToken4.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> "case" Expression() ":"
    *       | "default" ":"
    * </PRE>
    */
   public void visit(SwitchLabel n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "if"
    * nodeToken1 -> "("
    * expression -> Expression()
    * nodeToken2 -> ")"
    * statement -> Statement()
    * nodeOptional -> [ "else" Statement() ]
    * </PRE>
    */
   public void visit(IfStatement n) {
      n.nodeToken.accept(this);
      n.nodeToken1.accept(this);
      n.expression.accept(this);
      n.nodeToken2.accept(this);
      n.statement.accept(this);
      n.nodeOptional.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "while"
    * nodeToken1 -> "("
    * expression -> Expression()
    * nodeToken2 -> ")"
    * statement -> Statement()
    * </PRE>
    */
   public void visit(WhileStatement n) {
      n.nodeToken.accept(this);
      n.nodeToken1.accept(this);
      n.expression.accept(this);
      n.nodeToken2.accept(this);
      n.statement.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "do"
    * statement -> Statement()
    * nodeToken1 -> "while"
    * nodeToken2 -> "("
    * expression -> Expression()
    * nodeToken3 -> ")"
    * nodeToken4 -> ";"
    * </PRE>
    */
   public void visit(DoStatement n) {
      n.nodeToken.accept(this);
      n.statement.accept(this);
      n.nodeToken1.accept(this);
      n.nodeToken2.accept(this);
      n.expression.accept(this);
      n.nodeToken3.accept(this);
      n.nodeToken4.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "for"
    * nodeToken1 -> "("
    * nodeOptional -> [ ForInit() ]
    * nodeToken2 -> ";"
    * nodeOptional1 -> [ Expression() ]
    * nodeToken3 -> ";"
    * nodeOptional2 -> [ ForUpdate() ]
    * nodeToken4 -> ")"
    * statement -> Statement()
    * </PRE>
    */
   public void visit(ForStatement n) {
      n.nodeToken.accept(this);
      n.nodeToken1.accept(this);
      n.nodeOptional.accept(this);
      n.nodeToken2.accept(this);
      n.nodeOptional1.accept(this);
      n.nodeToken3.accept(this);
      n.nodeOptional2.accept(this);
      n.nodeToken4.accept(this);
      n.statement.accept(this);
   }

   /**
    * <PRE>
    * nodeChoice -> LocalVariableDeclaration()
    *       | StatementExpressionList()
    * </PRE>
    */
   public void visit(ForInit n) {
      n.nodeChoice.accept(this);
   }

   /**
    * <PRE>
    * statementExpression -> StatementExpression()
    * nodeListOptional -> ( "," StatementExpression() )*
    * </PRE>
    */
   public void visit(StatementExpressionList n) {
      n.statementExpression.accept(this);
      n.nodeListOptional.accept(this);
   }

   /**
    * <PRE>
    * statementExpressionList -> StatementExpressionList()
    * </PRE>
    */
   public void visit(ForUpdate n) {
      n.statementExpressionList.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "break"
    * nodeOptional -> [ &lt;IDENTIFIER&gt; ]
    * nodeToken1 -> ";"
    * </PRE>
    */
   public void visit(BreakStatement n) {
      n.nodeToken.accept(this);
      n.nodeOptional.accept(this);
      n.nodeToken1.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "continue"
    * nodeOptional -> [ &lt;IDENTIFIER&gt; ]
    * nodeToken1 -> ";"
    * </PRE>
    */
   public void visit(ContinueStatement n) {
      n.nodeToken.accept(this);
      n.nodeOptional.accept(this);
      n.nodeToken1.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "return"
    * nodeOptional -> [ Expression() ]
    * nodeToken1 -> ";"
    * </PRE>
    */
   public void visit(ReturnStatement n) {
      n.nodeToken.accept(this);
      n.nodeOptional.accept(this);
      n.nodeToken1.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "throw"
    * expression -> Expression()
    * nodeToken1 -> ";"
    * </PRE>
    */
   public void visit(ThrowStatement n) {
      n.nodeToken.accept(this);
      n.expression.accept(this);
      n.nodeToken1.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "synchronized"
    * nodeToken1 -> "("
    * expression -> Expression()
    * nodeToken2 -> ")"
    * block -> Block()
    * </PRE>
    */
   public void visit(SynchronizedStatement n) {
      n.nodeToken.accept(this);
      n.nodeToken1.accept(this);
      n.expression.accept(this);
      n.nodeToken2.accept(this);
      n.block.accept(this);
   }

   /**
    * <PRE>
    * nodeToken -> "try"
    * block -> Block()
    * nodeListOptional -> ( "catch" "(" FormalParameter() ")" Block() )*
    * nodeOptional -> [ "finally" Block() ]
    * </PRE>
    */
   public void visit(TryStatement n) {
      n.nodeToken.accept(this);
      n.block.accept(this);
      n.nodeListOptional.accept(this);
      n.nodeOptional.accept(this);
   }

}