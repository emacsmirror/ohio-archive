//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeToken -> "{"
 * nodeOptional -> [ VariableInitializer() ( "," VariableInitializer() )* ]
 * nodeOptional1 -> [ "," ]
 * nodeToken1 -> "}"
 * </PRE>
 */
public class ArrayInitializer implements Node {
   private Node parent;
   public NodeToken nodeToken;
   public NodeOptional nodeOptional;
   public NodeOptional nodeOptional1;
   public NodeToken nodeToken1;

   public ArrayInitializer(NodeToken n0, NodeOptional n1, NodeOptional n2, NodeToken n3) {
      nodeToken = n0;
      if ( nodeToken != null ) nodeToken.setParent(this);
      nodeOptional = n1;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      nodeOptional1 = n2;
      if ( nodeOptional1 != null ) nodeOptional1.setParent(this);
      nodeToken1 = n3;
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
   }

   public ArrayInitializer(NodeOptional n0, NodeOptional n1) {
      nodeToken = new NodeToken("{");
      if ( nodeToken != null ) nodeToken.setParent(this);
      nodeOptional = n0;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      nodeOptional1 = n1;
      if ( nodeOptional1 != null ) nodeOptional1.setParent(this);
      nodeToken1 = new NodeToken("}");
      if ( nodeToken1 != null ) nodeToken1.setParent(this);
   }

   public void accept(jde.parser.visitor.Visitor v) {
      v.visit(this);
   }
   public Object accept(jde.parser.visitor.ObjectVisitor v, Object argu) {
      return v.visit(this,argu);
   }
   public void setParent(Node n) { parent = n; }
   public Node getParent()       { return parent; }
}
