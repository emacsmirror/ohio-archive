//
// Generated by JTB 1.2.1
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * nodeOptional -> [ PackageDeclaration() ]
 * nodeListOptional -> ( ImportDeclaration() )*
 * nodeListOptional1 -> ( TypeDeclaration() )*
 * nodeToken -> &lt;EOF&gt;
 * </PRE>
 */
public class CompilationUnit implements Node {
   private Node parent;
   public NodeOptional nodeOptional;
   public NodeListOptional nodeListOptional;
   public NodeListOptional nodeListOptional1;
   public NodeToken nodeToken;

   public CompilationUnit(NodeOptional n0, NodeListOptional n1, NodeListOptional n2, NodeToken n3) {
      nodeOptional = n0;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      nodeListOptional = n1;
      if ( nodeListOptional != null ) nodeListOptional.setParent(this);
      nodeListOptional1 = n2;
      if ( nodeListOptional1 != null ) nodeListOptional1.setParent(this);
      nodeToken = n3;
      if ( nodeToken != null ) nodeToken.setParent(this);
   }

   public CompilationUnit(NodeOptional n0, NodeListOptional n1, NodeListOptional n2) {
      nodeOptional = n0;
      if ( nodeOptional != null ) nodeOptional.setParent(this);
      nodeListOptional = n1;
      if ( nodeListOptional != null ) nodeListOptional.setParent(this);
      nodeListOptional1 = n2;
      if ( nodeListOptional1 != null ) nodeListOptional1.setParent(this);
      nodeToken = new NodeToken("");
      if ( nodeToken != null ) nodeToken.setParent(this);
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

