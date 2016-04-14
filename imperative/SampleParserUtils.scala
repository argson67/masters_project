package imperative

trait SampleParserUtils extends SampleParserTrees with SampleParserTreesImpl {

  def strToInt: (
  String) => Num
  
  def leftAssocAdd: (
  Seq[Expr]) => Expr
  
  def leftAssocMultiply: (
  Seq[Expr]) => Expr
}