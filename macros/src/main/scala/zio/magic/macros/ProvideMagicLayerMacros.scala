package zio.magic.macros

import zio._
import zio.magic.macros.ExprGraph.LayerExpr

import scala.reflect.macros.blackbox

class ProvideMagicLayerMacros(val c: blackbox.Context) extends MacroUtils {
  import c.universe._

  def provideMagicLayerImpl[
      R: c.WeakTypeTag,
      E: c.WeakTypeTag,
      A
  ](
      layers: c.Expr[ZLayer[_, E, _]]*
  )(
      dummyK: c.Expr[DummyK[R]]
  ): c.Expr[ZIO[Any, E, A]] = {
    assertProperVarArgs(layers)
    val nodes = layers.map(getNode).toList
    val _     = ExprGraph(nodes, c).buildLayerFor(getRequirements[R])

    val renamedNodes = nodes.zipWithIndex.map { case (node, i) =>
      val freshName = c.freshName("layer")
      val termName  = TermName(freshName)
      node.copy(value = c.Expr[ZLayer[_, _, _]](q"${termName}"))
    }

    val layerExpr = ExprGraph(renamedNodes, c).buildLayerFor(getRequirements[R])
    val definitions = renamedNodes.zip(nodes).map { case (renamedNode, node) =>
      ValDef(Modifiers(), TermName(renamedNode.value.tree.toString()), TypeTree(), node.value.tree)
    }

    c.Expr(q"""
    ..$definitions
    ${c.prefix}.zio.provideLayer(${layerExpr.tree.asInstanceOf[c.Tree]})
    """)
  }

  def provideCustomMagicLayerImpl[
      R: c.WeakTypeTag,
      E: c.WeakTypeTag,
      A
  ](
      layers: c.Expr[ZLayer[_, E, _]]*
  )(
      dummyK: c.Expr[DummyK[R]]
  ): c.Expr[ZIO[ZEnv, E, A]] = {
    assertProperVarArgs(layers)
    val ZEnvRequirements = getRequirements[ZEnv]
    val requirements     = getRequirements[R] diff ZEnvRequirements

    val zEnvAny   = reify { ZEnv.any }
    val zEnvLayer = Node(List.empty, ZEnvRequirements, zEnvAny)
    val nodes     = (zEnvLayer +: layers.map(getNode)).toList

    val layerExpr = ExprGraph(nodes, c).buildLayerFor(requirements)
    c.Expr(q"${c.prefix}.zio.provideCustomLayer(${layerExpr.tree.asInstanceOf[c.Tree]})")
  }
}
