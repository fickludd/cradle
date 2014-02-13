package se.jt.frame

import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer
import scala.Array.canBuildFrom

object KvimTree {

	def main(args: Array[String]) = {
		val str = """a
 b1
 b2
  c1
   d1
   d2
  c2
   d3
 b3
  c3
   d4
    e1
"""

		val tt = KvimTree.fromString(str, "\n", ' ')
		println(tt)

		val rootless = """a1
 b1
 b2
  c1
  c2
a2
 b3
  c3
   d1
    e1
"""

		val tt2 = KvimTree.fromString(rootless, "\n", ' ')
		println(tt2)
	}
	
	
	def apply[K, V](obj: V, children: Map[K, Node[K,V]] = Nil.toMap) =
		new Node(obj, children)

	def fromString(str: String, sep: String, prefix: Char) = {
		val nodes = str.split(sep)
		val prefixedNodes = nodes.map(w => {
			val l = w.prefixLength(_ == prefix)
			(l, w.drop(l))
		})

		case class PreNode(val name: String) {
			var children = new ArrayBuffer[PreNode]
		}

		val stack = new Stack[PreNode]()
		val minPrefix = prefixedNodes.map(_._1).min
		if (prefixedNodes.head._1 != minPrefix)
			throw new Exception("String does not start with top level node")

		if (prefixedNodes.filter(_._1 == minPrefix).length > 1)
			stack.push(PreNode("root"))

		def fold = {
			val pn = stack.pop
			stack.top.children += pn
		}
		val stackStart = stack.length
		for ((p, n) <- prefixedNodes) {
			var i = p
			while (i <= stack.length - stackStart - 1)
				fold

			stack.push(PreNode(n))
		}

		while (stack.length > 1)
			fold

		def toNode(pn: PreNode): Node[String, String] =
			Node(pn.name, pn.children map (
					pn => pn.name -> toNode(pn)
				) toMap)

		toNode(stack.pop)
	}

	
	
	case class Node[K, V](
		val obj: V,
		val children: Map[K, KvimTreeNode[K, V]]
	) extends KvimTreeNode[K, V] {
	  def kvimTreeNode[U](u:U, childs:Map[K, KvimTreeNode[K, U]]) = Node(u, childs)
	}
	
}


trait KvimTreeNode[K, V] {
  
	def kvimTreeNode[U](u:U, children:Map[K, KvimTreeNode[K, U]]): KvimTreeNode[K, U]
	def obj: V
	def children:Map[K, KvimTreeNode[K, V]]

	override def toString = { treeString(" ") }
	def treeString(prefix: String = ""): String =
		prefix + "N(" + obj + ")\n" + 
			(children map {
				case (k, n) => k + " -> " + n.treeString(prefix + " ")
			}).mkString
			 

	def get(path: List[K]): Option[KvimTreeNode[K, V]] =
		path match {
			case Nil => Some(this)
			case k :: ks =>
				if (children contains k)
					children(k).get(ks)
				else None
		}

	def exists(path: List[K]): Boolean =
		path match {
			case Nil => true
			case k :: ks =>
				if (children contains k)
					children(k).exists(ks)
				else false
		}

	def realizeTree(path: List[K], n: KvimTreeNode[K, V]): KvimTreeNode[K, V] =
		path.foldRight(n)(
				(k, n) => kvimTreeNode(n.obj, Map(k -> n))
			)

	def updated(path: List[K], n: KvimTreeNode[K, V], insert: Boolean = false): Option[KvimTreeNode[K, V]] =
		path match {
			case Nil => return Some(n)
				
			case k :: Nil =>
				if (children contains k)
					Some(replace(k, n))
				else
					Some(kvimTreeNode(obj, children + (k -> n)))
					
			case k :: ks =>
				if (children contains k)
					children(k).updated(ks, n, insert) match {
						case Some(subTree) =>
							Some(replace(k, subTree))
						case None => None
					}
				
				else
					if (insert)
						Some(kvimTreeNode(obj, Map(k -> realizeTree(ks, n))))
					else
						None
				
		}

	def foreach(f: (V => Unit)): Unit = {
		f(obj)
		for (child <- children.values) child.foreach(f)
	}

	def foreachLeaf(f: (V => Unit)): Unit = {
		if (children.isEmpty) f(obj)
		else
			for (child <- children.values) child.foreachLeaf(f)
	}

	def selectiveLeaf(isLeaf: V => Boolean, f: (V => Unit)): Unit = {
		if (children.isEmpty || isLeaf(obj))
			f(obj)
		else
			for (child <- children.values) child.foreachLeaf(f)
	}

	def collect[R](pf: PartialFunction[V, R]): Iterable[R] =
		if (pf.isDefinedAt(obj))
			List(pf(obj))
		else
			children.values.flatMap(_.collect(pf))

	def map[U](f: V => U): KvimTreeNode[K, U] =
		kvimTreeNode(f(obj), children map { case (k, n) => k -> n.map(f) })
	

	lazy val height: Int = {
		if (children.nonEmpty)
			children.values.map(_.height).max + 1
		else 0
	}

	lazy val width: Int = {
		if (children.nonEmpty)
			children.values.map(_.width).sum
		else 1
	}

	private def replace(k: K, n: KvimTreeNode[K, V]) = {
		if (children contains k)
			kvimTreeNode(obj, children - k + (k -> n))
		else
			this
	}
}