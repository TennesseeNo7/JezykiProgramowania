package src.exercises.laboratories

object Lab4 extends App {

  //Zadanie 1
  def compose[A,B,C](f: A => B)(g: B => C): A => C = { x: A => g(f(x)) }

  def prod[A,B,C,D](f: A => C, g: B => D): (A, B) => (C, D) = { (x: A, y: B) => (f(x), g(y)) }

  def lift[A,B,T](op: (T,T) => T)(f: A => T, g: B => T): (A,B) => T = { (x: A, y: B) => op(f(x), g(y)) }

  //Zadanie 2
  type Pred[A] = A => Boolean

  def and[A](p: Pred[A], q: Pred[A]): Pred[A] = { a: A => p(a) && q(a) }

  def or[A](p: Pred[A], q: Pred[A]): Pred[A] = { a: A => p(a) || q(a) }

  def not[A](p: Pred[A]): Pred[A] = { a: A => !p(a) }

  def imp[A](p: Pred[A], q: Pred[A]): Pred[A] = { a: A => !(p(a) && !q(a)) }

  //Zadanie 3
  type MSet[A] = A => Int

  def +[A](a: MSet[A], b: MSet[A]): MSet[A] = (e: A) => a(e) + b(e)
  def -[A](a: MSet[A], b: MSet[A]): MSet[A] = (e: A) => if(a(e) - b(e) <= 0) 0 else a(e)-b(e)
  def &[A](a: MSet[A], b: MSet[A]): MSet[A] = (e: A) => if(a(e) > b(e)) b(e) else a(e)

  //Zadanie 4
  def size[A](a: Seq[A]): Int = {
    a match {
      case _ +: a2 => 1 + size(a2)
      case _ if a.isEmpty  => 0
    }
  }

  def sizeRec[A](a: Seq[A]): Int = {
    def sizeRecAux(a: Seq[A], size: Int = 0): Int = a match{
      case Seq() => size
      case _ +: s => sizeRecAux(s, size + 1)
    }
    sizeRecAux(a)
  }

  //Zadanie 5
  def forall[A](a: Seq[A])(pred: A => Boolean): Boolean = a match {
    case Seq() => true
    case e +: _ if !pred(e) => false
    case _ +: s => forall(s)(pred)
  }

  //Zadanie 6
  def merge[A](a: Seq[A], b: Seq[A])(leq: (A, A) => Boolean): Seq[A] = {
    def mergeAux(a: Seq[A], b: Seq[A], mer: Seq[A]): Seq[A] = {
      (a, b) match {
        case _ if a.isEmpty || b.isEmpty => mer.reverse ++ a ++ b
        case (e1 +: s1, e2 +: _) if leq(e1, e2) => mergeAux(s1, b, e1 +: mer)
        case (_, e2 +: s2 ) => mergeAux(a, s2, e2 +: mer)
      }
    }
    mergeAux(a, b, Seq())
  }

}
