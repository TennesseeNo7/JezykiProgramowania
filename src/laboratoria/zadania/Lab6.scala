package laboratoria.zadania

import scala.annotation.tailrec

object Lab6 extends App {

  //Zadanie 1
  def applyForAll[A,B](seq: Seq[A], f: A => B): Seq[B] = {
    @tailrec
    def aux(seq: Seq[A], f: A => B, out: Seq[B] = Seq()): Seq[B] = seq match {
      case e +: s => aux(s, f, f(e) +: out)
      case Seq() => out.reverse
    }
    aux(seq, f)
  }

  //Zadanie 2
  def applyForAll2[A, B, C](seq1: Seq[A], seq2: Seq[B], f: (A, B) => C): Seq[C] = {
    @tailrec
    def aux(seq1: Seq[A], seq2: Seq[B], f: (A, B) => C, out: Seq[C] = Seq()): Seq[C] = (seq1, seq2) match {
      case (e1 +: s1, e2 +: s2) => aux(s1, s2, f, f(e1, e2) +: out)
      case _ => out.reverse
    }
    aux(seq1, seq2, f)
  }

  //Zadanie 3
  def divide[A](seq: Seq[A]): (Seq[A], Seq[A]) = {
    @tailrec
    def aux(seq: Seq[A], out: (Seq[A], Seq[A]) = (Seq(), Seq())): (Seq[A], Seq[A]) = seq match {
      case e1 +: e2 +: s => aux(s, (e1 +: out._1, e2 +: out._2))
      case Seq(e) => ((e +: out._1).reverse, out._2.reverse)
      case Seq() => (out._1.reverse, out._2.reverse)
    }
    aux(seq)
  }

  //Zadanie 4
  def compute[A,B](seq: Seq[A])(init: B)(op: (A, B) => B): B = {
    @tailrec
    def aux (seq: Seq[A], out: B ): B = seq match {
        case e +: r => aux(r, op(e, out))
        case Seq() => out
      }
    aux (seq, init)
  }

  //Zadnie 5

  //a.
  def size[A](a: Seq[A]): Int = compute(a)(0)((_,n: Int)=> n+1)

  //b.
  def reverse[A](a: Seq[A]): Seq[A] = compute(a)(Seq(): Seq[A])((e: A, out: Seq[A]) => e +: out)

  //c.
  def applyforall[A, B](a: Seq[A], f: A => B): Seq[B] = compute(a)(Seq(): Seq[B])((a: A, out: Seq[B]) => f(a) +: out).reverse

  //d.
  def filter[A](a: Seq[A], pred: A=>Boolean): Seq[A] = compute(a)(Seq(): Seq[A])((a: A, out: Seq[A]) =>
    if(pred(a)) a +: out else out).reverse

  //e.
  def forall[A](a: Seq[A], pred: A=>Boolean): Boolean = compute(a)(true)((a: A, out: Boolean) =>
    if (pred(a)) out else false)



}
