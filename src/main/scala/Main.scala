object Main {

  implicit def eqEminem: Equal[Eminem] = (a: Eminem, b: Eminem) =>
    a.spaghetti == b.spaghetti

  implicit def eqDuck: Equal[Duck] = (a: Duck, b: Duck) =>
    a.name == b.name && a.legs == b.legs

  implicit def eqList[A](implicit e: Equal[A]): Equal[List[A]] = new Equal[List[A]] {
    @scala.annotation.tailrec
    override def eq(a: List[A], b: List[A]): Boolean = (a, b) match {
      case (Nil, Nil) => true
      case (_, Nil) => false
      case (Nil, _) => false
      case (x :: xs, y :: ys) => e.eq(x, y) && eq(xs, ys)
    }
  }

  def eq[A](a: A, b: A)(implicit e: Equal[A]): Unit = {
    println(e.eq(a, b))
  }

  @scala.annotation.tailrec
  def search[A](element: A, list: List[A])(implicit e: Equal[A]): Boolean =
    list match {
      case Nil => false
      case x :: xs => e.eq(element, x) || search(element, xs)
    }

  def matching[A](xs: List[A], ys: List[A])(implicit e: Equal[List[A]]): Boolean = e.eq(xs, ys)

  def main(args: Array[String]): Unit = {
    val d0 = Duck("Lmao", 2)
    val d1 = Duck("Kek", 2)
    val d2 = Duck("Lmao", 2)

    println("--------------------- Simple eq 1")
    println(eq(d0, d1))
    println(eq(d1, d2))
    println(eq(d0, d2))

    println("--------------------- Simple eq 2")
    println(eq(Eminem(10), Eminem(25)))
    println(eq(Eminem(10), Eminem(10)))

    println("--------------------- Search")
    println(search(Eminem(-1), List(Eminem(10), Eminem(32), Eminem(47))))
    println(search(Eminem(32), List(Eminem(10), Eminem(32), Eminem(47))))

    println("--------------------- Higher order implciit")
    println(matching(List(Eminem(5), Eminem(10)), List(Eminem(5), Eminem(10))))
    println(matching(List(Eminem(0)), List()))
  }
}
