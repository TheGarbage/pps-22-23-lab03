package Task

import org.junit.Assert.assertEquals
import org.junit.Test
import Task.List.*
import Task.Option.*
import Task.Person.*
import Task.Stream.*

class TaskTest:

  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  val tail = Cons(40, Nil())
  val lstF = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  val s = take(iterate(0)(_ + 1))(10)

  // Task 1a
  @Test def dropListTest() =
    assertEquals(Cons(20, Cons(30, Nil())), Task.List.drop(lst, 1))
    assertEquals(Cons(30, Nil()), Task.List.drop(lst, 2))
    assertEquals(Nil(), Task.List.drop(lst, 5))

  // Task 1b
  @Test def appendTest() =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(lst, tail))

  // Task 1c
  @Test def flatMapTest() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil ()))), flatMap(lst)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11 ,Cons(12, Cons(21 , Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  // Task 1d
  @Test def filterTest() =
    assertEquals(Cons(20, Cons(30, Nil())), Task.List.filter(lst)(_ >= 20))

  @Test def mapTest() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), Task.List.map(lst)(_ + 1))

  // Task 2
  @Test def maxTest() =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

  // Task 3
  @Test def coursesExtractionTest() =
    assertEquals(Cons("Informatica", Cons("Elettrotecnica", Cons("Biomedica", Nil()))),
      coursesExtraction(Cons(Teacher("Caio", "Informatica"), Cons(Teacher("io", "Elettrotecnica"),
                        Cons(Student("Marco", 1), Cons(Teacher("Julia", "Biomedica"), Nil()))))))

  // Task 4
  @Test def foldTest() =
    assertEquals(-16, foldLeft(lstF)(0)(_ - _))
    assertEquals(-8, foldRight(lstF)(0)(_ - _))

  // Task 5
  @Test def dropStreamTest() =
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), toList(Task.Stream.drop(s)(6)))

  // Task 6
  @Test def constantTest() =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil ()))))), Task.Stream.toList(take(constant("x"))(5)))

  // Task 7
  @Test def fibsTest() =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))), Task.Stream.toList(take(fibs)(8)))