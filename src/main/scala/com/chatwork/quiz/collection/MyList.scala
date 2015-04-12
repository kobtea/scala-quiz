package com.chatwork.quiz.collection

import com.chatwork.quiz.{ MySome, MyNone, MyOption }

sealed trait MyList[+A] {

  // Easy
  def length: Int = {
    def go(x: MyList[A])(y: Int): Int = x match {
      case MyNil              => y
      case MyCons(head, tail) => go(tail)(y + 1)
    }
    go(this)(0)
  }

  // Normal
  def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case MyNil              => z
    case MyCons(head, tail) => tail.foldLeft(f(z, head))(f)
  }

  // 難易度選択制
  // Normal: 条件 - 特にありません、気の向くままに実装してください。
  // Hard:   条件 - foldLeftを使って実装してください。
  def foldRight[B](z: B)(f: (A, B) => B): B = this.reverse.foldLeft(z)((a, b) => f(b, a))

  // Normal
  // scalastyle:off
  def ::[B >: A](b: B): MyList[B] = MyCons(b, this)

  // scalastyle:on

  // Normal
  def reverse: MyList[A] = {
    def go(x: MyList[A])(y: MyList[A]): MyList[A] = x match {
      case MyNil              => y
      case MyCons(head, tail) => go(tail)(MyCons(head, y))
    }
    go(this)(MyNil)
  }

  // Normal
  // scalastyle:off
  def ++[B >: A](b: MyList[B]): MyList[B] = {
    def go(x: MyList[A])(y: MyList[B]): MyList[B] = x match {
      case MyNil              => y
      case MyCons(head, tail) => go(tail)(MyCons(head, y))
    }
    go(this.reverse)(b)
  }

  // scalastyle:on

  // Normal
  def map[B](f: A => B): MyList[B] = {
    def go(x: MyList[A])(y: MyList[B]): MyList[B] = x match {
      case MyNil              => y
      case MyCons(head, tail) => go(tail)(MyCons(f(head), y))
    }
    go(this.reverse)(MyNil)
  }

  // Normal
  def flatMap[B](f: A => MyList[B]): MyList[B] = this.map(f).foldRight(MyNil: MyList[B])(_ ++ _)

  // Normal
  def filter(f: A => Boolean): MyList[A] = {
    def go(x: MyList[A])(y: MyList[A]): MyList[A] = x match {
      case MyNil => y
      case MyCons(head, tail) => f(head) match {
        case true  => go(tail)(MyCons(head, y))
        case false => go(tail)(y)
      }
    }
    go(this.reverse)(MyNil)
  }

  // Normal: 条件 - filterと同様の実装でも構いません。
  // Hard:   条件 - 中間リストを生成しないように実装してください。
  def withFilter(f: A => Boolean): MyList[A] = this.filter(f)

  // Normal
  def find(f: A => Boolean): MyOption[A] = this match {
    case MyNil => MyNone
    case MyCons(head, tail) => f(head) match {
      case true  => MySome(head)
      case false => tail.find(f)
    }
  }

  // Normal
  def startsWith[B >: A](prefix: MyList[B]): Boolean = ???

}

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  // Easy
  def empty[A]: MyList[A] = MyNil

  // Normal
  def apply[A](as: A*): MyList[A] = {
    def go(as: A*)(l: MyList[A] = MyNil): MyList[A] = as.toList match {
      case Nil          => l.reverse
      case head :: tail => go(tail: _*)(MyCons(head, l))
    }
    go(as: _*)(MyNil)
  }

}
