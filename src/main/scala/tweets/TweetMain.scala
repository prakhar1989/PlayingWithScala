package org.playwithscala.tweets

import TweetReader._

object SomeRandomObject {
  def main(args: Array[String]) {
    val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
    val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

    lazy val alltweetSet: TweetSet = TweetReader.allTweets // returns a tweetset
    lazy val googleTweets: TweetSet = alltweetSet.filter(tw => google.exists(e => tw.text.contains(e)))
    lazy val appleTweets: TweetSet = alltweetSet.filter(tw => apple.exists(e => tw.text.contains(e)))

    lazy val trending: TweetList = (googleTweets.union(appleTweets)).descendingByRetweet
    println(trending.head)
  }
  
}

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String = text + "[" + retweets + "]"
}

abstract class TweetSet {
  def isEmpty: Boolean
  def incl(tweet: Tweet): TweetSet
  def contains(tweet: Tweet): Boolean
  def foreach(f: Tweet => Unit): Unit // function with a side-effect
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet
  def filter(p: Tweet => Boolean): TweetSet = this.filterAcc(p, new Empty)  
  def union(that: TweetSet): TweetSet
  def mostRetweeted: Tweet
  def remove(tweet: Tweet): TweetSet
  def descendingByRetweet: TweetList
}

class Empty extends TweetSet {
  override def toString = "."
  def isEmpty: Boolean = true
  def contains(tweet: Tweet): Boolean = false
  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)
  def foreach(f: Tweet => Unit): Unit = ()
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc
  def union(that: TweetSet): TweetSet = that
  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException
  def remove(tweet: Tweet): TweetSet = this
  def descendingByRetweet: TweetList = Nil // singleTon
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
  override def toString = "{" + left + elem + right + "}" 

  def isEmpty: Boolean = false

  def contains(x: Tweet) : Boolean = 
    if (x.text < elem.text) left.contains(x)
    else if (x.text > elem.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = 
    if (x.text < elem.text) new NonEmpty(elem, left incl x, right)
    else if (x.text > elem.text) new NonEmpty(elem, left, right incl x)
    else this 
  
  def remove(x: Tweet): TweetSet = 
    if (x.text < elem.text) new NonEmpty(elem, left remove x, right)
    else if (x.text > elem.text) new NonEmpty(elem, left, right remove x)
    else left.union(right) // elem == tweet, left union right

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = 
    // if p is true for a tweet, acc incl x
    if (p(elem)) left.filterAcc(p, right.filterAcc(p, acc incl elem))
    else left.filterAcc(p, right.filterAcc(p, acc))

  def union(that: TweetSet): TweetSet = 
    // make sure you understand this
    (left union (right union that)).incl(elem)
  
  def mostRetweeted: Tweet = {
    lazy val l = left.mostRetweeted
    lazy val r = right.mostRetweeted
    
    if (!left.isEmpty && l.retweets > elem.retweets)
      if (!right.isEmpty && r.retweets > l.retweets) r
      else l
    else if (!right.isEmpty && r.retweets > elem.retweets) r
    else elem
  }

  def descendingByRetweet: TweetList = {
    // no need of 'this' in below
    new Cons(mostRetweeted, remove(mostRetweeted).descendingByRetweet)
  }
    
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head.nil")
  def tail = throw new java.util.NoSuchElementException("tail.nil")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}
