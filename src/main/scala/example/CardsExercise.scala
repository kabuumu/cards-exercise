package example

object CardsExercise {
  sealed trait Suit
  case object Clubs extends Suit
  case object Diamonds extends Suit
  case object Hearts extends Suit
  case object Spades extends Suit

  case class Card(value: Int, suit: Suit)

  sealed trait Hand
  case class Pair(cardA: Card, cardB: Card) extends Hand
  case class ThreeOfAKind(cardA: Card, cardB: Card, cardC: Card) extends Hand
  case class FourOfAKind(cardA: Card, cardB: Card, cardC: Card, cardD: Card) extends Hand
  case class TwoPairs(pairA: Pair, pairB: Pair) extends Hand
  case class FullHouse(pair: Pair, threeOfAKind: ThreeOfAKind) extends Hand
  case class HighCard(highCard: Card) extends Hand
  case class Flush(cardA: Card, cardB: Card, cardC: Card, cardD: Card, cardE: Card) extends Hand
  case class Straight(cardA: Card, cardB: Card, cardC: Card, cardD: Card, cardE: Card) extends Hand

  def getHighCard(hand: Seq[Card]): Card = hand.maxBy(_.value)

  def hasPairs(hand: Seq[Card]): Boolean = getGroups(hand).nonEmpty

  def getGroups(hand: Seq[Card]): Seq[Hand] =
    hand
      .groupBy(_.value)
      .values
      .collect {
        case cardA :: cardB :: cardC :: cardD :: Nil =>
          FourOfAKind(cardA, cardB, cardC, cardD)
        case cardA :: cardB :: cardC :: Nil =>
          ThreeOfAKind(cardA, cardB, cardC)
        case cardA :: cardB :: Nil =>
          Pair(cardA, cardB)
      }
      .toSeq

  def getScoredGroup(hand: Seq[Card]): Option[Hand] = {
    val groups = getGroups(hand)
    val pairs = groups.collect { case pair: Pair                             => pair }
    val threeOfAKind = groups.collectFirst { case threeOfAKind: ThreeOfAKind => threeOfAKind }
    val fourOfAKind = groups.collectFirst { case fourOfAKind: FourOfAKind    => fourOfAKind }

    (pairs, threeOfAKind, fourOfAKind) match {
      case (pairA :: pairB :: Nil, _, _) =>
        Some(TwoPairs(pairA, pairB))
      case (pair :: Nil, Some(threeOfAKind), _) =>
        Some(FullHouse(pair, threeOfAKind))
      case (_, Some(threeOfAKind: ThreeOfAKind), _) =>
        Some(threeOfAKind)
      case (_, _, Some(fourOfAKind: FourOfAKind)) =>
        Some(fourOfAKind)
      case (singlePair :: Nil, _, _) =>
        Some(singlePair)
      case _ =>
        None
    }
  }

  def hasStraight(hand: Seq[Card]): Boolean =
    getStraight(hand).isDefined

  def getStraight(hand: Seq[Card]): Option[Straight] = {
    val lowestCard = hand.minBy(_.value)

    val straight = for {
      increment    <- 1 to 4
      matchingCard <- hand.find(_.value == lowestCard.value + increment)
    } yield matchingCard

    straight.toList match {
      case cardA :: cardB :: cardC :: cardD :: Nil =>
        Some(Straight(lowestCard, cardA, cardB, cardC, cardD))
      case _ =>
        None
    }
  }

  def getFlush(hand: Seq[Card]): Option[Hand] =
    hand.groupBy(_.suit) collectFirst {
      case (_, cardA :: cardB :: cardC :: cardD :: cardE :: Nil) =>
        Flush(cardA, cardB, cardC, cardD, cardE)
    }

  def getHighestScoringHand(hand: Seq[Card]): Hand = {
    val optStraight = getStraight(hand)
    val optFlush = getFlush(hand)
    val optGroup = getScoredGroup(hand)
    val highCard = HighCard(getHighCard(hand))

    (optStraight ++ optGroup ++ optFlush ++ Some(highCard)).maxBy {
      case _: Straight if optFlush.isDefined =>
        8
      case _: FourOfAKind =>
        7
      case _: FullHouse =>
        6
      case _: Flush =>
        5
      case _: Straight =>
        4
      case _: ThreeOfAKind =>
        3
      case _: TwoPairs =>
        2
      case _: Pair =>
        1
      case _: HighCard =>
        0
    }
  }

}
