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
  case class Straight(cardA: Card, cardB: Card, cardC: Card, cardD: Card, cardE: Card) extends Hand {
    val isFlush: Boolean = Iterable(cardA, cardB, cardC, cardD, cardE).groupBy(_.suit).exists(_._2.size == 5)
  }

  def getHighCard(hand: Seq[Card]): Card =
    hand.maxBy(_.value)

  def hasPairs(hand: Seq[Card]): Boolean = {
    val pairs = for {
      cardA <- hand
      cardB <- hand
      if cardA.value == cardB.value && cardA != cardB
    } yield (cardA, cardB)

    pairs.nonEmpty
  }

  def getGroups(hand: Seq[Card]): Seq[Hand] =
    hand
      .groupBy(_.value)
      .collect {
        case (_, hand) if hand.size == 2 =>
          Pair(hand.head, hand(1))
        case (_, hand) if hand.size == 3 =>
          ThreeOfAKind(hand.head, hand(1), hand(2))
        case (_, hand) if hand.size == 4 =>
          FourOfAKind(hand.head, hand(1), hand(2), hand(3))
      }
      .toSeq

  def getScoredGroup(hand: Seq[Card]): Option[Hand] = {
    val groups = getGroups(hand)
    val pairs = groups.collect { case pair: Pair                             => pair }
    val threeOfAKind = groups.collectFirst { case threeOfAKind: ThreeOfAKind => threeOfAKind }
    val fourOfAKind = groups.collectFirst { case fourOfAKind: FourOfAKind    => fourOfAKind }

    (pairs.headOption, pairs.lift(1), threeOfAKind, fourOfAKind) match {
      case (Some(pairA), Some(pairB), _, _) =>
        Some(TwoPairs(pairA, pairB))
      case (Some(pair), _, Some(threeOfAKind), _) =>
        Some(FullHouse(pair, threeOfAKind))
      case (_, _, Some(threeOfAKind: ThreeOfAKind), _) =>
        Some(threeOfAKind)
      case (_, _, _, Some(fourOfAKind: FourOfAKind)) =>
        Some(fourOfAKind)
      case (Some(singlePair), _, _, _) =>
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

    if (straight.size == 4) {
      Some(
        Straight(
          lowestCard,
          straight(0),
          straight(1),
          straight(2),
          straight(3)
        ))
    } else None
  }

  def getFlush(hand: Seq[Card]): Option[Hand] =
    if (hand.groupBy(_.suit).exists(_._2.size == 5))
      Some(Flush(hand.head, hand(1), hand(1), hand(2), hand(3)))
    else None

  def getHighestScoringHand(hand: Seq[Card]): Hand = {
    val optStraight = getStraight(hand)
    val optFlush = getFlush(hand)
    val optGroup = getScoredGroup(hand)
    val highCard = HighCard(getHighCard(hand))

    (optStraight ++ optGroup ++ optFlush ++ Some(highCard)).maxBy {
      case straight: Straight if straight.isFlush =>
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
