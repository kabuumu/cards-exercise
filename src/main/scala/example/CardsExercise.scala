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
  case class HighCard(highCard: Card) extends Hand
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

  def hasStraight(hand: Seq[Card]): Boolean = {
    val lowestCard = hand.minBy(_.value)

    val straight = for {
      increment    <- 1 to 4
      matchingCard <- hand.find(_.value == lowestCard.value + increment)
    } yield matchingCard

    straight.size == 4
  }

}
