package example

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import CardsExercise._

class CardsExerciseSpec extends AnyWordSpec with Matchers {
  "getHighCard" should {
    "return the highest card from a hand of 5 cards" in {
      CardsExercise.getHighCard(
        Seq(
          Card(1, Clubs),
          Card(2, Clubs),
          Card(3, Clubs),
          Card(4, Clubs),
          Card(5, Clubs)
        )) shouldBe Card(5, Clubs)
    }
  }
  "hasPairs" should {
    "return if there is a pair in the provided hand" when {
      "there is a pair" in {
        CardsExercise.hasPairs(
          Seq(
            Card(1, Clubs),
            Card(1, Hearts),
            Card(3, Clubs),
            Card(4, Clubs),
            Card(5, Clubs)
          )) shouldBe true
      }
      "there are multiple pairs" in {
        CardsExercise.hasPairs(
          Seq(
            Card(1, Clubs),
            Card(1, Hearts),
            Card(3, Clubs),
            Card(3, Hearts),
            Card(5, Clubs)
          )) shouldBe true
      }
      "there is a three of a kind" in {
        CardsExercise.hasPairs(
          Seq(
            Card(1, Clubs),
            Card(1, Hearts),
            Card(1, Diamonds),
            Card(4, Clubs),
            Card(5, Clubs)
          )) shouldBe true
      }
      "there is a four of a kind" in {
        CardsExercise.hasPairs(
          Seq(
            Card(1, Clubs),
            Card(1, Hearts),
            Card(1, Diamonds),
            Card(1, Spades),
            Card(5, Clubs)
          )) shouldBe true
      }
      "there are no pairs" in {
        CardsExercise.hasPairs(
          Seq(
            Card(1, Clubs),
            Card(2, Clubs),
            Card(3, Clubs),
            Card(4, Clubs),
            Card(5, Clubs)
          )) shouldBe false
      }
    }
  }
  "getGroups" should {
    "return nothing if there are no groups" in {
      CardsExercise.getGroups(
        Seq(
          Card(1, Clubs),
          Card(2, Clubs),
          Card(3, Clubs),
          Card(4, Clubs),
          Card(5, Clubs)
        )) shouldBe empty
    }
    "return a single pair if there is only one" in {
      CardsExercise.getGroups(
        Seq(
          Card(1, Clubs),
          Card(1, Hearts),
          Card(3, Clubs),
          Card(4, Clubs),
          Card(5, Clubs)
        )) shouldBe Seq(
        Pair(Card(1, Clubs), Card(1, Hearts))
      )
    }
    "return a three of a kind if there is only one" in {
      CardsExercise.getGroups(
        Seq(
          Card(1, Clubs),
          Card(1, Hearts),
          Card(1, Diamonds),
          Card(4, Clubs),
          Card(5, Clubs)
        )) shouldBe Seq(
        ThreeOfAKind(Card(1, Clubs), Card(1, Hearts), Card(1, Diamonds))
      )
    }
    "return a four of a kind if there is only one" in {
      CardsExercise.getGroups(
        Seq(
          Card(1, Clubs),
          Card(1, Hearts),
          Card(1, Diamonds),
          Card(1, Spades),
          Card(5, Clubs)
        )) shouldBe Seq(
        FourOfAKind(Card(1, Clubs), Card(1, Hearts), Card(1, Diamonds), Card(1, Spades))
      )
    }
    "return two pairs if there are two" in {
      CardsExercise.getGroups(
        Seq(
          Card(1, Clubs),
          Card(1, Hearts),
          Card(2, Clubs),
          Card(2, Hearts),
          Card(5, Clubs)
        )) shouldBe Seq(
        Pair(Card(1, Clubs), Card(1, Hearts)),
        Pair(Card(2, Clubs), Card(2, Hearts))
      )
    }
    "return a pair and a three of a kind if they are both present" in {
      CardsExercise.getGroups(
        Seq(
          Card(1, Clubs),
          Card(1, Hearts),
          Card(2, Clubs),
          Card(2, Hearts),
          Card(2, Diamonds)
        )) shouldBe Seq(
        Pair(Card(1, Clubs), Card(1, Hearts)),
        ThreeOfAKind(Card(2, Clubs), Card(2, Hearts), Card(2, Diamonds))
      )
    }
  }
  "hasStraight" should {
    "return whether a hand contains a straight" when {
      "there is a straight" in {
        CardsExercise.hasStraight(
          Seq(
            Card(1, Clubs),
            Card(2, Clubs),
            Card(3, Clubs),
            Card(4, Clubs),
            Card(5, Clubs)
          )) shouldBe true
      }
      "there is no straight" in {
        CardsExercise.hasStraight(
          Seq(
            Card(1, Clubs),
            Card(2, Clubs),
            Card(3, Clubs),
            Card(4, Clubs),
            Card(6, Clubs)
          )) shouldBe false
      }
    }
  }
}
