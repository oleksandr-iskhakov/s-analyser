package com.garallex.stocks

import java.time.LocalDateTime

import scala.collection.mutable


object Main {
  def grahamPreFiltered(stock: Stock): Option[Boolean] =
    (stock.enterpriseValue,
      stock.currentRatio,
      stock.totalCurrentAssets,
      stock.longTermDebt,
      stock.peRatio,
      stock.priceToBook) match {
      case (Some(ev), Some(cr), Some(tca), Some(ltd), Some(per), Some(ptb)) => Some(
        ev >= 2000000000 &&
          cr >= 2 &&
          tca >= ltd &&
          per <= 15 &&
          ptb <= 1.5)
      case _ => None
    }

  def main(args: Array[String]): Unit = {
    val a = Map(1 -> "A", 2 -> "B", 3 -> "C")
    val b = Map(1 -> "D", 2 -> "B")
    val x = a.updated(4, "D")
    //println(LocalDateTime.of(2017, 3, 21, 0, 0).plusMinutes(65))

    while (true) {
      val ticker = scala.io.StdIn.readLine("Ticker:")
      if (ticker == null || ticker == "") return
      println(new StockBuilder(ticker.toUpperCase).build())
    }

    //    val doc = new edu.stanford.nlp.simple.Document("I live with a Vietnam Vet who served in the late 1960s with 1st Cav. Medivac. During service he earned two Purple Hearts, the Distinguished Flying Cross, and the Air Medal. Since WE WERE SOLDIERS concerns the 1st Cav., Randy wanted to see it. I reluctantly agreed; I am not partial to war films and I dislike Mel Gibson, and Randy is very hard on Vietnam War films. He dismisses PLATOON as a Hollywood 8x10 glossy; says APOCALYPSE NOW is an interesting movie that captures the paranoia, but all the technical details are wrong; and describes DEER HUNTER as excellent in its depiction of the strangeness of coming home but so full of plot holes that he can hardly endure it. And about one and all he says: \"It wasn't like that.\"\n\nHe was silent through the film, and when we left the theatre I asked what he thought. He said, \"They finally got it. That's what it was like. All the details are right. The actors were just like the men I knew. They looked like that and they talked like that. And the army wives too, they really were like that, at least every one I ever knew.\" The he was silent for a long time. At last he said, \"You remember the scene where the guy tries to pick up a burn victim by the legs and all the skin slides off? Something like that happened to me once. It was at a helicopter crash. I went to pick him up and all the skin just slid right off. It looked just like that, too. I've never told any one about it.\" In most respects WE WERE SOLDIERS is a war movie plain and simple. There are several moments when the film relates the war to the politics and social movements that swirled about it, and the near destruction of the 1st. Cav.'s 7th Battalion at Ia Drang clearly arises from the top brass' foolish decision to send the 7th into an obvious ambush--but the film is not so much interested in what was going on at home or at the army's top as it is in what was actually occurring on the ground. And in this it is extremely meticulous, detailed, and often horrifically successful. Neither Randy nor I--nor any one in the theatre I could see--was bored by or dismissive of the film. It grabs you and it grabs you hard, and I can easily say that it is one of the finest war movies I have ever seen, far superior to the likes of SAVING PRIVATE RYAN, which seems quite tame in comparison.\n\nPerhaps the single most impressive thing about the film is that it never casts its characters in a heroic light; they are simply soldiers who have been sent to do a job, and they do it knowing the risks, and they do it well in spite of the odds. Mel Gibson, although I generally despise him as both an actor and a human being, is very, very good as commanding officer Hal Moore, and he is equaled by Sam Elliot, Greg Kinnear, Chris Klein, and every other actor on the battlefield. The supporting female cast, seen early in the film and in shorter scenes showing the home front as the battle rages, is also particularly fine, with Julie Moore able to convey in glance what most actresses could not communicate in five pages of dialogue. The script, direction, cinematography, and special effects are sharp, fast, and possess a \"you are there\" quality that is very powerful.\n\nI myself had a criticism; there were points in the film when I found the use of a very modernistic, new-agey piece of music to be intrusive and out of place. And we both felt that a scene near the end of the movie, when a Vietnamese commander comments on the battle, to be improbable and faintly absurd. But these are nit-picky quibbles. WE WERE SOLDIERS is a damn fine movie. I'll give Randy, who served two tours of duty in Vietnam, the last word: \"It may not be 'the' Vietnam movie. I don't think there could ever be 'the' Vietnam movie. But they pretty much get everything right. That's how it looked and sounded, and that's what I saw, and this is the best movie about Vietnam I've ever seen.\" Gary F. Taylor, aka GFT, Amazon Reviewer")
    //    val sentences = doc.sentences()
    //    import scala.collection.JavaConverters._
    //
    //    sentences.asScala.foreach { sentence => println(sentence.sentiment()) }

    //    val stock = new StockBuilder("MCK").build()
    //    println(stock)
  }
}
