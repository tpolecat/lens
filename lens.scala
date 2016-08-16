
object lens1 {

  case class Album(name: String, band: Band)
  case class Band(name: String, lead: Person)
  case class Person(first: String, last: String)

  val a = Album("Dark Side of the Spoon", Band("Pink Lloyd", Person("Steve",  "Gilmour")))

  a.copy(name = "Dark Side of the Moon")
  a.copy(band = a.band.copy(name = "Pink Floyd"))
  a.copy(band = a.band.copy(lead = a.band.lead.copy(first = "David")))

}

object lens2 {

  case class Album(name: String, band: Band)
  case class Band(name: String, lead: Person)
  case class Person(first: String, last: String)

  val a = Album("Dark Side of the Spoon", Band("Pink Lloyd", Person("Steve",  "Gilmour")))
    
  case class Lens[A,B](set: (A, B) => A, get: A => B)

  val albumName = Lens[Album, String]((a, b) => a.copy(name = b), _.name)

  albumName.set(a, "Dark Side of the Moon")

}

object lens3 {

  case class Album(name: String, band: Band)
  case class Band(name: String, lead: Person)
  case class Person(first: String, last: String)

  val a = Album("Dark Side of the Spoon", Band("Pink Lloyd", Person("Steve",  "Gilmour")))

  case class Lens[A,B](set: (A, B) => A, get: A => B) { outer =>
    def andThen[C](lens: Lens[B, C]): Lens[A, C] =
      Lens((a, c) => outer.set(a, lens.set(outer.get(a), c)), a => lens.get(outer.get(a)))
  }

  val albumBand = Lens[Album, Band]((a, b) => a.copy(band = b), _.band)
  val bandName  = Lens[Band, String]((a, b) => a.copy(name = b), _.name)

  (albumBand andThen bandName).set(a, "Pink Floyd")

  val bandLead = Lens[Band, Person]((a, b) => a.copy(lead = b), _.lead)

  val personFirst = Lens[Person, String]((a, b) => a.copy(first = b), _.first)

  (albumBand andThen bandLead andThen personFirst).set(a, "David")


}


object lens4 {

  case class Lens[A,B](set: (A, B) => A, get: A => B) { outer =>
    def >=>[C](lens: Lens[B, C]): Lens[A, C] =
      Lens((a, c) => outer.set(a, lens.set(outer.get(a), c)), a => lens.get(outer.get(a)))
  }

  type @>[A, B] = Lens[A, B]

  case class Album(name: String, band: Band)
  object Album {
    val name: Album @> String = Lens((a, b) => a.copy(name = b), _.name)
    val band: Album @> Band   = Lens((a, b) => a.copy(band = b), _.band)
  }

  case class Band(name: String, lead: Person)
  object Band {
    val name: Band @> String = Lens((a, b) => a.copy(name = b), _.name)
    val lead: Band @> Person = Lens((a, b) => a.copy(lead = b), _.lead)
  }

  case class Person(first: String, last: String)
  object Person {
    val first: Person @> String = Lens((a, b) => a.copy(first = b), _.first)
    val last:  Person @> String = Lens((a, b) => a.copy(last  = b), _.last )
  }



  val a = Album("Dark Side of the Spoon", Band("Pink Lloyd", Person("Steve",  "Gilmour")))


  (Album.band >=> Band.name).set(a, "Pink Floyd")

  (Album.band >=> Band.lead >=> Person.first).set(a, "David")


}




object lens5 extends App {

  case class Lens[A,B](set: (A, B) => A, get: A => B) { outer =>

    def >=>[C](lens: Lens[B, C]): Lens[A, C] =
      Lens((a, c) => outer.set(a, lens.set(outer.get(a), c)), a => lens.get(outer.get(a)))

    def modify(a: A, f: B => B): A =
      set(a, f(get(a)))

  }

  type @>[A, B] = Lens[A, B]

  case class Album(name: String, band: Band)
  object Album {
    val name: Album @> String = Lens((a, b) => a.copy(name = b), _.name)
    val band: Album @> Band   = Lens((a, b) => a.copy(band = b), _.band)
  }

  case class Band(name: String, lead: Person)
  object Band {
    val name: Band @> String = Lens((a, b) => a.copy(name = b), _.name)
    val lead: Band @> Person = Lens((a, b) => a.copy(lead = b), _.lead)
  }

  case class Person(first: String, last: String)
  object Person {
    val first: Person @> String = Lens((a, b) => a.copy(first = b), _.first)
    val last:  Person @> String = Lens((a, b) => a.copy(last  = b), _.last )
  }



  val a = Album("Dark Side of the Spoon", Band("Pink Lloyd", Person("Steve",  "Gilmour")))


  (Album.band >=> Band.name).set(a, "Pink Floyd")

  println((Album.band >=> Band.lead >=> Person.first).modify(a, s => s.toUpperCase))


}


object lens5_5 extends App {

  case class Lens[A,B](set: (A, B) => A, get: A => B) { outer =>

    def >=>[C](lens: Lens[B, C]): Lens[A, C] =
      Lens((a, c) => outer.set(a, lens.set(outer.get(a), c)), a => lens.get(outer.get(a)))

    def modify(a: A, f: B => B): A =
      set(a, f(get(a)))

    def zip[C](lens: Lens[A, C]): Lens[A, (B, C)] =
      Lens((a, bc) => lens.set(outer.set(a, bc._1), bc._2), a => (outer.get(a), lens.get(a)))

  }

  type @>[A, B] = Lens[A, B]

  // trivial
  def id[A]: A @> A = Lens((a, b) => b, a => a)
  def trivial[A]: A @> Unit = Lens((a, b) => a, _ => ())

  // projection
  def fst[A,B]: (A, B) @> A = Lens((a, b) => (b, a._2), _._1)
  def snd[A,B]: (A, B) @> B = Lens((a, b) => (a._1, b), _._2)

  // unexpected
  def includes[A](a: A): Set[A] @> Boolean =
    Lens((s, b) => if (b) s + a else s - a, _.contains(a))

  def mapping[K,V](k: K): Map[K, V] @> Option[V] =
    Lens((m, ov) => ov.fold(m - k)(v => m + (k -> v)), _.get(k))


  val s = Set(1,2,3)

  println(includes(5).set(s, true))
  println(includes(2).set(s, false))


}



object lens6 extends App {

  case class State[S, A](run: S => (S, A)) {

    def map[B](f: A => B): State[S, B] =
      State { s =>
        val (s0, a) = run(s)
        (s0, f(a))
      }

    def flatMap[B](f: A => State[S,B]): State[S, B] =
      State { s =>
        val (s0, a) = run(s)
        f(a).run(s0)
      }

    def exec(s: S): S =
      run(s)._1

  }

  case class Lens[A,B](set: (A, B) => A, get: A => B) { outer =>

    def >=>[C](lens: Lens[B, C]): Lens[A, C] =
      Lens((a, c) => outer.set(a, lens.set(outer.get(a), c)), a => lens.get(outer.get(a)))

    def modify(a: A, f: B => B): A =
      set(a, f(get(a)))

    def :=(b: B): State[A, Unit] =
      State(a => (set(a, b), ()))

    def ask: State[A, B] =
      State(a => (a, get(a)))

  }

  type @>[A, B] = Lens[A, B]

  case class Album(name: String, band: Band)
  object Album {
    val name: Album @> String = Lens((a, b) => a.copy(name = b), _.name)
    val band: Album @> Band   = Lens((a, b) => a.copy(band = b), _.band)
  }

  case class Band(name: String, lead: Person)
  object Band {
    val name: Band @> String = Lens((a, b) => a.copy(name = b), _.name)
    val lead: Band @> Person = Lens((a, b) => a.copy(lead = b), _.lead)
  }

  case class Person(first: String, last: String)
  object Person {
    val first: Person @> String = Lens((a, b) => a.copy(first = b), _.first)
    val last:  Person @> String = Lens((a, b) => a.copy(last  = b), _.last )
  }

  val albumName = Album.name
  val bandName  = Album.band >=> Band.name
  val leadFirst = Album.band >=> Band.lead >=> Person.first

  def update =
    for {
      _ <- albumName := "Dark Side of the Moon"
      _ <- bandName  := "Pink Floyd"
      n <- leadFirst.ask
      _ <- leadFirst := n.toUpperCase
    } yield ()

  val a = Album("Dark Side of the Spoon", Band("Pink Lloyd", Person("Steve",  "Gilmour")))

  println(update.exec(a))

}



