package RTEC.Data

import RTEC._

final case class InputEntity(name: String, numOfArgs: Int, sources: Iterable[Data.EventId]) {
    // Use the patterns to calculate every possible entity given the input database
    def instances(data: Execute.EventDB): (String, Iterable[Seq[String]]) = {
        val s: Set[Seq[String]] = sources.flatMap(data.getEntities(_))(collection.breakOut)

        (name, s)
    }
}


final case class BuiltEntity(name: String, numOfArgs: Int, sources: Iterable[Seq[(Option[String], Seq[String])]]) {
    def instances(data: Map[String, Iterable[Seq[String]]]): (String, Iterable[Seq[String]]) = {
        val s: Set[Seq[String]] = sources
            .flatMap {_
                .map {
                    // Constants
                    case (None, elements) =>
                        elements map (Seq(_))

                    // whole entity
                    case (Some(id), List()) =>
                        data(id)

                    // one entity element
                    case (Some(id), List(index)) =>
                        val i = index.toInt
                        val d = data(id)
                        d map {instance =>
                            Seq(instance.apply(i))
                        }

                    // entity slice
                    case (Some(id), List(from, until)) =>
                        val f = from.toInt
                        val u = until.toInt
                        val d = data(id)
                        d map (_.slice(f, u))
                }
                .reduce {(x, y) =>
                    for (x1 <- x; y1 <- y)
                        yield x1 ++ y1
                }
            }(collection.breakOut)

        (name, s)
    }
}
