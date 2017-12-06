package RTEC.Execute.Reader

import RTEC._

object Main {

    type InputHappensAt = (Data.InstantEventId, Seq[String], Int)
    type InputHoldsAt = (Data.FluentId, Seq[String], Int)
    type InputHoldsFor = (Data.FluentId, Seq[String], Data.Intervals)

    type CachingTarget = (Data.EventId, String)

    private var _iEs: Option[Set[Data.InstantEvent]] = None
    private var _fluents: Option[Set[Data.Fluent]] = None
    private var _predicates: Option[Map[Data.EventId, Seq[Data.Predicate]]] = None
    private var _entities: Option[(Set[Data.InputEntity], Seq[Data.BuiltEntity])] = None
    private var _cachingOrder: Option[Seq[CachingTarget]] = None

    private var _realTime: Option[(Seq[InputHappensAt], Seq[InputHoldsAt], Seq[InputHoldsFor])] = None

    def staticData: Option[(Set[Data.InstantEvent],
                     Set[Data.Fluent],
                     (Map[Data.EventId, Seq[Data.Predicate]]),
                     (Set[Data.InputEntity], Seq[Data.BuiltEntity]),
                     Seq[CachingTarget])] = {

        (_iEs, _fluents, _predicates, _entities, _cachingOrder) match {
            case (Some(iEs), Some(fluents), Some(predicates), Some(entities), Some(cachingOrder)) =>
                Some((iEs, fluents, predicates, entities, cachingOrder))
            case _ => None
        }

    }
    def realTime: Option[(Seq[InputHappensAt], Seq[InputHoldsAt], Seq[InputHoldsFor])] = _realTime

    def readDeclarations(file: String): Unit = {
        val source = scala.io.Source.fromFile(file).mkString
        ParseDeclarations.get(source) match {
            case Some((iEs, fluents, entities, cachingOrd)) =>
                _iEs = Some(iEs)
                _fluents = Some(fluents)
                _cachingOrder = Some(cachingOrd)
                _entities = Some(entities)
        }
    }

    def readDefinitions(file: String): Unit = {
        val source = io.Source.fromFile(file).mkString
        _predicates = ParseDefinitions.get(source, _fluents.get)

    }

    def readDataset(file: String): Unit = {
        val source = io.Source.fromFile(file).mkString
        _realTime = ParseRealtimeInput.get(source)
    }
}