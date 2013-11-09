package yascc.settings

import scala.collection.mutable.{ Map => MMap }

import yascc.Yascc

trait Settings {
  self: Yascc =>

    private val settings: MMap[String, String] = MMap.empty

    def getSetting(name: String): Option[String] = settings.get(name)
    def setSetting(name: String, value: String): Unit = settings(name) = value
}
