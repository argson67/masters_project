package yascc.settings

import scala.language.implicitConversions
import scala.collection.mutable.{ Map => MMap }

import yascc.Yascc

trait Settings {
  self: Yascc =>

    private val settings: MMap[String, String] = MMap.empty

    def getSetting(name: String): Option[String] = settings.get(name)
    def setSetting(name: String, value: String): Unit = settings(name) = value

    implicit def setting2Int(opt: Option[String]): Int = 
      opt match {
        case Some(str) => Integer.parseInt(str)
        case None => -1
      }

    implicit def setting2Bool(opt: Option[String]): Boolean =
      opt match {
        case Some(str) if str.toLowerCase == "true" => true
        case _ => false
      }

    implicit def setting2Str(opt: Option[String]): String =
      opt getOrElse ""

    def getSettingT[T](name: String)(implicit conv: Option[String] => T): T = 
      conv(settings.get(name))

    // TODO: defaults
    //setSetting("")

    setSetting("parserName", "SampleParser")
    setSetting("packrat", "false")
    setSetting("positioned", "false")
    setSetting("packageName", "mypackage")
    setSetting("textWidth", "80")
}
