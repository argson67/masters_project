package yascc.output

trait Names {
  self: Targets =>

    val parserName: String = getSettingT("parserName")
    val treesName: String = parserName + "Trees"
    val treesImplName: String = treesName + "Impl"
    val utilsName: String = parserName + "Utils"
    val utilsImplName: String = parserName + "UtilsImpl"

    val parsersTrait: String = "Parsers"
    val packratTrait: String = "PackratParsers"

    val packageName: String = getSettingT("packageName")

    val textWidth: Int = getSettingT("textWidth")
}
