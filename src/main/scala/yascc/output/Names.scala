package yascc.output

trait Names {
  self: Targets =>

    def parserName: String = getSettingT("parserName")
    def treesName: String = parserName + "Trees"
    def treesImplName: String = treesName + "Impl"
    def utilsName: String = parserName + "Utils"
    def utilsImplName: String = parserName + "UtilsImpl"

    def parsersTrait: String = "Parsers"
    def packratTrait: String = "PackratParsers"

    def packageName: String = getSettingT("packageName")

    def textWidth: Int = getSettingT("textWidth")
}
